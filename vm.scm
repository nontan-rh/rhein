;;
;; Rhein VM bootstrap
;;

(use srfi-1)
(use srfi-43)
(use util.match)
(use gauche.record)

(require "./rhein-prelude")

(define-record-type rhein-environment #t #t
  (function-name) (functions) (function-count)
  (variable-name) (variables) (variable-count))

(define (null-rhein-environment)
  (make-rhein-environment
    (make-hash-table 'string=?)
    (make-hash-table 'eq?) 0
    (make-hash-table 'string=?)
    (make-hash-table 'eq?) 0))

(define (add-function env name body)
  (let1 function-id (~ env 'function-count)
    (hash-table-put! (~ env 'function-name) name function-id)
    (hash-table-put! (~ env 'functions) function-id body)
    (inc! (~ env 'function-count))))

(define (primary-rhein-environment)
  (rlet1 env (null-rhein-environment)
    (add-function env "print" rhein-print)))

(define-record-type rhein-function #t #t
  (name) (regc) (varc) (func) (argc) (code) (static-bindings))

(define (compile-all env asm)
  (for-each (^x (reserve-rhein-object env x)) asm)
  (for-each (^x (install-rhein-object env x)) asm))

(define (reserve-rhein-object env obj)
  (case (car obj)
    [(function) (reserve-rhein-function env obj)]))

(define (reserve-rhein-function env func)
  (match func
    [('function name (regc varc func argc) code)
     (let1 function-id (~ env 'function-count)
       (inc! (~ env 'function-count))
       (hash-table-put! (~ env 'function-name) name function-id)
       (hash-table-put! (~ env 'functions) function-id
                        (make-rhein-function name regc varc func argc (undefined) '() )))]))

(define (install-rhein-object env obj)
  (case (car obj)
    [(function) (install-rhein-function env obj)]))

(define (install-rhein-function env func)
  (match func
    [('function name _ _)
     (let1 function-id (hash-table-get (~ env 'function-name) name)
       (hash-table-put! (~ env 'functions) function-id
                        (compile-func env func)))]))

(define (compile-func env func)
  (match func
    [('function name (regc varc func argc) code)
     (make-rhein-function name regc varc func argc (compile-code env code) '() )]))

(define (compile-code env code)
  (receive [tag-list stripped] (correct-tags env code)
    (let1 out (make-vector (length stripped))
      (do ([cnt 0 (+ 1 cnt)]
           [insns stripped (cdr insns)])
        [(null? insns) out]
        (let1 insn (car insns)
          (vector-set! out cnt
                       (case (first insn)
                         ([gcall]
                          (match insn [(code func dst arg0 argc)
                                       (list code
                                             (hash-table-get (~ env 'function-name) func)
                                             dst arg0 argc)]))
                         ([jump]
                          (match insn [(code target)
                                       (list code (hash-table-get tag-list target))]))
                         ([if-jump nif-jump]
                          (match insn [(code jcond target)
                                       (list code jcond (hash-table-get tag-list target))]))
                         (else insn))))))))

(define (correct-tags env code)
  (let1 tag-list (make-hash-table 'eq?)
    (let lp ([cnt 0] [insns code])
      (cond
        [(null? insns) (values)]
        [(number? (car insns))
         (hash-table-put! tag-list (car insns) cnt)
         (lp cnt (cdr insns))]
        [else (lp (+ cnt 1) (cdr insns))]))
    (values tag-list (filter list? code))))

;;
;; Run it
;;

(define-record-type call-frame #t #t
  (function) (program-counter)
  (registers) (return-register)
  (static-bindings) (dynamic-bindings))

(define (run-function-with-name env name)
  (run-function-with-id env (hash-table-get (~ env 'function-name) name)))

(define (run-function-with-id env id)
  (let* ([entry-function (hash-table-get (~ env 'functions) id)]
         [current-frame (make-call-frame entry-function 0
                                         (make-vector (~ entry-function 'regc) 0) 0
                                         (list (make-vector (~ entry-function 'varc)))
                                         '())]
         [call-stack '()])
    ;(display (~ entry-function 'code)) (newline)
    ;(display (~ (hash-table-get (~ env 'functions) 2) 'code)) (newline)
    (let-syntax ([register-ref (syntax-rules ()
                                 [(_ regno)
                                  (vector-ref (~ current-frame 'registers) regno)])]
                 [register-set! (syntax-rules ()
                                  [(_ regno value)
                                   (vector-set! (~ current-frame 'registers) regno value)])])
      (let/cc br
        (while #t
          (match (vector-ref (~ current-frame 'function 'code)
                             (~ current-frame 'program-counter))
            [('move dst src) (register-set! dst (register-ref src))] 
            [('undef dst) (register-set! dst (undefined))]
            [('gvref dst src) (register-set! dst (hash-table-get (~ env 'variables) src))]
            [('lvref dst src-layer src-offset)
             (register-set! dst (vector-ref (list-ref (~ current-frame 'static-bindings) src-layer)
                                            src-offset))]
            [('gvset dst src)
             (hash-table-put! (~ env 'variables) dst (register-ref src))]
            [('lvset dst-layer dst-offset src)
             (vector-set! (list-ref (~ current-frame 'static-bindings) dst-layer) dst-offset
                          (register-ref src))]
            [('jump dst)
             (set! (~ current-frame 'program-counter) (- dst 1))]
            [('if-jump con dst)
             (when (register-ref con)
               (set! (~ current-frame 'program-counter) (- dst 1)))]
            [('nif-jump con dst)
             (unless (register-ref con)
               (set! (~ current-frame 'program-counter) (- dst 1)))]
            [('gcall func-id dst arg0 argc)
             (let1 new-func (hash-table-get (~ env 'functions) func-id)
               (if (procedure? new-func)
                 (register-set! dst (apply new-func
                                           (vector->list
                                             (~ current-frame 'registers) arg0 (+ arg0 argc))))
                 (let ([new-frame (make-call-frame (hash-table-get (~ env 'functions) func-id) -1
                                                   (make-vector (~ new-func 'regc)) dst
                                                   (~ new-func 'static-bindings)
                                                   (~ current-frame 'dynamic-bindings))]
                       [new-static-layer (vector-append
                                           (vector-copy (~ current-frame 'registers)
                                                        arg0 (+ arg0 argc))
                                           (make-vector (- (~ new-func 'varc) argc)))])
                   (push! (~ new-frame 'static-bindings) new-static-layer)
                   (push! call-stack current-frame)
                   (set! current-frame new-frame))))]
            [('ret return-value)
             (when (null? call-stack)
               (br (register-ref return-value)))
             (let1 old-frame current-frame
               (set! current-frame (pop! call-stack))
               (register-set! (~ old-frame 'return-register)
                              (vector-ref (~ old-frame 'registers) return-value)))]
            [('load-int dst value) (register-set! dst value)]
            [('bin-is dst src1 src2)
             (register-set! dst (eqv? (register-ref src1) (register-ref src2)))]
            [('bin-isnot dst src1 src2)
             (register-set! dst (not (eqv? (register-ref src1) (register-ref src2))))]
            [('bin-gt dst src1 src2)
             (register-set! dst (> (register-ref src1) (register-ref src2)))]
            [('bin-lt dst src1 src2)
             (register-set! dst (< (register-ref src1) (register-ref src2)))]
            [('bin-ge dst src1 src2)
             (register-set! dst (>= (register-ref src1) (register-ref src2)))]
            [('bin-le dst src1 src2)
             (register-set! dst (<= (register-ref src1) (register-ref src2)))]
            [('bin-add dst src1 src2)
             (register-set! dst (+ (register-ref src1) (register-ref src2)))]
            [('bin-sub dst src1 src2)
             (register-set! dst (- (register-ref src1) (register-ref src2)))]
            [('bin-mul dst src1 src2)
             (register-set! dst (* (register-ref src1) (register-ref src2)))]
            [('bin-div dst src1 src2)
             (register-set! dst (quotient (register-ref src1) (register-ref src2)))]
            [('uni-neg dst src) (register-set! dst (- (register-ref src1)))]
            [('uni-plus dst src) (register-set! dst (register-ref src1))]
            [('uni-not dst src) (register-set! dst (not (register-ref src1)))])
          (inc! (~ current-frame 'program-counter)))))))

(define (main args)
  (unless (= (length args) 2)
    (display "Usage: vm.scm <VM-code-file>") (newline) (exit 1))
  (let ([env (primary-rhein-environment)]
        [code (call-with-input-file (list-ref args 1) read)])
    (compile-all env code)
    (run-function-with-name env "entry")))

