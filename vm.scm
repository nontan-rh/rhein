;;
;; Rhein VM bootstrap
;;

(use srfi-1)
(use srfi-43)
(use util.match)
(use gauche.record)
(use gauche.parameter)

(require "./genseq")
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
    (add-function env "print" rhein-print)
    (add-function env "append" rhein-append)
    (add-function env "push" rhein-push)
    (add-function env "copy" rhein-copy)))

(define-record-type rhein-function #t #t
  (name) (regc) (varc) (func) (argc) (code) (variable-bindings) (function-bindings))

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
                        (make-rhein-function name regc varc func argc (undefined) '() '() )))]))

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
     (make-rhein-function name regc varc func argc (compile-code env code) '() '() )]))

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
                          (match-let1 (code func dst arg0 argc) insn
                            (list code (hash-table-get (~ env 'function-name) func)
                                  dst arg0 argc)))
                         ([enclose gfref]
                          (match-let1 (code dst func) insn
                            (list code dst (hash-table-get (~ env 'function-name) func))))
                         ([jump]
                          (match-let1 (code target) insn
                            (list code (hash-table-get tag-list target))))
                         ([if-jump nif-jump]
                          (match-let1 (code jcond target) insn
                            (list code jcond (hash-table-get tag-list target))))
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
  (variable-bindings) (function-bindings))

(define *current-frame* (make-parameter (undefined)))
(define *call-stack* (make-parameter (undefined)))
(define *environment* (make-parameter (undefined)))

(define (register-ref regno)
  (vector-ref (~ (*current-frame*) 'registers) regno))

(define (register-set! regno value)
  (vector-set! (~ (*current-frame*) 'registers) regno value))

(define (fetch-instruction)
  (begin0
    (vector-ref (~ (*current-frame*) 'function 'code) (~ (*current-frame*) 'program-counter))
    (inc! (~ (*current-frame*) 'program-counter))))

(define (make-frame-with-entry-function fn)
  (make-call-frame fn 0
                   (make-vector (~ fn 'regc)) 0
                   (list (make-vector (~ fn 'varc)))
                   (list (make-vector (~ fn 'func)))))

(define (run-function-with-name env name)
  (run-function-with-id env (hash-table-get (~ env 'function-name) name)))

(define (run-function-with-id env id)
  (let1 entry-function (hash-table-get (~ env 'functions) id)
    (parameterize ([*current-frame* (make-frame-with-entry-function entry-function)]
                   [*call-stack* '()]
                   [*environment* env])
      (let/cc br
        (while #t
          (match (fetch-instruction)
            [('move dst src) (instruction-move dst src)] 
            [('undef dst) (instruction-undef dst)]
            [('enclose dst func) (instruction-enclose dst func)]
            [('gfref dst func) (instruction-gfref dst func)]
            [('gvref dst src) (instruction-gvref dst src)]
            [('lvref dst src-layer src-offset) (instruction-lvref dst src-layer src-offset)]
            [('lfref dst src-layer src-offset) (instruction-lfref dst src-layer src-offset)]
            [('iref dst seq index) (instruction-iref dst seq index)]
            [('gvset dst src) (instruction-gvset dst src)]
            [('lvset dst-layer dst-offset src) (instruction-lvset dst-layer dst-offset src)]
            [('lfset dst-layer dst-offset src) (instruction-lfset dst-layer dst-offset src)]
            [('iset seq index src) (instruction-iset seq index src)]
            [('jump dst) (instruction-jump dst)]
            [('if-jump con dst) (instruction-if-jump con dst)]
            [('nif-jump con dst) (instruction-nif-jump con dst)]
            [('gcall func-id dst arg0 argc) (instruction-gcall func-id dst arg0 argc)]
            [('lcall func-layer func-offset dst arg0 argc)
             (instruction-lcall func-layer func-offset dst arg0 argc)]
            [('ecall func-reg dst arg0 argc) (instruction-ecall func-reg dst arg0 argc)]
            [('ret return-value) 
             (when (null? (*call-stack*))
               (br (register-ref return-value))) ;; Break the loop
             (instruction-ret return-value)]
            [('load-int dst value) (instruction-load-int dst value)]
            [('load-str dst value) (instruction-load-str dst value)]
            [('bin-is dst src1 src2) (instruction-bin-is dst src1 src2)]
            [('bin-isnot dst src1 src2) (instruction-bin-not dst src1 src2)]
            [('bin-gt dst src1 src2) (instruction-bin-gt dst src1 src2)]
            [('bin-lt dst src1 src2) (instruction-bin-lt dst src1 src2)]
            [('bin-ge dst src1 src2) (instruction-bin-ge dst src1 src2)]
            [('bin-le dst src1 src2) (instruction-bin-le dst src1 src2)]
            [('bin-add dst src1 src2) (instruction-bin-add dst src1 src2)]
            [('bin-sub dst src1 src2) (instruction-bin-sub dst src1 src2)]
            [('bin-mul dst src1 src2) (instruction-bin-mul dst src1 src2)]
            [('bin-div dst src1 src2) (instruction-bin-div dst src1 src2)]
            [('uni-neg dst src) (instruction-uni-neg dst src)]
            [('uni-plus dst src) (instruction-uni-plus dst src)]
            [('uni-not dst src) (instruction-uni-not dst src)]))))))

(define (instruction-move dst src)
  (register-set! dst (register-ref src)))

(define (instruction-undef dst)
  (register-set! dst (undefined)))

(define (instruction-enclose dst func)
  (let1 base (hash-table-get (~ (*environment*) 'functions) func)
    (let1 closure (make-rhein-function (~ base 'name) (~ base 'regc) (~ base 'varc)
                                       (~ base 'func) (~ base 'argc) (~ base 'code)
                                       (~ (*current-frame*) 'variable-bindings)
                                       (~ (*current-frame*) 'function-bindings))
      (register-set! dst closure))))

(define (instruction-gfref dst func)
  (register-set! dst (hash-table-get (~ (*environment*) 'functions) func)))

(define (instruction-gvref dst src)
  (register-set! dst (hash-table-get (~ (*environment*) 'variables) src)))

(define (instruction-lvref dst src-layer src-offset)
  (register-set!
    dst
    (vector-ref (list-ref (~ (*current-frame*) 'variable-bindings) src-layer) src-offset)))

(define (instruction-lfref dst src-layer src-offset)
  (register-set!
    dst
    (vector-ref (list-ref (~ (*current-frame*) 'function-bindings) src-layer) src-offset)))

(define (instruction-iref dst seq index)
  (register-set!
    dst
    (generic-sequence-ref (register-ref seq) (register-ref index))))

(define (instruction-gvset dst src)
  (hash-table-put! (~ (*environment*) 'variables) dst (register-ref src)))

(define (instruction-lvset dst-layer dst-offset src)
  (vector-set! (list-ref (~ (*current-frame*) 'variable-bindings) dst-layer) dst-offset
               (register-ref src)))

(define (instruction-lfset dst-layer dst-offset src)
  (vector-set! (list-ref (~ (*current-frame*) 'function-bindings) dst-layer) dst-offset
               (register-ref src)))

(define (instruction-iset seq index src)
  (register-set!
    seq
    (generic-sequence-set! (register-ref seq) index (register-ref src))))

(define (instruction-jump dst)
  (set! (~ (*current-frame*) 'program-counter) dst))

(define (instruction-if-jump con dst)
  (when (register-ref con)
    (set! (~ (*current-frame*) 'program-counter) dst)))

(define (instruction-nif-jump con dst)
  (unless (register-ref con)
    (set! (~ (*current-frame*) 'program-counter) dst)))

(define (call-function func dst arg0 argc)
  (if (procedure? func)
    (register-set! dst (apply func
                              (vector->list
                                (~ (*current-frame*) 'registers) arg0 (+ arg0 argc))))
    (let ([new-frame (make-call-frame func 0
                                      (make-vector (~ func 'regc)) dst
                                      (~ func 'variable-bindings)
                                      (~ func 'function-bindings))]
          [new-variable-layer (vector-append
                                (vector-copy (~ (*current-frame*) 'registers)
                                             arg0 (+ arg0 argc))
                                (make-vector (- (~ func 'varc) argc)))]
          [new-function-layer (make-vector (- (~ func 'func)))])
      (push! (~ new-frame 'variable-bindings) new-variable-layer)
      (push! (~ new-frame 'function-bindings) new-function-layer)
      (*call-stack* (cons (*current-frame*) (*call-stack*)))
      (*current-frame* new-frame))))

(define (instruction-gcall func-id dst arg0 argc)
  (let1 new-func (hash-table-get (~ (*environment*) 'functions) func-id)
    (call-function new-func dst arg0 argc)))

(define (instruction-lcall func-layer func-offset dst arg0 argc)
  (let1 new-func (vector-ref
                   (list-ref (~ (*current-frame*) 'function-bindings) func-layer) func-offset)
    (call-function new-func dst arg0 argc)))

(define (instruction-ecall func-reg dst arg0 argc)
  (let1 new-func (register-ref func-reg)
    (call-function new-func dst arg0 argc)))

(define (instruction-ret return-value)
  (let1 old-frame (*current-frame*)
    (*current-frame* (car (*call-stack*)))
    (*call-stack* (cdr (*call-stack*)))
    (register-set! (~ old-frame 'return-register)
                   (vector-ref (~ old-frame 'registers) return-value))))

(define (instruction-load-int dst value)
  (register-set! dst value))

(define (instruction-load-str dst value)
  (register-set! dst (string->generic-sequence value)))

(define (instruction-bin-is dst src1 src2)
  (register-set! dst (eqv? (register-ref src1) (register-ref src2))))

(define (instruction-bin-isnot dst src1 src2)
  (register-set! dst (not (eqv? (register-ref src1) (register-ref src2)))))

(define (instruction-bin-gt dst src1 src2)
  (register-set! dst (> (register-ref src1) (register-ref src2))))

(define (instruction-bin-lt dst src1 src2)
  (register-set! dst (< (register-ref src1) (register-ref src2))))

(define (instruction-bin-ge dst src1 src2)
  (register-set! dst (>= (register-ref src1) (register-ref src2))))

(define (instruction-bin-le dst src1 src2)
  (register-set! dst (<= (register-ref src1) (register-ref src2))))

(define (instruction-bin-add dst src1 src2)
  (register-set! dst (+ (register-ref src1) (register-ref src2))))

(define (instruction-bin-sub dst src1 src2)
  (register-set! dst (- (register-ref src1) (register-ref src2))))

(define (instruction-bin-mul dst src1 src2)
  (register-set! dst (* (register-ref src1) (register-ref src2))))

(define (instruction-bin-div dst src1 src2)
  (register-set! dst (/ (register-ref src1) (register-ref src2))))

(define (instruction-uni-neg dst src)
  (register-set! dst (- (register-ref src))))

(define (instruction-uni-plus dst src)
  (register-set! dst (+ (register-ref src))))

(define (instruction-uni-not dst src)
  (register-set! dst (not (register-ref src))))

(define (instruction-uni-neg dst src)
  (register-set! dst (- (register-ref src))))

(define (main args)
  (let ([env (primary-rhein-environment)]
        [code (read)])
    (compile-all env code)
    (run-function-with-name env "entry")))

