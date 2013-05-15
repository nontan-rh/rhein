;;
;; compile.scm
;;

(define-module compile
  (use ast)
  (use gauche.parameter)
  (use gauche.lazy)
  (use gauche.process)
  (use srfi-1)
  (use srfi-13)
  (export-all))

(select-module compile)

;; Compiler driver

(define (compile-all a)
  (resolve-name-all a)
  (let1 f (generate-code-all a)
    (verify-code-all f)
    (assembly-output f)))

;; Name resolving

(define-class <slot-reference> ()
  ((kind :init-keyword :kind)
   (depth :init-keyword :depth)
   (offset :init-keyword :offset)))

(define (make-relative-reference r)
  (make <slot-reference>
        :kind (~ r 'kind)
        :depth (- (~ (*local-namespace*) 'closure-level)
                  (~ r 'depth))
        :offset (~ r 'offset)))

(define (make-global-reference k i)
  (make <slot-reference>
        :kind k
        :depth 'global
        :offset i))

(define *local-namespace* (make-parameter #f))

(define-class <local-namespace> ()
  ((closure-parent :init-keyword :closure-parent)
   (closure-level :init-keyword :closure-level)
   (function-count :init-value 0)
   (variable-count :init-value 0)
   (inner-layers :init-value #f)))

(define-class <inner-layer> ()
  ((inner-parent :init-keyword :inner-parent)
   (function-slots :init-form (make-hash-table 'eq?))
   (variable-slots :init-form (make-hash-table 'eq?))))

(define (make-namespace parent)
  (let1 level (if parent
                (+ 1 (~ parent 'closure-level))
                0)
    (make <local-namespace>
          :closure-parent parent
          :closure-level level)))

(define (push-inner-layer! n)
  (set! (~ n 'inner-layers) (make <inner-layer>
                                  :inner-parent (~ n 'inner-layers))))

(define (pop-inner-layer! n)
  (set! (~ n 'inner-layers) (~ n 'inner-layers 'inner-parent)))

;; Outer API
(define-syntax layer-let
  (syntax-rules ()
    ((_ code ...)
     (begin
       (push-inner-layer! (*local-namespace*))
       code ...
       (pop-inner-layer! (*local-namespace*))))))

;; Outer API
(define-syntax closure-let
  (syntax-rules ()
    ((_ code ...)
     (parameterize ((*local-namespace* (make-namespace (*local-namespace*))))
       code ...))))

;; add-slot namespace slot-kind name reference
(define-method add-slot ((l <local-namespace>) (s <symbol>) n r)
  (add-slot (~ l 'inner-layers) s n r))

(define-method add-slot ((i <inner-layer>) (s <symbol>) n r)
  (hash-table-put! (~ i s) n r))

;; get-slot namespace slot-kind name
(define-method get-slot ((l <local-namespace>) (s <symbol>) n)
  (let1 r (get-slot (~ l 'inner-layers) s n)
    (if r
      r
      (get-slot (~ l 'closure-parent) s n))))

(define-method get-slot ((i <inner-layer>) (s <symbol>) n)
  (if (hash-table-exists? (~ i s) n)
    (hash-table-get (~ i s) n)
    (get-slot (~ i 'inner-parent) s n)))

(define-method get-slot ((l <boolean>) _ _)
  #f)

(define (add-reference s1 s2 s3 n)
  (let ([d (~ (*local-namespace*) 'closure-level)]
        [o (~ (*local-namespace*) s1)])
    (inc! (~ (*local-namespace*) s1))
    (add-slot (*local-namespace*) s3 n (make <slot-reference>
                                             :kind s2
                                             :depth d
                                             :offset o))))

;; Outer API
(define (add-function-slot n)
  (add-reference 'function-count 'function 'function-slots n))

;; Outer API
(define (add-variable-slot n)
  (add-reference 'variable-count 'variable 'variable-slots n))

;; Outer API
(define (add-argument-slot n order)
  (add-slot (*local-namespace*) 'variable-slots n
            (make <slot-reference>
                  :kind 'argument
                  :depth (~ (*local-namespace*) 'closure-level)
                  :offset order)))

(define (get-slot-relative s n)
  (let1 r (get-slot (*local-namespace*) s n)
    (if r
      (make-relative-reference r)
      #f)))

;; Outer API
(define (get-function-slot-relative n)
  (get-slot-relative 'function-slots n))

;; Outer API
(define (get-variable-slot-relative n)
  (get-slot-relative 'variable-slots n))

;; Outer API
(define (get-used-function-slot-count)
  (~ (*local-namespace*) 'function-count))

;; Outer API
(define (get-used-variable-slot-count)
  (~ (*local-namespace*) 'variable-count))

;; Traversing

(define (resolve-name-all a)
  (parameterize ([*is-global-context* #t])
    (for-each resolve-name a)))

(define-method resolve-name ((a <rh-ast>))
  ;; Do nothing
  )

(define-method resolve-name ((a <null>))
  ;; Do nothing
  )

(define-method resolve-name ((a <rh-function>))
  (unless (*is-global-context*)
    (add-function-slot (~ a 'id))
    (set! (~ a 'reference) (get-function-slot-relative (~ a 'id))))
  (closure-let
    (layer-let
      (let ([ps (map (cut ~ <> 'id) (~ a 'parameters))]
            [pc (length (~ a 'parameters))])
        (for-each add-argument-slot ps (iota pc))
        (parameterize ([*is-global-context* #f])
          (resolve-name (~ a 'code))))
      (set! (~ a 'function-slot-count) (get-used-function-slot-count))
      (set! (~ a 'variable-slot-count) (get-used-variable-slot-count)))))

(define-method resolve-name ((a <rh-block>))
  (layer-let
    (for-each resolve-name (~ a 'code))))

(define-method resolve-name ((a <rh-local-declaration>))
  (for-each register-locals (~ a 'declarations)))

(define-method register-locals ((a <rh-variable-declaration>))
  (resolve-name (~ a 'initial-value))
  (add-variable-slot (~ a 'id))
  (set! (~ a 'reference) (get-slot-relative 'variable-slots (~ a 'id))))

(define-method register-globals ((a <rh-variable-declaration>))
  (resolve-name (~ a 'initial-value)))

(define-method resolve-name ((a <rh-if-expression>))
  (for-each resolve-name (~ a 'conditional-clauses))
  (resolve-name (~ a 'else-clause)))

(define-method resolve-name ((a <rh-conditional-clause>))
  (resolve-name (~ a 'condition-expression))
  (resolve-name (~ a 'code)))

(define-method resolve-name ((a <rh-while-expression>))
  (resolve-name (~ a 'condition-expression))
  (resolve-name (~ a 'code)))

(define-method resolve-name ((a <rh-break-statement>))
  (resolve-name (~ a 'expression)))

(define-method resolve-name ((a <rh-identifier-reference>))
  (case (~ a 'prefix)
    [(tilde)
     (set! (~ a 'reference)
       (make-global-reference 'klass (~ a 'id)))]
    [(hat)
     (let1 r (get-slot-relative 'function-slots (~ a 'id))
       (set! (~ a 'reference)
         (if r r
           (make-global-reference 'function (~ a 'id)))))]
    ['()
     (let1 r (get-slot-relative 'variable-slots (~ a 'id))
       (set! (~ a 'reference)
         (if r r
           (make-global-reference 'variable (~ a 'id)))))]))

(define-method resolve-name ((a <rh-member-reference>))
  (resolve-name (~ a 'base-expression)))

(define-method resolve-name ((a <rh-index-reference>))
  (resolve-name (~ a 'base-expression))
  (resolve-name (~ a 'index-expression)))

(define-method resolve-name ((a <rh-assign-statement>))
  (for-each resolve-name (~ a 'destinations))
  (resolve-name (~ a 'expression)))

(define-method resolve-name ((a <rh-unary-expression>))
  (resolve-name (~ a 'expression)))

(define-method resolve-name ((a <rh-binary-expression>))
  (resolve-name (~ a 'left-expression))
  (resolve-name (~ a 'right-expression)))

(define-method resolve-name ((a <rh-named-function-call>))
  (let1 r (get-function-slot-relative (~ a 'id))
    (set! (~ a 'reference)
      (if r r
        (make-global-reference 'function (~ a 'id)))))
  (for-each resolve-name (~ a 'arguments)))

(define-method resolve-name ((a <rh-nameless-function-call>))
  (resolve-name (~ a 'function))
  (for-each resolve-name (~ a 'arguments)))

(define-method resolve-name ((a <rh-hash-literal>))
  (for-each resolve-name (~ a 'contains)))

(define-method resolve-name ((a <rh-key-value>))
  (resolve-name (~ a 'key))
  (resolve-name (~ a 'value)))

(define-method resolve-name ((a <rh-array-literal>))
  (for-each resolve-name (~ a 'contains)))

(define-method resolve-name ((a <rh-function-literal>))
  (closure-let
    (layer-let
      (let ([ps (map (cut ~ <> 'id) (~ a 'parameters))]
            [pc (length (~ a 'parameters))])
        (for-each add-argument-slot ps (iota pc))
        (resolve-name (~ a 'code)))
      (set! (~ a 'function-slot-count) (get-used-function-slot-count))
      (set! (~ a 'variable-slot-count) (get-used-variable-slot-count)))))

;; Code generation

(define-class <name-generator> ()
  ((prefix :init-keyword :prefix)))

(define-method generate-name ((g <name-generator>))
  (string-append
    (~ g 'prefix)
    (string-take (call-with-input-process "uuidgen" port->string) 36)))

(define-class <label-generator> ()
  ((counter :init-value 0)))

(define (generate-label)
  (begin0 (~ (*label-generator*) 'counter)
    (inc! (~ (*label-generator*) 'counter))))

(define-class <compiled-object> ()
  ((internal-id :init-keyword :internal-id)
   (external-id :init-keyword :external-id)))

(define-class <compiled-function> (<compiled-object>)
  ((variable-arguments :init-keyword :variable-arguments)
   (parameter-class :init-keyword :parameter-class)
   (function-slot-count :init-keyword :function-slot-count)
   (variable-slot-count :init-keyword :variable-slot-count)
   (stack-size)
   (code :init-keyword :code)))

(define-class <compiled-class> (<compiled-object>)
  ((parent :init-keyword :parent)
   (members :init-keyword :members)))

(define-class <rhein-bytecode-file> ()
  ((object-name-generator :init-form (make <name-generator> :prefix "!!"))
   (functions :init-form (make-hash-table 'string=?))
   (classes :init-form (make-hash-table 'string=?))
   (initializer-id)))

(define-method add-object ((o <compiled-function>))
  (hash-table-put! (~ (*bytecode-file*) 'functions) (~ o 'internal-id) o))

(define-method add-object ((o <compiled-class>))
  (hash-table-put! (~ (*bytecode-file*) 'classes) (~ o 'internal-id) o))

(define (generate-object-name)
  (generate-name (~ (*bytecode-file*) 'object-name-generator)))

(define *bytecode-file* (make-parameter #f))
(define *is-global-context* (make-parameter #f))
(define *label-generator* (make-parameter #f))

(define *initializer-name* (make-parameter #f))

(define (constant t v)
  (list (cons "type" t) (cons "value" v)))

(define (generate-code-all a)
  (parameterize ([*bytecode-file* (make <rhein-bytecode-file>)]
                 [*is-global-context* #t]
                 [*label-generator* (make <label-generator>)])
    (*initializer-name* (generate-object-name))
    (set! (~ (*bytecode-file*) 'initializer-id) (*initializer-name*))
    (add-object (make <compiled-function>
                      :internal-id (*initializer-name*)
                      :code (list->vector
                              (lappend (concatenate (map generate-code a))
                                         (x->lseq (list #("ret")))))
                      :external-id (*initializer-name*)
                      :variable-arguments 'false
                      :function-slot-count 0
                      :variable-slot-count 0
                      :parameter-class #()))
    (*bytecode-file*)))

(define-method get-instruction ((a <slot-reference>))
  (case (~ a 'kind)
    [(variable)
     (if (eq? 'global (~ a 'depth))
       (vector "gvref" (symbol->string (~ a 'offset)))
       (vector "lvref" (~ a 'depth) (~ a 'offset)))]
    [(argument)
     (vector "aref" (~ a 'depth) (~ a 'offset))]
    [(function)
     (if (eq? 'global (~ a 'depth))
       (vector "gfref" (symbol->string (~ a 'offset)))
       (vector "lfref" (~ a 'depth) (~ a 'offset)))]
    [(klass)
     (vector "loadklass" (symbol->string (~ a 'offset)))]
    [else (error "Unknown reference kind" (~ a 'kind))]))

(define-method generate-code ((a <rh-ast>))
  ;; Do nothing
  )

(define-method generate-code ((a <rh-class>))
  (unless (*is-global-context*)
    (error "Cannot define class in local context"))
  (let ([i (generate-object-name)]
        [e (~ a 'id)])
    (add-object (make <compiled-class>
                      :internal-id i
                      :external-id (symbol->string e)
                      :parent (symbol->string (~ a 'parent))
                      :members (list->vector
                                 (map symbol->string (~ a 'members)))))
    (x->lseq
      (list (vector "load" (constant "string" (symbol->string e)))
            (vector "loadklass" i)
            (vector "gfref" "!!register_class")
            (vector "call" 2)))))

(define-method generate-code ((a <rh-function>))
  (let ([i (generate-object-name)]
        [e (~ a 'id)])
    (add-object (make <compiled-function>
                      :internal-id i
                      :external-id (symbol->string e)
                      :code (parameterize ([*is-global-context* #f]
                                           [*label-generator* (make <label-generator>)])
                              (list->vector
                                (lappend (generate-code (~ a 'code))
                                           (x->lseq (list #("ret"))))))
                      :variable-arguments 'false
                      :function-slot-count (~ a 'function-slot-count)
                      :variable-slot-count (~ a 'variable-slot-count)
                      :parameter-class (list->vector (map (cut ~ <> 'type) (~ a 'parameters)))))
    (if (*is-global-context*)
      (x->lseq
        (list (vector "load" (constant "string" (symbol->string e)))
              (vector "gfref" i)
              (vector "gfref" "!!register_function")
              (vector "call" 2)))
      (x->lseq
        (list (vector "enclose" i)
              (vector "lfset" 0 (~ a 'reference 'offset)))))))

(define-method generate-code ((a <rh-global-declaration>))
  (define (generate-each x)
    (lappend (generate-code (~ a 'initial-value))
             (x->lseq
               (list (vector "load" (constant "string" (symbol->string (~ a 'id))))
                     (vector "call" 2)))))
  (unless (*is-global-context*)
    (error "Cannot define global variable in local context"))
  (apply lappend (lmap generate-each (~ a 'declarations))))

(define-method generate-code ((a <rh-local-declaration>))
  (define (generate-each x)
    (lappend (if (null? (~ x 'initial-value))
               (x->lseq (list (vector "loadundef")))
               (generate-code (~ x 'initial-value)))
             (x->lseq
               (list (vector "lvset" 0 (~ x 'reference 'offset))))))
  (define (connect-pop x y)
    (lappend x (x->lseq (list (vector "pop"))) y))
  (when (*is-global-context*)
    (error "Cannot define local variable in global context"))
  (let1 ds (map generate-each (~ a 'declarations))
    (fold-left connect-pop (car ds) (cdr ds))))

(define-method generate-code ((a <rh-block>))
  (define (generate-pop x y)
    (lappend x (x->lseq (list (vector "pop"))) y))
  (if (null? (~ a 'code))
    (x->lseq (list (vector "loadundef")))
    (let1 c (map generate-code (~ a 'code))
      (fold-left generate-pop (car c) (cdr c)))))

(define-method generate-code ((a <rh-if-expression>))
  (let1 exit-label (generate-label)
    (define (generate-each x)
      (let1 next-label (generate-label)
        (lappend (generate-code (~ x 'condition-expression))
                 (x->lseq (list (vector "unlessjump" next-label)))
                 (generate-code (~ x 'code))
                 (x->lseq (list (vector "jump" exit-label)
                                (vector "label" next-label))))))
    (let ([c (apply lappend (map generate-each (~ a 'conditional-clauses)))]
          [e (x->lseq (list (vector "label" exit-label)))])
      (if (null? (~ a 'else-clause))
        (lappend c e)
        (lappend c (generate-code (~ a 'else-clause)) e)))))

(define-method generate-code ((a <rh-while-expression>))
  (let ([loop-label (generate-label)]
        [exit-label (generate-label)])
    (lappend (x->lseq (list (vector "label" loop-label)))
             (generate-code (~ a 'condition-expression))
             (x->lseq (list (vector "unlessjump" exit-label)))
             (generate-code (~ a 'code))
             (x->lseq (list (vector "jump" loop-label)
                            (vector "label" exit-label))))))

(define-method generate-code ((a <rh-break-statement>))
  ; TODO: Implement here
  (error "Not implemented"))

(define-method generate-code ((a <rh-and-expression>))
  (let1 exit-label (generate-label)
    (define (generate-pop x y)
      (lappend x
               (x->lseq (list (vector "dup")
                              (vector "unlessjump" exit-label)
                              (vector "pop")))
               y))
    (let1 c (map generate-code (~ a 'code))
      (lappend (fold-left generate-pop (car c) (cdr c))
               (x->lseq (list (vector "label" exit-label)))))))

(define-method generate-code ((a <rh-or-expression>))
  (let1 exit-label (generate-label)
    (define (generate-pop x y)
      (lappend x
               (x->lseq (list (vector "dup")
                              (vector "ifjump" exit-label)
                              (vector "pop")))
               y))
    (let1 c (map generate-code (~ a 'code))
      (lappend (fold-left generate-pop (car c) (cdr c))
               (x->lseq (list (vector "label" exit-label)))))))

;; Right value index reference
(define-method generate-code ((a <rh-index-reference>))
  (lappend (generate-code (~ a 'base-expression))
           (generate-code (~ a 'index-expression))
           (x->lseq (list (vector "iref")))))

;; Right value member reference
(define-method generate-code ((a <rh-member-reference>))
  (lappend (generate-code (~ a 'base-expression))
           (x->lseq (list (vector "mref" (symbol->string (~ a 'member-id)))))))

(define-method generate-code ((a <rh-assign-statement>))
  (define (generate-assignment x)
    (cond
      [(is-a? x <rh-member-reference>)
       (lappend (generate-code (~ x 'base-expression))
                (x->lseq (list (vector "mset" (symbol->string (~ x 'member-id))))))]
      [(is-a? x <rh-index-reference>)
       (lappend (generate-code (~ x 'base-expression))
                (generate-code (~ x 'index-expression))
                (x->lseq (list (vector "iset"))))]
      [(is-a? x <rh-identifier-reference>)
       (let1 r (~ x 'reference)
         (case (~ r 'kind)
           [(argument)
            (x->lseq (list (vector "aset" (~ r 'depth) (~ r 'offset))))]
           [(variable)
            (if (eq? (~ r 'depth) 'global)
              (x->lseq (list (vector "gvset" (symbol->string (~ r 'offset)))))
              (x->lseq (list (vector "lvset" (~ r 'depth) (~ r 'offset)))))]
           [(function)
            (error "Cannot assign to function slot")]))]))
  (lappend (generate-code (~ a 'expression))
           (apply lappend (map generate-assignment (~ a 'destinations)))))

(define-method generate-code ((a <rh-unary-expression>))
  (define (operator->instruction x)
    (case x
      [(neg) "neg"]
      [(not) "not"]
      [else (error "Program error: No such operator")]))
  (lappend (generate-code (~ a 'expression))
           (x->lseq (list (vector (operator->instruction (~ a 'operator)))))))

(define-method generate-code ((a <rh-binary-expression>))
  (define (operator->instruction x)
    (case x
      [(>) "gt"]
      [(>=) "ge"]
      [(<) "lt"]
      [(<=) "le"]
      [(eq) "eq"]
      [(ne) "ne"]
      [(+) "add"]
      [(-) "sub"]
      [(*) "mul"]
      [(/) "div"]
      [else (error "Program error: No such operator")]))
  (lappend (generate-code (~ a 'left-expression))
           (generate-code (~ a 'right-expression))
           (x->lseq (list (vector (operator->instruction (~ a 'operator)))))))

(define-method generate-code ((a <rh-named-function-call>))
  (lappend (apply lappend (map generate-code (reverse (~ a 'arguments))))
           (x->lseq (list (get-instruction (~ a 'reference))
                          (vector "call" (length (~ a 'arguments)))))))

(define-method generate-code ((a <rh-nameless-function-call>))
  (lappend (apply lappend (map generate-code (reverse (~ a 'arguments))))
           (generate-code (~ a 'function))
           (x->lseq (list (vector "call" (length (~ a 'arguments)))))))

(define-method generate-code ((a <rh-identifier-reference>))
  (x->lseq (list (get-instruction (~ a 'reference)))))

(define (generate-array x)
  (define (set-element y)
    (lappend (generate-code y)
             (x->lseq (list (vector "raset")
                            (vector "inc")))))
  (lappend (x->lseq (list (vector "load" (constant "int" (length x)))
                          (vector "ranew")
                          (vector "load" (constant "int" 0))))
           (apply lappend (map set-element x))
           (x->lseq (list (vector "pop")))))

(define-method generate-code ((a <rh-hash-literal>))
  (lappend (generate-array (map (cut ~ <> 'key) (~ a 'contains)))
           (generate-array (map (cut ~ <> 'value) (~ a 'contains)))
           (x->lseq (list (vector "loadklass" "hashtable")
                          (vector "gfref" "literal")
                          (vector "call" 3)))))

(define-method generate-code ((a <rh-array-literal>))
  (lappend (generate-array (~ a 'contains))
           (x->lseq (list (vector "loadklass" "array")
                          (vector "gfref" "literal")
                          (vector "call" 2)))))

(define-method generate-code ((a <rh-function-literal>))
  (let ([i (generate-object-name)]
        [e ""])
    (add-object (make <compiled-function>
                      :internal-id i
                      :external-id e
                      :code (parameterize ([*is-global-context* #f]
                                           [*label-generator* (make <label-generator>)])
                              (list->vector
                                (lappend (generate-code (~ a 'code))
                                           (x->lseq (list (vector "ret"))))))
                      :variable-arguments #f
                      :function-slot-count (~ a 'function-slot-count)
                      :variable-slot-count (~ a 'variable-slot-count)
                      :parameter-class (map (cut ~ <> 'type) (~ a 'parameters))))
      (x->lseq
        (list (vector "enclose" i)))))

(define-method generate-code ((a <rh-character-literal>))
  (x->lseq
    (list (vector "load" (constant "char" (string (~ a 'value)))))))

(define-method generate-code ((a <rh-string-literal>))
  (x->lseq
    (list (vector "load" (constant "string" (~ a 'value))))))

(define-method generate-code ((a <rh-integer-literal>))
  (x->lseq
    (list (vector "load" (constant "int" (~ a 'value))))))

(define-method generate-code ((a <rh-special-literal>))
  (case (~ a 'value)
    [(nil) (x->lseq (list (vector "loadnull")))]
    [(true) (x->lseq (list (vector "loadtrue")))]
    [(false) (x->lseq (list (vector "loadfalse")))]))

;; Verifing bytecode

(define (verify-code-all f)
  (for-each (lambda (x) (verify-function (cdr x)))
    (hash-table->alist (~ f 'functions))))

(define (verify-function o)
  (let* ([bc (~ o 'code)]
         [bl (vector-length (~ o 'code))]
         [st (make-vector bl #f)]
         [jt (make-hash-table 'string=?)])
    ;; Make jump table
    (let lp ([c 0])
      (if (>= c bl)
        #t
        (let1 i (vector-ref bc c)
          (when (string=? (vector-ref i 0) "label")
            (hash-table-put! jt (vector-ref i 1) c))
          (lp (+ 1 c)))))

    ;; Stack check
    (let lp ([c 0] [h 0])
      (cond
        [(and (vector-ref st c) (= (vector-ref st c)))
         ;; Correct, so stop traversing this node
         ]
        [(vector-ref st c)
         (error "Stack height mismatch")]
        [else
         (vector-set! st c h)
         (let1 i (vector-ref bc c)
           (case (string->symbol (vector-ref i 0))
             [(jump)
              ;; Branch
              (let1 dest (hash-table-ref jt (vector-ref i 1) #f)
                (unless dest
                  (error "No such label"))
                (lp dest h))]
             [(ifjump unlessjump)
              ;; Conditional branch
              (unless (>= h 1)
                (error "Stack underflow" c))
              (let1 dest (hash-table-ref jt (vector-ref i 1) #f)
                (unless dest
                  (error "No such label"))
                (lp dest h)
                (lp (+ c 1) (- h 1)))]
             [(add sub mul div mod eq ne gt lt ge le)
              ;; Binary operator
              (unless (>= h 2)
                (error "Stack underflow" c))
              (lp (+ c 1) (- h 1))]
             [(not neg inc dec)
              (unless (>= h 1)
                (error "Stack underflow" c))
              (lp (+ c 1) h)]
             [(pop)
              (unless (>= h 1)
                (error "Stack underflow" c))
              (lp (+ c 1) (- h 1))]
             [(ret)
              (unless (>= h 1)
                (error "Stack underflow" c))]
             [(call escape)
              (let1 argc (vector-ref i 1)
                (unless (>= h (+ argc 1))
                  (error "Stack underflow" c))
                (lp (+ c 1) (- h argc)))]
             [(ranew mref lfset lvset laset gvset)
              (unless (>= h 1)
                (error "Stack underflow" c))
              (lp (+ c 1) h)]
             [(raref)
              (unless (>= h 2)
                (error "Stack underflow" c))
              (lp (+ c 1) h)]
             [(raset)
              (unless (>= h 3)
                (error "Stack underflow" c))
              (lp (+ c 1) (- h 1))]
             [(iref mset)
              (unless (>= h 2)
                (error "Stack underflow" c))
              (lp (+ c 1) (- h 1))]
             [(iset)
              (unless (>= h 3)
                (error "Stack underflow" c))
              (lp (+ c 1) (- h 2))]
             [(lfref lvref laref gfref gvref
               load loadklass loadundef loadnull loadtrue loadfalse)
              (lp (+ c 1) (+ h 1))]
             [(dup)
              (unless (>= h 1)
                (error "Stack underflow"))
              (lp (+ c 1) (+ h 1))]
             [else
              (error "Unknown instruction" i)]))]))
    (set! (~ o 'stack-size) (fold max 0 (vector->list st)))))

;; Constructing output

(define (assembly-output f)
  (list
    (cons "initializer" (~ f 'initializer-id))
    (cons "objects"
          (list->vector (append
            (map (lambda (x) (class-assembly-output (cdr x)))
                 (hash-table->alist (~ f 'classes)))
            (map (lambda (x) (function-assembly-output (cdr x)))
                 (hash-table->alist (~ f 'functions))))))))

(define (class-assembly-output o)
  (list
    (cons "type" "class")
    (cons "name" (~ o 'internal-id))
    (cons "parent" (~ o 'parent))
    (cons "members" (~ o 'members))))

(define (function-assembly-output o)
  (list
    (cons "type" "function")
    (cons "name" (~ o 'internal-id))
    (cons "varg" (~ o 'variable-arguments))
    (cons "argument_type" (~ o 'parameter-class))
    (cons "function_size" (~ o 'function-slot-count))
    (cons "variable_size" (~ o 'variable-slot-count))
    (cons "stack_size" (~ o 'stack-size))
    (cons "code" (~ o 'code))))

