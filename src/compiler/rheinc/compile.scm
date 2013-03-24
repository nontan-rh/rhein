;;
;; Bytecode compiler
;;

(define-module rheinc.compile
  (use gauche.interactive)
  (use gauche.parameter)
  (use gauche.lazy)
  (use rfc.json)
  (use rheinc.ast)
  (export rhein-compile)
  )

(select-module rheinc.compile)

(define (rhein-compile ast p)
  (resolve-name ast)
  (parameterize ([*global-env* (make <global-environment>)])
    (generate-code ast)
    (print-code (*global-env*) p)))

(define-class <id-allocator> ()
  ((free-list :init-form (make-tree-map = <))
   (max-count :init-value 0)))

(define-method allocate-id! ((ator <id-allocator>))
  (if (zero? (tree-map-num-entries (~ ator 'free-list)))
    (begin0 (~ ator 'max-count) (inc! (~ ator 'max-count)))
    (car (tree-map-pop-min! (~ ator 'free-list)))))

(define-method deallocate-id! ((ator <id-allocator>) id)
  (when (tree-map-exists? (~ ator 'free-list) id)
    (error "Free id collision!!")) (tree-map-push! (~ ator 'free-list) id '() ))

(define-class <local-environment> ()
  ((parent-environment :init-keyword :parent-environment)
   (functions :init-form (make-hash-table 'string=?))
   (function-allocator :init-form (make <id-allocator>))
   (variables :init-form (make-hash-table 'string=?))
   (variable-allocator :init-form (make <id-allocator>))
   (arguments :init-form (make-hash-table 'string=?))
   (argument-allocator :init-form (make <id-allocator>))))

(define-method function-exists? ((env <local-environment>) name)
  (or (hash-table-exists? (~ env 'functions) name)
    (if (null? (~ env 'parent-environment))
      #f
      (function-exists? (~ env 'parent-environment) name))))

(define-method variable-exists? ((env <local-environment>) name)
  (or
    (hash-table-exists? (~ env 'variables) name)
    (if (null? (~ env 'parent-environment))
      #f
      (variable-exists? (~ env 'parent-environment) name))))

(define-method argument-exists? ((env <local-environment>) name)
  (or
    (hash-table-exists? (~ env 'arguments) name)
    (if (null? (~ env 'parent-environment))
      #f
      (argument-exists? (~ env 'parent-environment) name))))

(define-method get-function-relative-address ((env <local-environment>) name)
  (define (help e cnt)
    (cond
      [(hash-table-exists? (~ e 'functions) name)
       (values cnt (hash-table-get (~ e 'functions) name))]
      [(null? (~ e 'parent-environment)) (error "Function does not exist")]
      [else (help (~ e 'parent-environment) (+ cnt 1))]))
  (help env 0))

(define-method get-variable-relative-address ((env <local-environment>) name)
  (define (help e cnt)
    (cond
      [(hash-table-exists? (~ e 'variables) name)
       (values cnt (hash-table-get (~ e 'variables) name))]
      [(null? (~ e 'parent-environment)) (error "Variable does not exist")]
      [else (help (~ e 'parent-environment) (+ cnt 1))]))
  (help env 0))

(define-method get-argument-relative-address ((env <local-environment>) name)
  (define (help e cnt)
    (cond
      [(hash-table-exists? (~ e 'arguments) name)
       (values cnt (hash-table-get (~ e 'arguments) name))]
      [(null? (~ e 'parent-environment)) (error "Variable does not exist")]
      [else (help (~ e 'parent-environment) (+ cnt 1))]))
  (help env 0))

(define-method register-function! ((env <local-environment>) name)
  (when (hash-table-exists? (~ env 'functions) name)
    (error "Function name collision" name))
  (hash-table-put! (~ env 'functions) name (allocate-id! (~ env 'function-allocator))))

(define-method register-variable! ((env <local-environment>) name)
  (when (hash-table-exists? (~ env 'variables) name)
    (error "Variable name collision" name))
  (hash-table-put! (~ env 'variables) name (allocate-id! (~ env 'variable-allocator))))

(define-method register-argument! ((env <local-environment>) name)
  (when (hash-table-exists? (~ env 'arguments) name)
    (error "Argument name collision" name))
  (hash-table-put! (~ env 'arguments) name (allocate-id! (~ env 'argument-allocator))))

(define-method unwind-function! ((env <local-environment>) name)
  (unless (hash-table-exists? (~ env 'functions) name)
    (error "No function" name))
  (deallocate-id! (~ env 'function-allocator) (hash-table-get (~ env 'functions) name))
  (hash-table-delete! (~ env 'functions) name))

(define-method unwind-variable! ((env <local-environment>) name)
  (unless (hash-table-exists? (~ env 'variables) name)
    (error "No variable" name))
  (deallocate-id! (~ env 'variable-allocator) (hash-table-get (~ env 'variables) name))
  (hash-table-delete! (~ env 'variables) name))

(define-method unwind-argument! ((env <local-environment>) name)
  (unless (hash-table-exists? (~ env 'arguments) name)
    (error "No argument" name))
  (deallocate-id! (~ env 'argument-allocator) (hash-table-get (~ env 'arguments) name))
  (hash-table-delete! (~ env 'arguments) name))

;; For debug

(define-method print-bindings ((env <null>))
  (display "functions:") (newline)
  (display "variables:") (newline))

(define-method print-bindings ((env <local-environment>))
  (display "functions:")
  (hash-table-for-each (~ env 'functions) (^(x _) (format #t " ~A" x)))
  (newline)
  (display "variables:")
  (hash-table-for-each (~ env 'variables) (^(x _) (format #t " ~A" x)))
  (newline))

(define-class <local-reference> ()
  ((kind :init-keyword :kind)
   (layer :init-keyword :layer)
   (offset :init-keyword :offset)))

(define-class <global-reference> ()
  ((kind :init-keyword :kind)
   (name :init-keyword :name)))

(define-method load-reference ((lr <local-reference>))
  (case (~ lr 'kind)
    ['variable (list 'lvref (~ lr 'layer) (~ lr 'offset))]
    ['function (list 'lfref (~ lr 'layer) (~ lr 'offset))]
    ['argument (list 'laref (~ lr 'layer) (~ lr 'offset))]
    [else (error "Can't load local-reference" (~ lr 'kind))]))

(define-method load-reference ((gr <global-reference>))
  (case (~ gr 'kind)
    ['variable (list 'gvref (~ gr 'name))]
    ['function (list 'gfref (~ gr 'name))]
    ['class (list 'loadklass (~ gr 'name))]
    [else (error "Can't load local-reference" (~ gr 'kind))]))

(define-method call-reference ((lr <local-reference>))
  (case (~ lr 'kind)
    ['function (list 'lfref (~ lr 'layer) (~ lr 'offset))]
    [else (error "Can't call local-reference" (~ lr 'kind))]))

(define-method call-reference ((gr <global-reference>))
  (case (~ gr 'kind)
    ['function (list 'gfref (~ gr 'name))]
    [else (error "Can't call local-reference" (~ gr 'kind))]))

(define-method set-reference ((lr <local-reference>))
  (case (~ lr 'kind)
    ['variable (list 'lvset (~ lr 'layer) (~ lr 'offset))]
    ['argument (list 'laset (~ lr 'layer) (~ lr 'offset))]
    [else (error "Can't set local-reference" (~ lr 'kind))]))

(define-method set-reference ((gr <global-reference>))
  (case (~ gr 'kind)
    ['variable (list 'gvset (~ gr 'name))]
    [else (error "Can't set global-reference" (~ lr 'kind))]))

(define *local-env* (make-parameter '() ))

(define-method resolve-name ((_ <rh-ast>)))

(define-method resolve-name ((prog <rh-program>))
  (for-each resolve-name (~ prog 'variable-definitions))
  (for-each resolve-name (~ prog 'function-definitions)))

(define-method resolve-name ((variable <rh-global-variable>))
  (define (resolve-each x)
    (parameterize ([*local-env* (make <local-environment> :parent-environment (*local-env*))])
      (unless (null? (~ x 'initial-value))
        (resolve-name (~ x 'initial-value)))))
  (for-each resolve-each (~ variable 'variable-definitions)))

(define-method resolve-name ((func <rh-function>))
  (parameterize ([*local-env* (make <local-environment> :parent-environment (*local-env*))])
    (for-each (^x (register-argument! (*local-env*) x)) (~ func 'parameters))
    (resolve-name (~ func 'code))
    (for-each (^x (unwind-argument! (*local-env*) x)) (~ func 'parameters))
    (set! (~ func 'function-count) (~ (*local-env*) 'function-allocator 'max-count))
    (set! (~ func 'variable-count) (~ (*local-env*) 'variable-allocator 'max-count))
    (set! (~ func 'argument-types) (~ func 'argument-types))))

(define-method resolve-name ((clo <rh-closure-function>))
  (receive [l o] (get-function-relative-address (*local-env*) (~ clo 'name))
    (set! (~ clo 'reference) (make <local-reference> :layer l :offset o)))
  (next-method))

(define-method resolve-name ((blk <rh-block>))
  (for-each (^x (register-variable! (*local-env*) (~ x 'name))) (~ blk 'variable-definitions))
  (for-each (^x (register-function! (*local-env*) (~ x 'name))) (~ blk 'function-definitions))
  (for-each resolve-name (~ blk 'function-definitions))
  (for-each resolve-name (~ blk 'variable-definitions))
  (for-each resolve-name (~ blk 'code))
  (for-each (^x (unwind-variable! (*local-env*) (~ x 'name))) (~ blk 'variable-definitions))
  (for-each (^x (unwind-function! (*local-env*) (~ x 'name))) (~ blk 'function-definitions)))

(define-method resolve-name ((vd <rh-variable-definition>))
  (receive [l o] (get-variable-relative-address (*local-env*) (~ vd 'name))
    (set! (~ vd 'reference) (make <local-reference> :kind 'variable :layer l :offset o)))
  (unless (null? (~ vd 'initial-value))
    (resolve-name (~ vd 'initial-value))))

(define-method resolve-name ((ife <rh-if-expression>))
  (for-each resolve-name (~ ife 'conditional-clauses))
  (resolve-name (~ ife 'else-clause)))

(define-method resolve-name ((cc <rh-conditional-clause>))
  (resolve-name (~ cc 'condition-expression))
  (resolve-name (~ cc 'code)))

(define-method resolve-name ((we <rh-while-expression>))
  (resolve-name (~ we 'condition-expression))
  (resolve-name (~ we 'code)))

(define-method resolve-name ((bs <rh-break-statement>))
  (unless (null? (~ bs 'expression))
    (resolve-name (~ bs 'expression))))

(define-method resolve-name ((id <rh-identifier-reference>))
  ;; Identifier as variable
  (case (~ id 'prefix)
    ['hat
     (if (function-exists? (*local-env*) (~ id 'name))
       (receive [l o] (get-function-relative-address (*local-env*) (~ id 'name))
         (set! (~ id 'reference) (make <local-reference> :kind 'function :layer l :offset o)))
       (set! (~ id 'reference) (make <global-reference> :kind 'function :name (~ id 'name))))]
    ['tilde
     (set! (~ id 'reference) (make <global-reference> :kind 'class :name (~ id 'name)))]
    ['()
     (cond
       [(variable-exists? (*local-env*) (~ id 'name))
        (receive [l o] (get-variable-relative-address (*local-env*) (~ id 'name))
          (set! (~ id 'reference) (make <local-reference> :kind 'variable :layer l :offset o)))]
       [(argument-exists? (*local-env*) (~ id 'name))
        (receive [l o] (get-argument-relative-address (*local-env*) (~ id 'name))
          (set! (~ id 'reference) (make <local-reference> :kind 'argument :layer l :offset o)))]
       [else
        (set! (~ id 'reference) (make <global-reference> :kind 'variable :name (~ id 'name)))])]))

(define-method resolve-name ((in <rh-index-reference>))
  (resolve-name (~ in 'base-expression))
  (resolve-name (~ in 'index-expression)))

(define-method resolve-name ((me <rh-member-reference>))
  (resolve-name (~ me 'base-expression)))

(define-method resolve-name ((idc <rh-identifier-call>))
  (for-each resolve-name (~ idc 'arguments))
  (if (function-exists? (*local-env*) (~ idc 'name))
    (receive [l o] (get-function-relative-address (*local-env*) (~ idc 'name))
      (set! (~ idc 'reference) (make <local-reference> :kind 'function :layer l :offset o)))
    (set! (~ idc 'reference) (make <global-reference> :kind 'function :name (~ idc 'name)))))

(define-method resolve-name ((ase <rh-assign-expression>))
  (for-each resolve-name (~ ase 'destinations))
  (resolve-name (~ ase 'expression)))

(define-method resolve-name ((ue <rh-unary-expression>))
  (resolve-name (~ ue 'expression)))

(define-method resolve-name ((be <rh-binary-expression>))
  (resolve-name (~ be 'left-expression))
  (resolve-name (~ be 'right-expression)))

(define-method resolve-name ((ec <rh-expression-call>))
  (for-each resolve-name (~ ec 'arguments))
  (resolve-name (~ ec 'function)))

(define-method resolve-name ((hl <rh-hash-literal>))
  (for-each resolve-name (~ hl 'contains)))

(define-method resolve-name ((kv <rh-key-value>))
  (resolve-name (~ kv 'key))
  (resolve-name (~ kv 'value)))

(define-method resolve-name ((ar <rh-array-literal>))
  (for-each resolve-name (~ ar 'contains)))

(define-class <tempname-generator> ()
  ((prefix :init-keyword :prefix)
   (name-count :init-value 0)))

(define-method get-tempname ((tempgen <tempname-generator>))
  (begin0
    (string-append (~ tempgen 'prefix) (number->string (~ tempgen 'name-count)))
    (inc! (~ tempgen 'name-count))))

(define-class <bytecode-label> ()
  ((stack-level :init-keyword :stack-level)
   (tag :init-keyword :tag)))

(define-class <bytecode-builder> ()
  ((code :init-form (x->lseq '() ))
   (labels :init-form '() )
   (tag-allocator :init-form (make <id-allocator>))
   (function-count :init-keyword :function-count)
   (variable-count :init-keyword :variable-count)
   (argument-types :init-keyword :argument-types)
   (max-stack :init-value 1)
   (stack-level :init-value 0)))

(define-method emit ((func <bytecode-builder>) code)
  (set! (~ func 'code) (lappend (~ func 'code) (x->lseq (list code)))))

(define-method allocate-label ((bb <bytecode-builder>))
  (number->string (allocate-id! (~ bb 'tag-allocator))))

(define-method register-label ((bb <bytecode-builder>) name label)
  (push! (~ bb 'labels)
    (cons name (make <bytecode-label> :stack-level (~ bb 'stack-level) :tag label))))

(define-method unwind-label ((bb <bytecode-builder>))
  (pop! (~ bb 'labels)))

(define-method get-named-label ((bb <bytecode-builder>) name)
  (let1 res (assoc name (~ bb 'labels))
    (unless res
      (error "No tag" name))
    (cdr res)))

(define-method sinc ((bb <bytecode-builder>) delta)
  (set! (~ bb 'stack-level) (+ (~ bb 'stack-level) delta))
  (set! (~ bb 'max-stack) (max (~ bb 'stack-level) (~ bb 'max-stack))))

(define-method sdec ((bb <bytecode-builder>) delta)
  (set! (~ bb 'stack-level) (- (~ bb 'stack-level) delta)))

(define-class <temporary-function-builder> (<bytecode-builder>) ())

(define-class <named-function-builder> (<bytecode-builder>)
  ((name :init-keyword :name)))

(define-class <compiled-class> ()
  ((name :init-keyword :name)
   (members :init-keyword :members)))

(define-class <variable-builder> (<bytecode-builder>)
  ((name :init-keyword :name)))

(define-class <global-environment> ()
  ((function-tempgen :init-form (make <tempname-generator> :prefix "!!function!!"))
   (functions :init-form (make-hash-table 'string=?))
   (variables :init-form (make-hash-table 'string=?))
   (classes :init-form (make-hash-table 'string=?))))

(define-method install ((genv <global-environment>) (func <named-function-builder>))
  (hash-table-put! (~ genv 'functions) (~ func 'name) func))

(define-method install ((genv <global-environment>) (klass <compiled-class>))
  (hash-table-put! (~ genv 'classes) (~ klass 'name) klass))

(define-method install ((genv <global-environment>) (var <variable-builder>))
  (hash-table-put! (~ genv 'variables) (~ var 'name) var))

(define-method get-function-tempname ((genv <global-environment>))
  (get-tempname (~ genv 'function-tempgen)))

(define *global-env* (make-parameter '() ))

(define-method generate-code ((prog <rh-program>))
  (for-each generate-code (~ prog 'class-definitions))
  (for-each generate-code (~ prog 'variable-definitions))
  (for-each generate-code (~ prog 'function-definitions)))

(define-method generate-code ((klass <rh-class>))
  (install (*global-env*) (make <compiled-class> :name (~ klass 'name) :members (~ klass 'members))))

(define *builder* (make-parameter '() ))

(define-method generate-code ((variable <rh-global-variable>))
  (define (generate-each x)
    (parameterize ([*builder* (make <variable-builder> :name (~ x 'name))])
      (if (null? (~ x 'initial-value))
        (begin (emit (*builder*) (list 'loadundef (peek-register (*builder*)))) (sinc (*builder*) 1))
        (generate-code (~ x 'initial-value)))
      (emit (*builder*) (list 'ret 0))
      (install (*global-env*) (*builder*))))
  (for-each generate-each (~ variable 'variable-definitions)))

(define-method generate-code ((func <rh-function>))
  (parameterize ([*builder* (make <named-function-builder>
                                  :name (~ func 'name)
                                  :function-count (~ func 'function-count)
                                  :variable-count (~ func 'variable-count)
                                  :argument-types (~ func 'argument-types))])
    (let1 exit-label (allocate-label (*builder*))
      (register-label (*builder*) (~ func 'label) exit-label)
      (generate-code (~ func 'code))
      (emit (*builder*) (list 'label exit-label))
      (emit (*builder*) (list 'ret))
      (unwind-label (*builder*)))
    (install (*global-env*) (*builder*))))

(define-method generate-code ((clo <rh-closure-function>))
  (set! (~ clo 'name) (get-function-tempname (*global-env*)))
  (emit (*builder*) (list 'enclose (~ clo 'name))) (sinc (*builder*) 1)
  (emit (*builder*) (list 'lfset (~ clo 'reference 'layer) (~ clo 'reference 'offset)))
  (emit (*builder*) (list 'pop)) (sdec (*builder*) 1)
  (next-method))

(define-method generate-code ((blk <rh-block>))
  (for-each generate-code (~ blk 'function-definitions))
  (for-each generate-code (~ blk 'variable-definitions))
  (if (null? (~ blk 'code))
    (begin (emit (*builder*) (list 'loadundef)) (sinc (*builder*) 1))
    (let* ([revcode (reverse (~ blk 'code))] [lastcode (car revcode)]
           [othercode (reverse (cdr revcode))])
      (define (pop-generate-code x)
        (generate-code x)
        (emit (*builder*) (list 'pop)) (sdec (*builder*) 1))
      (for-each pop-generate-code othercode)
      (generate-code lastcode))))

(define-method generate-code ((vd <rh-variable-definition>))
  (unless (null? (~ vd 'initial-value))
    (generate-code (~ vd 'initial-value))
    (emit (*builder*) (list 'lvset (~ vd 'reference 'layer) (~ vd 'reference 'offset)))
    (emit (*builder*) (list 'pop)) (sdec (*builder*) 1)))

(define-method generate-code ((andex <rh-and-expression>))
  (for-each generate-code (~ andex 'function-definitions))
  (for-each generate-code (~ andex 'variable-definitions))
  (let1 exit-label (allocate-label (*builder*))
    (cond
      [(null? (~ andex 'code))
       (emit (*builder*) (list 'loadtrue) (sinc (*builder*) 1))]
      [else
       (let* ([revcode (reverse (~ andex 'code))]
              [lastcode (car revcode)]
              [othercode (reverse (cdr revcode))])
         (define (jump-generate-code x)
           (generate-code x)
           (emit (*builder*) (list 'dup)) (sinc (*builder*) 1)
           (emit (*builder*) (list 'unlessjump exit-label)) (sdec (*builder*) 1)
           (emit (*builder*) (list 'pop)) (sdec (*builder*) 1))
         (for-each jump-generate-code othercode)
         (generate-code lastcode)
         (emit (*builder*) (list 'label exit-label)))])))

(define-method generate-code ((orex <rh-or-expression>))
  (for-each generate-code (~ orex 'function-definitions))
  (for-each generate-code (~ orex 'variable-definitions))
  (let1 exit-label (allocate-label (*builder*))
    (cond
      [(null? (~ orex 'code))
       (emit (*builder*) (list 'loadfalse) (sinc (*builder*) 1))]
      [else
       (let* ([revcode (reverse (~ orex 'code))]
              [lastcode (car revcode)]
              [othercode (reverse (cdr revcode))])
         (define (jump-generate-code x)
           (generate-code x)
           (emit (*builder*) (list 'dup)) (sinc (*builder*) 1)
           (emit (*builder*) (list 'ifjump exit-label)) (sdec (*builder*) 1)
           (emit (*builder*) (list 'pop)) (sdec (*builder*) 1))
         (for-each jump-generate-code othercode)
         (generate-code lastcode)
         (emit (*builder*) (list 'label exit-label)))])))

(define-method generate-code ((ife <rh-if-expression>))
  (let1 exit-label (allocate-label (*builder*))
    (for-each (cut generate-code <> exit-label) (~ ife 'conditional-clauses))
    (unless (null? (~ ife 'else-clause))
      (generate-code (~ ife 'else-clause)))
    (emit (*builder*) (list 'label exit-label))))

(define-method generate-code ((cc <rh-conditional-clause>) exit-label)
  (let1 next-label (allocate-label (*builder*))
    (generate-code (~ cc 'condition-expression))
    (emit (*builder*) (list 'unlessjump next-label)) (sdec (*builder*) 1)
    (generate-code (~ cc 'code))
    (emit (*builder*) (list 'jump exit-label))
    (emit (*builder*) (list 'label next-label))))

(define-method generate-code ((we <rh-while-expression>))
  (let ([loop-label (allocate-label (*builder*))]
        [exit-label (allocate-label (*builder*))])
    (register-label (*builder*) (~ we 'label) exit-label)
    (emit (*builder*) (list 'label loop-label))
    (generate-code (~ we 'condition-expression))
    (emit (*builder*) (list 'unlessjump exit-label)) (sdec (*builder*) 1)
    (generate-code (~ we 'code))
    (emit (*builder*) (list 'jump loop-label))
    (emit (*builder*) (list 'label exit-label)))
    (unwind-label (*builder*)))

(define-method generate-code ((br <rh-break-statement>))
  (let1 label (get-named-label (*builder*) (~ br 'label))
    (cond
      [(null? (~ br 'expression))
        (emit (*builder*) (list 'loadundef)) (sinc (*builder*) 1)]
      [else
        (generate-code (~ br 'expression))])
    (emit (*builder*) (list 'escape (- (~ (*builder*) 'stack-level) (~ label 'stack-level))))
    (emit (*builder*) (list 'jump (~ label 'tag)))
    (sdec (*builder*) 1))) ;; Justify stack depth

(define-method generate-code ((id <rh-identifier-reference>))
  (emit (*builder*) (load-reference (~ id 'reference))) (sinc (*builder*) 1))

(define-method generate-code ((in <rh-index-reference>))
  (generate-code (~ in 'base-expression))
  (generate-code (~ in 'index-expression))
  (emit (*builder*) (list 'iref)) (sdec (*builder*) 1))

(define-method generate-code ((in <rh-member-reference>))
  (generate-code (~ in 'base-expression))
  (emit (*builder*) (list 'mref (~ in 'member-name))))

(define-method generate-code ((ic <rh-identifier-call>))
  (define (generate-code-inc x)
    (generate-code x))
  (for-each generate-code-inc (reverse (~ ic 'arguments)))
  (emit (*builder*) (call-reference (~ ic 'reference))) (sinc (*builder*) 1)
  (emit (*builder*) (list 'call (length (~ ic 'arguments))))
  (sdec (*builder*) (length (~ ic 'arguments))))

(define-method generate-code ((ae <rh-assign-expression>))
  (define (f x)
    (generate-set-code x) (sdec (*builder*) 1))
  (generate-code (~ ae 'expression))
  (for-each f (~ ae 'destinations)))

(define-method generate-set-code ((ir <rh-identifier-reference>))
  (emit (*builder*) (set-reference (~ ir 'reference))))

(define-method generate-set-code ((in <rh-index-reference>))
  (generate-code (~ in 'base-expression))
  (generate-code (~ in 'index-expression))
  (emit (*builder*) (list 'iset)) (sdec (*builder*) 2))

(define-method generate-set-code ((mr <rh-member-reference>))
  (generate-code (~ mr 'base-expression))
  (emit (*builder*) (list 'mset (~ mr 'member-name))))

(define-method generate-code ((ue <rh-unary-expression>))
  (generate-code (~ ue 'expression))
  (emit (*builder*) (list (unary-op->insn (~ ue 'operator)))))

(define-method generate-code ((be <rh-binary-expression>))
  (generate-code (~ be 'left-expression))
  (generate-code (~ be 'right-expression))
  (emit (*builder*) (list (binary-op->insn (~ be 'operator)))) (sdec (*builder*) 1))

(define (unary-op->insn op)
  (case op
   ['- 'neg]
   ['+ 'plus]
   ['not 'not]
   [else (error "Unknown operator")]))

(define (binary-op->insn op)
  (case op
    ['is 'eq]
    ['isnot 'ne]
    ['> 'gt]
    ['< 'lt]
    ['>= 'ge]
    ['<= 'le]
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['/ 'div]
    [else (error "Unknown operator")]))

(define-method generate-code ((ec <rh-expression-call>))
  (for-each generate-code (reverse (~ ec 'arguments)))
  (generate-code (~ ec 'function))
  (emit (*builder*) (list 'call (length (~ ec 'arguments))))
  (sdec (*builder*) (length (~ ec 'arguments))))

(define-method generate-code ((hl <rh-hash-literal>))
  (let1 array-len (length (~ hl 'contains))
    (emit (*builder*) (list 'load array-len)) (sinc (*builder*) 1)
    (emit (*builder*) (list 'ranew))
    (emit (*builder*) (list 'load 0)) (sinc (*builder*) 1)
    (do [(i 0 (+ 1 i)) (cs (~ hl 'contains) (cdr cs))] [(null? cs) '()]
      (generate-code (~ (car cs) 'value))
      (emit (*builder*) (list 'raset)) (sdec (*builder*) 1)
      (emit (*builder*) (list 'inc)))
    (emit (*builder*) (list 'pop)) (sdec (*builder*) 1)

    (emit (*builder*) (list 'load array-len)) (sinc (*builder*) 1)
    (emit (*builder*) (list 'ranew))
    (emit (*builder*) (list 'load 0)) (sinc (*builder*) 1)
    (do [(i 0 (+ 1 i)) (cs (~ hl 'contains) (cdr cs))] [(null? cs) '()]
      (generate-code (~ (car cs) 'key))
      (emit (*builder*) (list 'raset)) (sdec (*builder*) 1)
      (emit (*builder*) (list 'inc)))
    (emit (*builder*) (list 'pop)) (sdec (*builder*) 1)

    (emit (*builder*) (list 'loadklass "hashtable")) (sinc (*builder*) 1)
    (emit (*builder*) (list 'gfref "literal")) (sinc (*builder*) 1)
    (emit (*builder*) (list 'call 3)) (sdec (*builder*) 2)))

(define-method generate-code ((al <rh-array-literal>))
  (let1 array-len (length (~ al 'contains))
    (emit (*builder*) (list 'load array-len)) (sinc (*builder*) 1)
    (emit (*builder*) (list 'ranew))
    (emit (*builder*) (list 'load 0)) (sinc (*builder*) 1)
    (do [(i 0 (+ 1 i)) (cs (~ al 'contains) (cdr cs))] [(null? cs) '()]
      (generate-code (car cs))
      (emit (*builder*) (list 'raset)) (sdec (*builder*) 1)
      (emit (*builder*) (list 'inc)))
    (emit (*builder*) (list 'pop)) (sdec (*builder*) 1)
    (emit (*builder*) (list 'loadklass "array")) (sinc (*builder*) 1)
    (emit (*builder*) (list 'gfref "literal")) (sinc (*builder*) 1)
    (emit (*builder*) (list 'call 2)) (sdec (*builder*) 2)))

(define-method generate-code ((fl <rh-function-literal>))
  (set! (~ fl 'name) (get-function-tempname (*global-env*)))
  (next-method)
  (emit (*builder*) (list 'enclose  (~ fl 'name))) (sinc (*builder*) 1))

(define-method generate-code ((cl <rh-character-literal>))
  (emit (*builder*) (list 'pushchar (string (~ cl 'value)))) (sinc (*builder*) 1))

(define-method generate-code ((cl <rh-string-literal>))
  (emit (*builder*) (list 'load (~ cl 'value))) (sinc (*builder*) 1))

(define-method generate-code ((cl <rh-integer-literal>))
  (emit (*builder*) (list 'load (~ cl 'value))) (sinc (*builder*) 1))

(define-method generate-code ((tl <rh-true-literal>))
  (emit (*builder*) (list 'loadtrue)) (sinc (*builder*) 1))

(define-method generate-code ((tl <rh-false-literal>))
  (emit (*builder*) (list 'loadfalse)) (sinc (*builder*) 1))

(define-method generate-code ((tl <rh-null-literal>))
  (emit (*builder*) (list 'loadnull)) (sinc (*builder*) 1))

(define-method print-code ((genv <global-environment>) p)
  (let ([c (hash-table-map (~ genv 'classes) print-class)]
        [v (hash-table-map (~ genv 'variables) print-variable)]
        [f (hash-table-map (~ genv 'functions) print-functions)])
    (construct-json (list->vector (append c v f)) p)))

(define (print-class name body)
  (rlet1 r '()
    (push! r (cons "name" name))
    (push! r (cons "type" "class"))
    (push! r (cons "parent" "any"))
    (push! r (cons "members" (list->vector (~ body 'members))))))

(define (print-variable name body)
  (rlet1 r '()
    (push! r (cons "name" name))
    (push! r (cons "type" "variable"))
    (push! r (cons "code" (bytecode->json-string (~ body 'code))))))

(define (print-functions name body)
  (rlet1 r '()
    (push! r (cons "name" name))
    (push! r (cons "type" "function"))
    (push! r (cons "stack_size" (~ body 'max-stack)))
    (push! r (cons "variable_size" (~ body 'variable-count)))
    (push! r (cons "function_size" (~ body 'function-count)))
    (push! r (cons "argument_type" (list->vector (~ body 'argument-types))))
    (push! r (cons "varg" 'false))
    (push! r (cons "code" (print-c (~ body 'code))))))

(define (print-c code)
  (list->vector (map (^x (list->vector (cons (symbol->string (car x)) (cdr x)))) code)))

