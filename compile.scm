;;
;; Bytecode compiler
;;

(use gauche.interactive)
(use gauche.parameter)
(use gauche.lazy)

(require "./ast")

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
   (variable-allocator :init-form (make <id-allocator>))))

(define-method function-exists? ((env <local-environment>) name)
  (or (hash-table-exists? (~ env 'functions) name)
    (if (null? (~ env 'parent-environment))
      #f
      (function-exists? (~ env 'parent-environment) name))))

(define-method variable-exists? ((env <local-environment>) name)
  (or (hash-table-exists? (~ env 'variables) name)
    (if (null? (~ env 'parent-environment))
      #f
      (variable-exists? (~ env 'parent-environment) name))))

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

(define-method register-function! ((env <local-environment>) name)
  (when (hash-table-exists? (~ env 'functions) name)
    (error "Function name collision" name))
  (hash-table-put! (~ env 'functions) name (allocate-id! (~ env 'function-allocator))))

(define-method register-variable! ((env <local-environment>) name)
  (when (hash-table-exists? (~ env 'variables) name)
    (error "Variable name collision" name))
  (hash-table-put! (~ env 'variables) name (allocate-id! (~ env 'variable-allocator))))

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

(define-method load-reference ((lr <local-reference>) dest)
  (case (~ lr 'kind)
    ['variable (list 'lvref dest (~ lr 'layer) (~ lr 'offset))]
    ['function (list 'lfref dest (~ lr 'layer) (~ lr 'offset))]
    [else (error "Can't load local-reference" (~ lr 'kind))]))

(define-method load-reference ((gr <global-reference>) dest)
  (case (~ gr 'kind)
    ['variable (list 'gvref dest (~ gr 'name))]
    ['function (list 'gfref dest (~ gr 'name))]
    ['class (list 'load-tid dest (~ gr 'name))]
    [else (error "Can't load local-reference" (~ gr 'kind))]))

(define-method call-reference ((lr <local-reference>) dest argc)
  (case (~ lr 'kind)
    ['function (list 'lcall dest (~ lr 'layer) (~ lr 'offset) argc)]
    [else (error "Can't call local-reference" (~ lr 'kind))]))

(define-method call-reference ((gr <global-reference>) dest argc)
  (case (~ gr 'kind)
    ['function (list 'gcall dest (~ gr 'name) argc)]
    [else (error "Can't call local-reference" (~ gr 'kind))]))

(define-method set-reference ((lr <local-reference>) src)
  (case (~ lr 'kind)
    ['variable (list 'lvset (~ lr 'layer) (~ lr 'offset) src)]
    [else (error "Can't set local-reference" (~ lr 'kind))]))

(define-method set-reference ((gr <global-reference>) src)
  (case (~ gr 'kind)
    ['variable (list 'gvset (~ gr 'name) src)]
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
    (for-each (^x (register-variable! (*local-env*) x)) (~ func 'parameters))
    (resolve-name (~ func 'code))
    (set! (~ func 'function-count) (~ (*local-env*) 'function-allocator 'max-count))
    (set! (~ func 'variable-count) (~ (*local-env*) 'variable-allocator 'max-count))
    (set! (~ func 'argument-count) (length (~ func 'parameters)))))

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
    (set! (~ vd 'reference) (make <local-reference> :layer l :offset o)))
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
  (resolve-name (~ we 'expression)))

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
     (if (variable-exists? (*local-env*) (~ id 'name))
       (receive [l o] (get-variable-relative-address (*local-env*) (~ id 'name))
         (set! (~ id 'reference) (make <local-reference> :kind 'variable :layer l :offset o)))
       (set! (~ id 'reference) (make <global-reference> :kind 'variable :name (~ id 'name))))]))

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
  ((register :init-keyword :register)
   (tag :init-keyword :tag)))

(define-class <bytecode-builder> ()
  ((code :init-form (x->lseq '() ))
   (labels :init-form '() )
   (tag-allocator :init-form (make <id-allocator>))
   (register-allocator :init-form (make <id-allocator>))
   (function-count :init-keyword :function-count)
   (variable-count :init-keyword :variable-count)
   (argument-count :init-keyword :argument-count)
   (register-count)))

(define-method emit ((func <bytecode-builder>) code)
  (set! (~ func 'code) (lappend (~ func 'code) (x->lseq (list code)))))

(define-method allocate-tag ((bb <bytecode-builder>))
  (allocate-id! (~ bb 'tag-allocator)))

(define-method allocate-register ((bb <bytecode-builder>))
  (allocate-id! (~ bb 'register-allocator)))

(define-method deallocate-register ((bb <bytecode-builder>) reg)
  (deallocate-id! (~ bb 'register-allocator) reg))

(define-method peek-register ((bb <bytecode-builder>))
  (rlet1 reg (allocate-id! (~ bb 'register-allocator))
    (deallocate-id! (~ bb 'register-allocator) reg)))

(define-method register-label ((bb <bytecode-builder>) name tag retreg)
  (push! (~ bb 'labels) (cons name (make <bytecode-label> :register retreg :tag tag))))

(define-method unwind-label ((bb <bytecode-builder>))
  (pop! (~ bb 'labels)))

(define-method get-top-label ((bb <bytecode-builder>))
  (cdar (~ bb 'labels)))

(define-method get-named-label ((bb <bytecode-builder>) name)
  (let1 res (assoc name (~ bb 'labels))
    (unless res
      (error "No tag" name))
    (cdr res)))

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
        (emit (*builder*) (list 'undef (peek-register (*builder*)))) 
        (generate-code (~ x 'initial-value)))
      (emit (*builder*) (list 'ret 0))
      (install (*global-env*) (*builder*))))
  (for-each generate-each (~ variable 'variable-definitions)))

(define-method generate-code ((func <rh-function>))
  (parameterize ([*builder* (make <named-function-builder>
                                  :name (~ func 'name)
                                  :function-count (~ func 'function-count)
                                  :variable-count (~ func 'variable-count)
                                  :argument-count (~ func 'argument-count))])
    (let ([return-register (peek-register (*builder*))]
          [return-tag (allocate-tag (*builder*))])
      (register-label (*builder*) (~ func 'label) return-tag return-register)
      (generate-code (~ func 'code))
      (emit (*builder*) (list 'ret 0)))
    (set! (~ (*builder*) 'register-count) (~ (*builder*) 'register-allocator 'max-count))
    (install (*global-env*) (*builder*))))

(define-method generate-code ((clo <rh-closure-function>))
  (set! (~ clo 'name) (get-function-tempname (*global-env*)))
  (let1 reg (allocate-register (*builder*))
    (emit (*builder*) (list 'enclose reg (~ clo 'name)))
    (emit (*builder*) (list 'lfset (~ clo 'reference 'layer) (~ clo 'reference 'offset) reg))
    (deallocate-register (*builder*) reg))
  (next-method))

(define-method generate-code ((blk <rh-block>))
  (for-each generate-code (~ blk 'function-definitions))
  (for-each generate-code (~ blk 'variable-definitions))
  (for-each generate-code (~ blk 'code)))

(define-method generate-code ((vd <rh-variable-definition>))
  (if (null? (~ vd 'initial-value))
    (begin)
    (let1 reg (generate-code (~ vd 'initial-value))
      (emit (*builder*) (list 'lvset (~ vd 'reference 'layer) (~ vd 'reference 'offset) reg)))))

(define-method generate-code ((ife <rh-if-expression>))
  (let1 exit-tag (allocate-tag (*builder*))
    (for-each (^x (generate-code x exit-tag)) (~ ife 'conditional-clauses))
    (unless (null? (~ ife 'else-clause))
      (generate-code (~ ife 'else-clause)))
    (emit (*builder*) (list 'tag exit-tag)))
  (peek-register (*builder*)))

(define-method generate-code ((cc <rh-conditional-clause>) exit-tag)
  (let ([cond-reg (generate-code (~ cc 'condition-expression))]
        [next-tag (allocate-tag (*builder*))])
    (emit (*builder*) (list 'nif-jump cond-reg ntag))
    (generate-code (~ cc 'code))
    (emit (*builder*) (list 'jump exit-tag))
    (emit (*builder*) (list 'tag next-tag))))

(define-method generate-code ((we <rh-while-expression>))
  (let ([loop-tag (allocate-tag (*builder*))]
        [label-tag (allocate-tag (*builder*))]
        [label-reg (peek-register (*builder*))])
    (register-label (*builder*) (~ we 'label) label-tag label-reg)
    (emit (*builder*) (list 'tag loop-tag))
    (let1 cond-reg (generate-code (~ we 'condition-expression))
      (emit (*builder*) (list 'nif-jump cond-reg label-tag)))
    (generate-code (~ we 'code))
    (emit (*builder*) (list 'tag label-tag))
    (unwind-label (*builder*)))
  (peek-register (*builder*)))

(define-method generate-code ((br <rh-break-statement>))
  (let1 label (get-named-label (*builder*) (~ br 'label))
    (cond
      [(null? (~ br 'expression))
        (emit (*builder*) (list 'undef (~ label 'register)))]
      [else
        (let1 expr-reg (generate-code (~ br 'expression))
          (emit (*builder*) (list 'move (~ label 'register) expr-reg)))])
    (emit (*builder*) (list 'jump (~ label 'tag)))))

(define-method generate-code ((id <rh-identifier-reference>))
  (let1 reg (peek-register (*builder*))
    (emit (*builder*) (load-reference (~ id 'reference) reg))
    reg))

(define-method generate-code ((in <rh-index-reference>))
  (let* ([base-reg (peek-register (*builder*))] ;; Peek next register will be allocated
         [dest-reg base-reg])
    (generate-code (~ in 'base-expression)) ;; Peeked register will be used
    (allocate-register (*builder*)) ;; Reserve the register peeked
    (let1 index-reg (generate-code (~ in 'index-expression))
      (emit (*builder*) (list 'iref dest-reg base-reg index-reg)))
    (deallocate-register (*builder*) base-reg)
    dest-reg))

(define-method generate-code ((in <rh-member-reference>))
  (let* ([base-reg (peek-register (*builder*))]
         [dest-reg base-reg])
    (generate-code (~ in 'base-expression)) ;; Peeked register will be used
    (emit (*builder*) (list 'mref dest-reg base-reg (~ in 'member-name)))
    dest-reg))

(define-method generate-code ((ic <rh-identifier-call>))
  (let* ([dest-reg (peek-register (*builder*))]
         [arg-reg dest-reg])
    (define (push-argument x)
      (generate-code x)
      (emit (*builder*) (list 'push arg-reg)))
    (for-each push-argument (~ ic 'arguments))
    (emit (*builder*) (call-reference (~ ic 'reference) dest-reg (length (~ ic 'arguments))))))

(define-method generate-code ((ae <rh-assign-expression>))
  (let1 expr-reg (peek-register (*builder*))
    (generate-code (~ ae 'expression)) ;; Peeked register will be used
    (allocate-register (*builder*)) ;; Reserve it
    (for-each (^x (generate-set-code x expr-reg)) (~ ae 'destinations))
    (deallocate-register (*builder*) expr-reg) ;; Free it
    ))

(define-method generate-set-code ((ir <rh-identifier-reference>) value-reg)
  (emit (*builder*) (set-reference (~ ir 'reference) value-reg)))

(define-method generate-set-code ((in <rh-index-reference>) value-reg)
  (let1 base-reg (peek-register (*builder*)) ;; Peek next register will be allocated
    (generate-code (~ in 'base-expression)) ;; Peeked register will be used
    (allocate-register (*builder*)) ;; Reserve the register peeked
    (let1 index-reg (generate-code (~ in 'index-expression))
      (emit (*builder*) (list 'iset base-reg index-reg index-reg)))
    (deallocate-register (*builder*) base-reg)))

(define-method generate-set-code ((mr <rh-member-reference>) value-reg)
  (let1 base-reg (peek-register (*builder*))
    (generate-code (~ mr 'base-expression)) ;; Peeked register will be used
    (emit (*builder*) (list 'mset base-reg (~ mr 'member-name) value-reg))))

(define-method generate-code ((ue <rh-unary-expression>))
  (let1 reg (peek-register (*builder*))
    (generate-code (~ ue 'expression))
    (emit (*builder*) (list (unary-op->insn (~ ue 'operator)) reg reg))
    reg)) 

(define-method generate-code ((be <rh-binary-expression>))
  (let* ([dest-reg (peek-register (*builder*))]
         [left-reg dest-reg])
    (generate-code (~ be 'left-expression))
    (allocate-register (*builder*))
    (let1 right-reg (generate-code (~ be 'right-expression))
      (emit (*builder*) (list (binary-op->insn (~ be 'operator)) dest-reg left-reg right-reg)))
    (deallocate-register (*builder*) left-reg)
    dest-reg))

(define (unary-op->insn op)
  (case op
   ['- 'uni-neg]
   ['+ 'uni-plus]
   ['not 'uni-not]
   [else (error "Unknown operator")]))

(define (binary-op->insn op)
  (case op
    ['is 'bin-is]
    ['isnot 'bin-isnot]
    ['> 'bin-gt]
    ['< 'bin-lt]
    ['>= 'bin-ge]
    ['<= 'bin-le]
    ['+ 'bin-add]
    ['- 'bin-sub]
    ['* 'bin-mul]
    ['/ 'bin-div]
    [else (error "Unknown operator")]))

(define-method generate-code ((ec <rh-expression-call>))
  (let* ([func-reg (peek-register (*builder*))]
         [dest-reg func-reg])
    (generate-code (~ ec 'function))
    (allocate-register (*builder*))
    (let1 arg-reg (peek-register (*builder*))
      (define (push-argument x)
        (generate-code x)
        (emit (*builder*) (list 'push arg-reg)))
      (for-each push-argument (~ ec 'arguments)))
    (emit (*builder*) (list 'ecall dest-reg func-reg (length (~ ec 'arguments))))
    (deallocate-register (*builder*) func-reg)
    dest-reg))

(define-method generate-code ((hl <rh-hash-literal>))
  (let* ([key-array-reg (allocate-register (*builder*))]
         [value-array-reg key-array-reg]
         [tid-reg key-array-reg]
         [counter-reg (allocate-register (*builder*))]
         [one-reg (allocate-register (*builder*))]
         [dest-reg key-array-reg]
         [len-reg (allocate-register (*builder*))]
         [array-len (length (~ hl 'contains))])
    (emit (*builder*) (list 'load-tid tid-reg "hash"))
    (emit (*builder*) (list 'push tid-reg))
    (emit (*builder*) (list 'load-int one-reg 1))
    (emit (*builder*) (list 'load-int len-reg array-len))
    (emit (*builder*) (list 'ranew key-array-reg len-reg))
    (emit (*builder*) (list 'load-int counter-reg 0))
    (do [(i 0 (+ 1 i)) (cs (~ hl 'contains) (cdr cs))] [(null? cs) '()]
      (let1 key-reg (generate-code (~ (car cs) 'key))
        (emit (*builder*) (list 'raset key-array-reg counter-reg key-reg))
        (emit (*builder*) (list 'bin-add counter-reg counter-reg one-reg))))
    (emit (*builder*) (list 'push key-array-reg))
    (emit (*builder*) (list 'ranew value-array-reg array-len))
    (emit (*builder*) (list 'load-int counter-reg 0))
    (do [(i 0 (+ 1 i)) (cs (~ hl 'contains) (cdr cs))] [(null? cs) '()]
      (let1 value-reg (generate-code (~ (car cs) 'value))
        (emit (*builder*) (list 'raset value-array-reg counter-reg value-reg))
        (emit (*builder*) (list 'bin-add counter-reg counter-reg one-reg))))
    (emit (*builder*) (list 'push value-array-reg))
    (emit (*builder*) (list 'gcall dest-reg "literal" 3))
    (deallocate-register (*builder*) len-reg)
    (deallocate-register (*builder*) one-reg)
    (deallocate-register (*builder*) counter-reg)
    (deallocate-register (*builder*) key-array-reg)
    dest-reg))

(define-method generate-code ((al <rh-array-literal>))
  (let* ([array-reg (allocate-register (*builder*))]
         [tid-reg array-reg]
         [counter-reg (allocate-register (*builder*))]
         [one-reg (allocate-register (*builder*))]
         [dest-reg array-reg]
         [len-reg (allocate-register (*builder*))]
         [array-len (length (~ al 'contains))])
    (emit (*builder*) (list 'load-tid tid-reg "array"))
    (emit (*builder*) (list 'push tid-reg))
    (emit (*builder*) (list 'load-int one-reg 1))
    (emit (*builder*) (list 'load-int len-reg array-len))
    (emit (*builder*) (list 'ranew array-reg len-reg))
    (emit (*builder*) (list 'load-int counter-reg 0))
    (do [(i 0 (+ 1 i)) (cs (~ al 'contains) (cdr cs))] [(null? cs) '()]
      (let1 value-reg (generate-code (car cs))
        (emit (*builder*) (list 'raset array-reg counter-reg array-reg))
        (emit (*builder*) (list 'bin-add counter-reg counter-reg one-reg))))
    (emit (*builder*) (list 'push array-reg))
    (emit (*builder*) (list 'gcall dest-reg "literal" 2))
    (deallocate-register (*builder*) len-reg)
    (deallocate-register (*builder*) one-reg)
    (deallocate-register (*builder*) counter-reg)
    (deallocate-register (*builder*) array-reg)
    dest-reg))

(define-method generate-code ((fl <rh-function-literal>))
  (set! (~ fl 'name) (get-function-tempname (*global-env*)))
  (next-method)
  (emit (*builder*) (list 'enclose (peek-register (*builder*)) (~ fl 'name)))
  (peek-register (*builder*)))

(define-method generate-code ((cl <rh-character-literal>))
  (emit (*builder*) (list 'load-char (peek-register (*builder*)) (~ cl 'value)))
  (peek-register (*builder*)))

(define-method generate-code ((cl <rh-string-literal>))
  (emit (*builder*) (list 'load-str (peek-register (*builder*)) (~ cl 'value)))
  (peek-register (*builder*)))

(define-method generate-code ((cl <rh-integer-literal>))
  (emit (*builder*) (list 'load-int (peek-register (*builder*)) (~ cl 'value)))
  (peek-register (*builder*)))

(define-method print-code ((genv <global-environment>) p)
  (hash-table-for-each (~ genv 'classes) (^(k v) (print-class k v p)))
  (hash-table-for-each (~ genv 'variables) (^(k v) (print-variable k v p)))
  (hash-table-for-each (~ genv 'functions) (^(k v) (print-functions k v p))))

(define (print-class name body p)
  (format p "class ~a~%" name)
  (for-each (^x (format p "~a~%" x)) (~ body 'members))
  (format p "endclass~%~%"))

(define (print-variable name body p)
  (format p "variable ~a~%" name)
  (print-bytecode (~ body 'code) p)
  (format p "endvariable~%~%"))

(define (print-functions name body p)
  (format p "function ~s ~s ~s ~s ~s~%"
          name
          (~ body 'argument-count)
          (~ body 'variable-count)
          (~ body 'function-count)
          (~ body 'register-count))
  (print-bytecode (~ body 'code) p)
  (format p "endfunction~%~%"))

(define (print-bytecode code p)
  (define (print-insn x)
    (format p "~s" (car x))
    (for-each (^y (format p " ~s" y)) (cdr x))
    (format p "~%"))
  (for-each print-insn code))

