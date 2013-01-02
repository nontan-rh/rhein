;;
;; compile.scm - Re-written bootstrap compiler
;;

(use srfi-1)
(use srfi-9)
(use util.match)
(use gauche.lazy)
(use gauche.parameter)

;;
;; Gauche lazy sequence library extention
;;

(define (lconcatenate lst) (fold-left lappend (x->lseq '()) lst))
(define (make-lseq . x) (x->lseq x))

;;
;; Resolve variable/function names and allocate numeric id
;;

(define *local-level* (make-parameter 0))
(define *local-variables* (make-parameter '() ))
(define *local-variables-max* (make-parameter 0))
(define *local-variables-top* (make-parameter 0))
(define *local-functions* (make-parameter '() ))
(define *local-functions-max* (make-parameter 0))
(define *local-functions-top* (make-parameter 0))

(define (ignore-null fn x) (if (null? x) x (fn x)))

(define (bind-variables names)
  (let lp ([xs names] [alis (*local-variables*)])
    (if (null? xs)
      alis
      (let1 variable-id (*local-variables-top*)
        (*local-variables-top* (+ variable-id 1))
        (*local-variables-max* (max (*local-variables-top*) (*local-variables-max*)))
        (lp (cdr xs) (acons (car xs) (cons (*local-level*) variable-id) alis))))))

(define (bind-functions names)
  (let lp ([xs names] [alis (*local-functions*)])
    (if (null? xs)
      alis
      (let1 function-id (*local-functions-top*)
        (*local-functions-top* (+ function-id 1))
        (*local-functions-max* (max (*local-functions-top*) (*local-functions-max*)))
        (lp (cdr xs) (acons (car xs) (cons (*local-level*) function-id) alis))))))

(define *global-closure-count* 0)

(define (generate-global-closure-name name)
  (begin0
    (string-append "!" name "!" (number->string *global-closure-count*))
    (inc! *global-closure-count*)))

;;
;; Reference sequence
;; Rhein reference representation and its utilities
;;

(define (get-variable-reference name)
  (let1 local-offset (assoc name (*local-variables*))
    (if local-offset
      `(var-ref-s1 (local ,(cons (- (*local-level*) (cadr local-offset)) (cddr local-offset))))
      `(var-ref-s1 (global ,name)))))
 
(define (get-function-reference name)
  (let1 local-offset (assoc name (*local-functions*))
    (if local-offset
      `(func-ref-s1 (local ,(cons (- (*local-level*) (cadr local-offset)) (cddr local-offset))))
      `(func-ref-s1 (global ,name)))))

(define (make-type-identifier name)
  `(type-identifier-s1 ,name))

(define (compile-post-expr-s1 base cont)
  (let1 buf (undefined)
    (match base
      [('raw-ident name)
       (match cont
         [(('funcall-name-s0 args) contx ...)
          (set! buf (compile-funcall-name-s1 name (map compile-code-s1 args)))
          (set! cont contx)]
         [x (set! buf (get-variable-reference name))])]
      [('hat-ident name)
       (set! buf (get-function-reference name))]
      [('tilde-ident name)
       (set! buf (make-type-identifier name))]
      [x (set! buf (compile-code-s1 x))])
    (do [(cx cont (cdr cx))] [(null? cx) buf]
      (match (car cx)
        [('index expr)
         (set! buf `(index-ref-s1 ,buf ,(compile-code-s1 expr)))]
        [('member ('raw-ident name))
         (set! buf `(member-ref-s1 ,buf ,name))]
        [('funcall-expr-s0 args)
         (set! buf `(funcall-expression-s1 ,buf ,(map compile-code-s1 args)))]
        [('funcall-meth-s0 ('raw-ident name) args)
         (set! buf (compile-funcall-name-s1 name (cons buf (map compile-code-s1 args))))]))))

(define (compile-funcall-name-s1 name args)
  (let1 local-function-id (assoc name (*local-functions*))
    (if local-function-id 
      (let1 offset (cdr local-function-id)
        `(funcall-local-s1 ,(cons (- (*local-level*) (car offset)) (cdr offset)) ,args))
      `(funcall-global-s1 ,name ,args))))

(define (compile-post-expr-left-s1 base cont))

(define (compile-all-s1 ast)
  (define (fn x)
    (match x
      [('defun-s0 _ _ _ _) (compile-func-s1 x)]
      [('class-def-s0 _ _) (compile-class-s1 x)]))
  (map fn ast))

(define (compile-class-s1 ast)
  (match-let1 ('class-def-s0 name mem) ast
    `(class-def-s1 ,name ,mem)))

(define (compile-func-s1 ast)
  (match-let1 ('defun-s0 name label params code) ast
    (parameterize ([*local-level* (+ (*local-level*) 1)]
                   [*local-variables-top* 0]
                   [*local-variables-max* 0]
                   [*local-functions-top* 0]
                   [*local-functions-max* 0])
      (parameterize ([*local-variables* (bind-variables params)])
        (let1 compiled-code (compile-code-s1 code)
          `(defun-s1 ,name ,label ,params
                     ,(*local-variables-max*) ,(*local-functions-max*)
                     ,compiled-code))))))

(define (compile-closures-s1 ast)
  (define (defun-name x) (match-let1 ('defun-s0 name _ _ _) x name))
  (define (defun-name-replace x name)
    (match-let1 ('defun-s0 _ label params code) x `(defun-s0 ,name ,label ,params ,code)))
  (define (defun-cons-offset local-name x)
    (let1 label (cdr (assoc local-name (*local-functions*)))
      (list (cons (- (*local-level*) (car label)) (cdr label)) x)))
  (let* ([local-names (map defun-name ast)]
         [global-names (map generate-global-closure-name local-names)])
    (parameterize ([*local-functions* (bind-functions local-names)])
      (let1 replaced-defuns (map defun-name-replace ast global-names)
        (let1 compiled-defuns (map compile-func-s1 replaced-defuns)
          (values (*local-functions*) (map defun-cons-offset local-names compiled-defuns)))))))

(define (compile-code-s1 ast)
  (match ast
    [('block-s0 vars funcs code)
     (parameterize ([*local-variables* (bind-variables vars)])
       (receive (function-bind fcode) (compile-closures-s1 funcs)
         (parameterize ([*local-functions* function-bind])
           `(block-s1 ,vars ,fcode ,(map compile-code-s1 code)))))]
    [('cond-branch-s0 cond-clause else-clause)
     (let ([cond-clause-s1
             (map (match-lambda [(cnd code) `(,(compile-code-s1 cnd) ,(compile-code-s1 code))])
                  cond-clause)]
           [else-clause-s1 (ignore-null compile-code-s1 else-clause)])
       `(cond-branch-s1 ,cond-clause-s1 ,else-clause-s1))]
    [('loop-s0 label cnd code)
     `(loop-s1 ,label ,(compile-code-s1 cnd) ,(compile-code-s1 code))]
    [('label-break-s0 name expr)
     `(label-break-s1 ,name ,(compile-code-s1 expr))]
    [('post-expr-s0 base cont) (compile-post-expr-s1 base cont)]
    [('set-s0 refsqs expr)
     `(set-s1 ,(map (^x (compile-post-expr-s1 (cadr x) (caddr x))) refsqs) ,(compile-code-s1 expr))]
    [('uni-op-s0 op expr) `(uni-op-s1 ,op ,(compile-code-s1 expr))]
    [('bin-op-s0 op lft rht) `(bin-op-s1 ,op ,(compile-code-s1 lft) ,(compile-code-s1 rht))]
    [('funcall-expr-s0 expr args)
     `(funcall-expression-s1 ,(compile-code-s1 expr) ,(map compile-code-s1 args))]
    [('hash-literal-s0 pairs)
     `(hash-literal-s1 ,(map (^x (cons (compile-code-s1 (car x)) (compile-code-s1 (cdr x)))) pairs))]
    [('array-literal-s0 exprs) `(array-literal-s1 ,(map (^x (compile-code-s1 x)) exprs))]
    [('proc-literal-s0 params code) `(proc-literal-s1 ,(compile-proc-literal-s1 params code))]
    [('char-literal-s0 ch) `(char-literal-s1 ,ch)]
    [('string-literal-s0 v) `(string-literal-s1 ,v)]
    [('int-literal-s0 v) `(int-literal-s1 ,v)]))

(define (compile-proc-literal-s1 params code)
  (let1 proc-literal-defun (list 'defun-s0 (generate-global-closure-name "") "" params code)
    (compile-func-s1 proc-literal-defun)))

;;
;; Generate Rhein virtual machine code
;;

(define-record-type rhein-environment #t #t
  (functions) (classes))

(define (make-null-rhein-environment)
  (make-rhein-environment (make-hash-table 'string=?) (make-hash-table 'string=?)))

(define (rhein-install-function name code params varnum funnum regnum)
  (hash-table-put! (~ (*global-environment*) 'functions) name
                   (list code params varnum funnum regnum)))

(define (rhein-install-class name mem)
  (hash-table-put! (~ (*global-environment*) 'classes) name
                   (list mem)))

(define (rhein-generate-output)
  (define (output-function name body)
    (match-let1 (code params varnum funnum regnum) body
      `(function ,name (,regnum ,varnum ,funnum ,(length params)) ,(force code))))
  (define (output-class name body)
    (match-let1 ((mem ...)) body
      `(class ,name ,mem)))
  (append (hash-table-map (~ (*global-environment*) 'classes) output-class)
          (hash-table-map (~ (*global-environment*) 'functions) output-function)))

(define *registers-top* (make-parameter 0))
(define *registers-max* (make-parameter 1))
(define *labels-top* (make-parameter 0))
(define *labels* (make-parameter '() ))
(define *global-environment* (make-parameter '() ))

(define (generate-tag)
  (rlet1 label-id (*labels-top*)
    (*labels-top* (+ label-id 1))))

(define (bind-label name)
  (acons name (cons (*registers-top*) (generate-tag)) (*labels*)))

(define label-register car)
(define label-destination cdr)

(define (renew-registers-max) (*registers-max* (max (+ (*registers-top*) 1) (*registers-max*))))

(define (compile-all-s2 ast)
  (define (fn x)
    (match x
      [('defun-s1 _ _ _ _ _ _) (compile-func-s2 x)]
      [('class-def-s1 _ _) (compile-class-s2 x)]))
  (for-each fn ast))

(define (compile-class-s2 ast)
  (match-let1 ('class-def-s1 name mem) ast
    (rhein-install-class name mem)))

(define (compile-func-s2 ast)
  (match-let1 ('defun-s1 name label params varnum funnum code) ast
    (parameterize ([*labels-top* 0]
                   [*registers-top* 0]
                   [*registers-max* 1]
                   [*labels* '()])
      (parameterize ([*labels* (bind-label label)])
        (let1 function-code (decorate-epilogue (compile-code-s2 code))
          (rhein-install-function name function-code params varnum funnum (*registers-max*)))))))

(define (decorate-epilogue x)
  (lappend x
           (make-lseq `(ret 0))))

(define (compile-closures-s2 ast)
  (define (enclose x)
    (match-let1 (offset (and definition ('defun-s1 name _ _ _ _ _))) x
      (compile-func-s2 definition)
      (x->lseq (list `(enclose ,(*registers-top*) ,name)
                     `(lfset ,(car offset) ,(cdr offset) ,(*registers-top*))))))
  (lconcatenate (map enclose ast)))

(define (compile-code-s2 ast)
  (match ast
    [('block-s1 _ funcs code) (generate-code-block funcs code)]
    [('cond-branch-s1 cond-clause else-clause) (generate-code-branch cond-clause else-clause)]
    [('loop-s1 label cnd code) (generate-code-loop label cnd code)]
    [('label-break-s1 name expr) (generate-code-break name expr)]
    [('var-ref-s1 (kind vinfo)) (generate-code-var-ref kind vinfo)]
    [('func-ref-s1 (kind vinfo)) (generate-code-func-ref kind vinfo)]
    [('index-ref-s1 expr index) (generate-code-index-ref expr index)]
    [('member-ref-s1 expr mem) (generate-code-member-ref expr mem)]
    [('set-s1 reftrs expr) (generate-code-set reftrs expr)]
    [('uni-op-s1 op expr) (generate-code-uni-op op expr)]
    [('bin-op-s1 op lft rht) (generate-code-bin-op op lft rht)]
    [('funcall-global-s1 name args) (generate-code-funcall-global name args)]
    [('funcall-local-s1 offset args) (generate-code-funcall-local offset args)]
    [('funcall-expression-s1 expr args) (generate-code-funcall-expression expr args)]
    [('type-identifier-s1 name) (generate-code-load-tid name)]
    [('array-literal-s1 exprs) (generate-code-load-array exprs)]
    [('hash-literal-s1 pairs) (generate-code-load-hash pairs)]
    [('proc-literal-s1 func) (generate-code-load-proc func)]
    [('char-literal-s1 ch) (generate-code-load-char ch)]
    [('string-literal-s1 v) (generate-code-load-string v)]
    [('int-literal-s1 v) (generate-code-load-int v)]))

(define (generate-code-block funcs code)
  (lappend (compile-closures-s2 funcs) (lconcatenate (map compile-code-s2 code))))

(define (generate-code-branch cond-clause else-clause)
  (define (compile-cond exit-tag ast)
    (let ([next-tag (generate-tag)]
          [condition-register (+ (*registers-top*) 1)])
      (match-let1 (cnd code) ast
        (lappend (parameterize ([*registers-top* condition-register])
                   (renew-registers-max)
                   (compile-code-s2 cnd))
                 (make-lseq `(nif-jump ,condition-register ,next-tag))
                 (compile-code-s2 code)
                 (make-lseq `(jump ,exit-tag)
                            next-tag)))))
  (define (compile-else exit-tag ast)
    (let1 exit-tag-lseq (make-lseq exit-tag)
      (if (null? ast)
        exit-tag-lseq
        (lappend (compile-code-s2 ast) exit-tag-lseq))))
  (let* ([exit-tag (generate-tag)]
         [cond-code (lconcatenate (map (^x (compile-cond exit-tag x)) cond-clause))]
         [else-code (compile-else exit-tag else-clause)])
    (lappend cond-code else-code)))

(define (generate-code-loop label cnd code)
  (parameterize ([*labels* (bind-label label)])
    (let ([loop-tag (generate-tag)]
          [exit-label (cdr (assoc label (*labels*)))]
          [result-register (*registers-top*)]
          [condition-register (+ (*registers-top*) 1)])
      (lappend (make-lseq `(undef ,result-register)
                          loop-tag)
               (parameterize ([*registers-top* condition-register])
                 (renew-registers-max)
                 (compile-code-s2 cnd))
               (make-lseq `(nif-jump ,condition-register ,(label-destination exit-label)))
               (compile-code-s2 code)
               (make-lseq `(jump ,loop-tag)
                          (label-destination exit-label))))))

(define (generate-code-break name expr)
  (let1 exit-label-pair (assoc name (*labels*))
    (unless exit-label-pair
      (error "Label not found"))
    (let1 exit-label (cdr exit-label-pair)
      (lappend (compile-code-s2 expr)
               (make-lseq `(move ,(label-register exit-label) ,(*registers-top*))
                          `(jump ,(label-destination exit-label)))))))

(define (generate-code-var-ref kind vinfo)
  (let1 destination (*registers-top*)
    (make-lseq (case kind
                 [(global) `(gvref ,destination ,vinfo)]
                 [(local) `(lvref ,destination ,(car vinfo) ,(cdr vinfo))]))))

(define (generate-code-func-ref kind vinfo)
  (let1 destination (*registers-top*)
    (make-lseq (case kind
                 [(global) `(gfref ,destination ,vinfo)]
                 [(local) `(lfref ,destination ,(car vinfo) ,(cdr vinfo))]))))

(define (generate-code-index-ref expr index)
  (let ([destination (*registers-top*)]
        [expr-reg (*registers-top*)]
        [index-reg (+ (*registers-top*) 1)])
    (lappend (compile-code-s2 expr)
             (parameterize ([*registers-top* index-reg])
               (renew-registers-max)
               (compile-code-s2 index))
             (make-lseq `(iref ,destination ,expr-reg ,index-reg)))))

(define (generate-code-member-ref expr mem)
  (let ([destination (*registers-top*)]
        [expr-reg (*registers-top*)])
    (lappend (compile-code-s2 expr)
             (make-lseq `(mref ,destination ,expr-reg ,mem)))))

(define (generate-code-set reftrs expr)
  (define (compile-set-index-ref expr index)
    (let ([source (*registers-top*)]
          [expr-reg (+ (*registers-top*) 1)]
          [index-reg (+ (*registers-top*) 2)])
      (lappend (parameterize ([*registers-top* expr-reg])
                 (compile-code-s2 expr))
               (parameterize ([*registers-top* index-reg])
                 (renew-registers-max)
                 (compile-code-s2 index))
               (make-lseq `(iset ,expr-reg ,index-reg ,source)))))
  (define (compile-set-member-ref expr mem)
    (let ([source (*registers-top*)]
          [expr-reg (+ (*registers-top*) 1)])
      (lappend (parameterize ([*registers-top* expr-reg])
                 (renew-registers-max)
                 (compile-code-s2 expr))
               (make-lseq `(mset ,expr-reg ,mem ,source)))))
  (define (compile-set ast)
    (let1 source (*registers-top*)
      (match ast
        [('var-ref-s1 ('global name)) (make-lseq `(gvset ,name ,source))]
        [('var-ref-s1 ('local (layer . offset))) (make-lseq `(lvset ,layer ,offset ,source))]
        [('member-ref-s1 expr mem) (compile-set-member-ref expr mem)]
        [('index-ref-s1 expr index) (compile-set-index-ref expr index)])))
  (lappend (compile-code-s2 expr)
           (lconcatenate (map compile-set reftrs))))

(define (generate-code-uni-op op expr)
  (let1 register (*registers-top*)
    (lappend (compile-code-s2 expr)
             (make-lseq `(,(get-uni-op op) ,register ,register)))))

(define (generate-code-bin-op op lft rht)
  (let ([destination (*registers-top*)]
        [temporary (+ (*registers-top*) 1)])
    (lappend (compile-code-s2 lft)
             (parameterize ([*registers-top* temporary])
               (renew-registers-max)
               (compile-code-s2 rht))
             (make-lseq `(,(get-bin-op op) ,destination ,destination ,temporary)))))

(define (generate-code-argument args)
  (let1 buffer (x->lseq '() )
    (do ([argument-count (*registers-top*) (+ argument-count 1)] [xs args (cdr xs)])
      [(null? xs) buffer]
      (parameterize ([*registers-top* argument-count])
        (renew-registers-max)
        (set! buffer (lappend buffer (compile-code-s2 (car xs))))))))

(define (generate-code-funcall-global name args)
  (let ([args-count (length args)]
        [destination (*registers-top*)]
        [argument-begin (*registers-top*)])
    (lappend (generate-code-argument args)
             (make-lseq `(gcall ,name ,destination ,argument-begin ,args-count)))))

(define (generate-code-funcall-local offset args)
  (let ([args-count (length args)]
        [destination (*registers-top*)]
        [argument-begin (*registers-top*)])
    (lappend (generate-code-argument args)
             (make-lseq
               `(lcall ,(car offset) ,(cdr offset) ,destination ,argument-begin ,args-count)))))

(define (generate-code-funcall-expression expr args)
  (let* ([args-count (length args)]
         [destination (*registers-top*)]
         [argument-begin (*registers-top*)]
         [function (+ (*registers-top*) args-count)])
    (lappend (generate-code-argument args)
             (parameterize ([*registers-top* function])
               (renew-registers-max)
               (compile-code-s2 expr))
             (make-lseq 
               `(ecall ,function ,destination ,argument-begin ,args-count)))))

(define (generate-code-load-tid name)
  (make-lseq `(load-tid ,(*registers-top*) ,name)))

(define (generate-code-load-array exprs)
  (let ([array-len (length exprs)]
        [array-reg (*registers-top*)]
        [array-len-reg (*registers-top*)]
        [tid-reg (*registers-top*)]
        [raw-array-reg (+ (*registers-top*) 1)]
        [count-reg (+ (*registers-top*) 2)]
        [one-reg (+ (*registers-top*) 3)]
        [expr-reg (+ (*registers-top*) 4)])
    (define (load-elements expr)
      (lappend (parameterize ([*registers-top* expr-reg])
                 (renew-registers-max)
                 (compile-code-s2 expr))
               (make-lseq `(raset ,raw-array-reg ,count-reg ,expr-reg)
                          `(bin-add ,count-reg ,count-reg ,one-reg))))
    (lappend (make-lseq `(load-int ,array-len-reg ,array-len)
                        `(ranew ,raw-array-reg ,array-len-reg)
                        `(load-int ,count-reg 0)
                        `(load-int ,one-reg 1))
             (lconcatenate (map load-elements exprs))
             (make-lseq `(load-tid ,tid-reg "array")
                        `(gcall "literal" ,array-reg ,tid-reg 2)))))

(define (generate-code-load-hash pairs)
  (let ([array-len (length pairs)]
        [array-len-reg (*registers-top*)]
        [hash-reg (*registers-top*)]
        [tid-reg (*registers-top*)]
        [raw-key-array-reg (+ (*registers-top*) 1)]
        [raw-value-array-reg (+ (*registers-top*) 2)]
        [count-reg (+ (*registers-top*) 3)]
        [one-reg (+ (*registers-top*) 4)]
        [expr-reg (+ (*registers-top*) 5)])
    (define (load-elements pair)
      (match-let1 (key . value) pair
        (lappend (parameterize ([*registers-top* expr-reg])
                   (renew-registers-max)
                   (compile-code-s2 key))
                 (make-lseq `(raset ,raw-key-array-reg ,count-reg ,expr-reg))
                 (parameterize ([*registers-top* expr-reg])
                   (renew-registers-max)
                   (compile-code-s2 value))
                 (make-lseq `(raset ,raw-value-array-reg ,count-reg ,expr-reg)
                            `(bin-add ,count-reg ,count-reg ,one-reg)))))
    (lappend (make-lseq `(load-int ,array-len-reg ,array-len)
                        `(ranew ,raw-key-array-reg ,array-len-reg)
                        `(ranew ,raw-value-array-reg ,array-len-reg)
                        `(load-int ,count-reg 0)
                        `(load-int ,one-reg 1))
             (lconcatenate (map load-elements pairs))
             (make-lseq `(load-tid ,tid-reg "hash")
                        `(gcall "literal" ,hash-reg ,tid-reg 3)))))

(define (generate-code-load-proc func)
  (match-let1 (and definition ('defun-s1 name _ _ _ _ _)) func
    (compile-func-s2 definition)
    (make-lseq `(enclose ,(*registers-top*) ,name))))

(define (generate-code-load-char ch)
  (make-lseq `(load-char ,(*registers-top*) ,ch)))

(define (generate-code-load-string v)
  (make-lseq `(load-str ,(*registers-top*) ,v)))

(define (generate-code-load-int v)
  (make-lseq `(load-int ,(*registers-top*) ,v)))

(define (get-bin-op op)
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

(define (get-uni-op op)
  (case op
   ['- 'uni-neg]
   ['+ 'uni-plus]
   ['not 'uni-not]
   [else (error "Unknown operator")]))

;;
;; Compiler interface
;;

(define (rhein-compile ast)
  (parameterize ([*global-environment* (make-null-rhein-environment)])
    (compile-all-s2 (compile-all-s1 ast))
    (rhein-generate-output)))

