;;
;; parse.scm - Parse Rhein program
;;

(require "./pcombi")
(require "./ast")

(use srfi-1)
(use util.match)

(define (make-stmt-seq lis)
  (filter (^x (not (null? x))) lis))

(define (make-decl-seq lis)
  (define (function? x) (is-a? x <rh-function>))
  (define (variable? x) (list? x)) ;;(is-a? x <rh-variable-definition>))
  (let ([funcs (filter function? lis)]
        [vars  (filter variable? lis)])
    (list (concatenate vars) funcs)))

(define (make-binary-expr lis)
  (define (combine-exprs lft cont)
    (match-let1 (op rht) cont
      (make <rh-binary-expression> :operator op :left-expression lft :right-expression rht)))
  (match-let1 (lft rhts) lis
    (fold-left combine-exprs lft rhts)))

(define (make-asgn-expr lis)
  (match-let1 (lft rht) lis
    (make <rh-assign-expression> :destinations lft :expression rht)))

(define (make-block lis)
  (match-let1 ((vars funcs) sseq) lis
    (make <rh-block>
          :variable-definitions vars
          :function-definitions funcs
          :code sseq)))

(define (make-typed-param lis)
  (match-let1 (varname _ vartype) lis
    (make <rh-parameter> :name varname :type vartype)))

(define (make-nontyped-param varname)
  (make <rh-parameter> :name varname :type "any"))

(define (make-global-func-def lis)
  (match-let1 (_ name label params code) lis
    (make <rh-function> :name name :label label
                        :parameters (map (^x (~ x 'name)) params)
                        :argument-types (map (^x (~ x 'type)) params)
                        :code code)))

(define (make-closure-func-def lis)
  (match-let1 (_ name label params code) lis
    (make <rh-closure-function> :name name :label label
                                :parameters (map (^x (~ x 'name)) params)
                                :argument-types (map (^x (~ x 'type)) params)
                                :code code)))

(define (make-if-expr lis)
  (define (make-cond-clause x)
    (match-let1 (_ cnd code) x
      (make <rh-conditional-clause> :condition-expression cnd :code code)))
  (match-let1 (if-clause elif-clauses else-clause) lis
    (make <rh-if-expression>
          :conditional-clauses (map make-cond-clause (cons if-clause elif-clauses))
          :else-clause (match else-clause [(_ code) code] [() '()]))))

(define (make-while-expr lis)
  (match-let1 (_ label cnd code) lis
    (make <rh-while-expression>
          :label label
          :condition-expression cnd
          :code code)))

(define (make-funcall-name lis)
  (match-let1 (name (args ...)) lis
    (make <rh-identifier-call> :name name :arguments args)))

(define (make-proc-literal lis)
  (match lis
    [((? char? _) code) (make <rh-function-literal> :parameters '() :code code :label '() )]
    [(params code) (make <rh-function-literal> :parameters params :code code :label '() )]))

(define (make-break-stmt lis)
  (match-let1 (_ label expr) lis
    (make <rh-break-statement> :label label :expression expr)))

(define (report-statement-error ph)
  (receive [lineno column] (lineno-and-column ph)
    (format #t "line:~A col:~A Syntax error, Invalid statement\n" lineno column)
    (exit 1)))

(define (make-ident name) name)

(define (make-ref-ident lis)
  (match-let1 (prefix ident) lis
    (case prefix
      [(#\^) (make <rh-identifier-reference> :prefix 'hat :name ident)]
      [(#\~) (make <rh-identifier-reference> :prefix 'tilde :name ident)]
      ['() (make <rh-identifier-reference> :prefix '() :name ident)])))

(define (make-ref-index lis)
  (list 'index lis))

(define (make-funcall-meth lis)
  (match-let1 (_ name args) lis
    (list 'method-call name args)))

(define (make-funcall-expr lis)
  (list 'expression-call lis))

(define (make-ref-member lis)
  (match-let1 (_ ident) lis
    (list 'member ident)))

(define (make-post-expr lis)
  (define (combine-postfix c b)
    (match c
      [('index expr) (make <rh-index-reference> :base-expression b :index-expression expr)]
      [('expression-call args) (make <rh-expression-call> :function b :arguments args)]
      [('method-call name args) (make <rh-identifier-call> :name name :arguments (cons b args))]
      [('member name) (make <rh-member-reference> :base-expression b :member-name name)]))
  (match-let1 (base (cont ...)) lis
    (fold combine-postfix base cont)))

(define (last-and-other lis)
  (let1 rlis (reverse lis)
    (values (car rlis) (reverse (cdr rlis)))))

(define (make-string-literal lis)
  (match-let1 (_ (strbody ...)) lis
    (receive [_ xs] (last-and-other strbody)
      (make <rh-string-literal> :value (list->string xs)))))

(define (make-char-literal lis)
  (match-let1 (_ ch) lis
    (make <rh-character-literal> :value ch)))

(define (xdigit->char lis)
  (match-let1 (x1 x2) lis
    (integer->char (+ (* 16 (digit->integer x1)) (digit->integer x2)))))

(define (make-member-decl lis) lis)

(define (make-class-block lis) lis)

(define (make-class-def lis)
  (match-let1 (_ name mem) lis
    (make <rh-class> :name name :members mem)))

(define (make-array-literal lis)
  (match-let1 (exprs ...) lis
    (make <rh-array-literal> :contains exprs)))

(define (make-hash-pair lis)
  (match-let1 (key _ value) lis
    (make <rh-key-value> :key key :value value)))

(define (make-hash-literal lis)
  (match-let1 (pairs ...) lis
    (make <rh-hash-literal> :contains pairs)))

(define (make-int-literal value) (make <rh-integer-literal> :value value))

(define (make-global-var-def lis)
  (match-let1 (_ (defs ...)) lis
    (make <rh-global-variable> :variable-definitions defs)))

(define (make-var-init lis)
  (match-let1 (name expr) lis
    (make <rh-variable-definition> :name name :initial-value expr)))

(define (make-program lis)
  (define (class? x) (is-a? x <rh-class>))
  (define (function? x) (is-a? x <rh-function>))
  (define (variable? x) (is-a? x <rh-global-variable>))
  (let ([classes (filter class? lis)]
        [functions (filter function? lis)]
        [variables (filter variable? lis)])
    (make <rh-program>
          :function-definitions functions
          :class-definitions classes
          :variable-definitions variables)))

; Lexical

; Keywords
(define gr-keyw-local (pkeyword "local"))
(define gr-keyw-def (pkeyword "def"))
(define gr-keyw-if (pkeyword "if"))
(define gr-keyw-elif (pkeyword "elif"))
(define gr-keyw-else (pkeyword "else"))
(define gr-keyw-while (pkeyword "while"))
(define gr-keyw-break (pkeyword "break"))
(define gr-keyw-class (pkeyword "class"))
(define gr-keyw-global (pkeyword "global"))
(define gr-keyw (p/ gr-keyw-local
                    gr-keyw-def
                    gr-keyw-if
                    gr-keyw-elif
                    gr-keyw-else
                    gr-keyw-while
                    gr-keyw-break
                    gr-keyw-class
                    gr-keyw-global))

; Symbols
(define gr-lp (pskipwl (pc #[\u0028])))
(define gr-rp (pskipwl (pc #[\u0029])))
(define gr-lb (pskipwl (pc #[\u007b])))
(define gr-rb (pskipwl (pc #[\u007d])))
(define gr-lbracket (pskipwl (pc #[\u005b])))
(define gr-rbracket (pskipwl (pc #[\u005d])))
(define gr-at (pskipwl (pc #[@])))
(define gr-colon (pskipwl (pc #[:])))
(define gr-dq (pskipwl (pc #[\"])))
(define gr-hat (pskipwl (pc #[\^])))
(define gr-hatlp (pskipwl (ps "^(")))
(define gr-tilde (pskipwl (pc #[\~])))
(define gr-dlbracket (pskipwl (ps "$[")))
(define gr-dlb (pskipwl (ps "${")))
(define gr-question (pc #[?]))
(define gr-delim-symbol (pskipwl (p/ (pc #[\u003b]))))
(define gr-delim (pskipwl (p/ (pc #[\u003b]) (pseq))))
(define gr-dot (pskipwl (pc #[\.])))
(define gr-comma (pskipwl (pc #[,])))
(define gr-mul-op (p/ (pkeysym "*") (pkeysym "/")))
(define gr-add-op (p/ (pkeysym "+") (pkeysym "-")))
(define gr-relat-op (p/ (pkeyword "is")
                        (pkeyword "isnot")
                        (pkeysym ">")
                        (pkeysym "<")
                        (pkeysym ">=")
                        (pkeysym "<=")))
(define gr-asgn-op (pkeysym "="))
(define gr-ident
  (peval make-ident
         (pskipwl (pseqn 1 (p! gr-keyw) (psc (pseq palpha (p* (p/ palphanum (pc #[_])))))))))
(define gr-label-decl (pseqn 1 gr-at gr-ident))
(define gr-param (p/ (peval make-typed-param (pseq gr-ident gr-colon gr-ident))
                     (peval make-nontyped-param gr-ident)))
(define gr-fname-param-list (pbetween gr-lp (psependby gr-param gr-comma) gr-rp))
(define gr-fname-arg-list (pbetween gr-lp (psependby gr-relat-expr gr-comma) gr-rp))
(define gr-fexpr-param-list (pbetween gr-hatlp (psependby gr-param gr-comma) gr-rp))
(define gr-fexpr-arg-list (pbetween gr-hatlp (psependby gr-relat-expr gr-comma) gr-rp))

; Control structures

(define gr-if-expr
  (peval make-if-expr
         (pseq (pseq gr-keyw-if gr-relat-expr gr-block)
               (p* (pseq gr-keyw-elif gr-relat-expr gr-block))
               (popt (pseq gr-keyw-else gr-block)))))
(define gr-while-expr
  (peval make-while-expr
         (pseq gr-keyw-while (popt gr-label-decl) gr-relat-expr gr-block)))

; Literals
(define gr-proc-literal (peval make-proc-literal (pseq (p/ gr-fexpr-param-list gr-hat) gr-block)))
(define gr-digit (peval make-int-literal (pskipwl (psn (p+ pdigit)))))
(define gr-string-char (p/ (peval xdigit->char (pseqn 1 (ps "\\x") (pcount 2 (pc #[[:xdigit:]]))))
                           (pseqn 1 (pc #[\\]) gr-dq)
                           pprint))
(define gr-char (p/ (peval xdigit->char (pseqn 1 (ps "\\x") (pcount 2 (pc #[[:xdigit:]]))))
                    (pseqn 1 (pc #[\\]) gr-dq)
                    pgraph))
(define gr-char-literal (peval make-char-literal (pseq gr-question gr-char)))
(define gr-array-literal (peval
                           make-array-literal
                           (pbetween gr-dlbracket (psependby gr-relat-expr gr-comma) gr-rbracket)))
(define gr-hash-pair (peval make-hash-pair (pseq gr-relat-expr gr-colon gr-relat-expr)))
(define gr-hash-literal (peval
                          make-hash-literal
                          (pbetween gr-dlb (psependby gr-hash-pair gr-comma) gr-rb)))
(define gr-string (peval make-string-literal (pseq gr-dq (pmtill gr-string-char gr-dq))))
(define gr-paren-expr (pbetween gr-lp gr-relat-expr gr-rp))

; Identifier
(define gr-ref-ident (peval make-ref-ident (pseq (popt (p/ gr-hat gr-tilde)) gr-ident)))

; Normal function call
(define gr-funcall-name (peval make-funcall-name (pseq gr-ident gr-fname-arg-list)))

; Expressions

(define gr-prim-expr (p/ gr-if-expr
                         gr-while-expr
                         gr-proc-literal
                         gr-char-literal
                         gr-array-literal
                         gr-hash-literal
                         gr-string
                         gr-funcall-name
                         gr-ref-ident
                         gr-digit
                         gr-paren-expr))

; Postfix
(define gr-postfix-index (peval make-ref-index (pbetween gr-lbracket gr-relat-expr gr-rbracket)))
(define gr-postfix-fexpr (peval make-funcall-expr gr-fexpr-arg-list))
(define gr-postfix-fmeth (peval make-funcall-meth (pseq gr-dot gr-ident gr-fname-arg-list)))
(define gr-postfix-member (peval make-ref-member (pseq gr-dot gr-ident)))
(define gr-postfixs (p/ gr-postfix-index
                        gr-postfix-fexpr
                        gr-postfix-fmeth
                        gr-postfix-member))
(define gr-post-expr (peval make-post-expr (pseq gr-prim-expr (p* gr-postfixs))))

; Binary
(define gr-mul-expr (peval make-binary-expr (pseq gr-post-expr (p* (pseq gr-mul-op gr-post-expr)))))
(define gr-add-expr (peval make-binary-expr (pseq gr-mul-expr (p* (pseq gr-add-op gr-mul-expr)))))
(define gr-relat-expr (peval make-binary-expr (pseq gr-add-expr (p* (pseq gr-relat-op gr-add-expr)))))

; Statements
(define gr-asgn-expr (peval make-asgn-expr
                            (pseq (p+ (pseqn 0 gr-post-expr gr-asgn-op)) gr-relat-expr)))
(define gr-break-stmt (peval make-break-stmt
                             (pseq gr-keyw-break (popt gr-label-decl)
                                   (popt gr-relat-expr))))

(define gr-skip-indent (p* pwhite))
(define gr-stmt (p/ (pseqn 1 gr-skip-indent gr-break-stmt)
                    (pseqn 1 gr-skip-indent gr-asgn-expr)
                    (pseqn 1 gr-skip-indent gr-relat-expr)))
(define gr-stmt-seq
  (peval make-stmt-seq
         (psependby (perror-if report-statement-error
                               gr-stmt
                               (p! (p/ gr-delim-symbol gr-rb)))
                    gr-delim)))

(define gr-var-init
  (peval make-var-init (pseq gr-ident (popt (pseqn 1 gr-asgn-op gr-relat-expr)))))

; Function

(define gr-vdecl (p/ (pseqn 2 gr-skip-indent gr-keyw-local (psependby gr-var-init gr-comma))))
(define gr-lfdef (peval make-closure-func-def (pseqn 1 gr-skip-indent gr-func-def)))
(define gr-decl-seq (peval make-decl-seq (psependby (p/ gr-lfdef gr-vdecl) gr-delim)))
(define gr-block (peval make-block (pbetween gr-lb (pseq gr-decl-seq gr-stmt-seq) gr-rb)))
(define gr-func-def (pseq gr-keyw-def gr-ident (popt gr-label-decl) gr-fname-param-list gr-block))

; Class

(define gr-member-decl (peval make-member-decl (psependby gr-ident gr-comma)))
(define gr-class-block (peval make-class-block (pbetween gr-lb gr-member-decl gr-rb)))
(define gr-class-def (peval make-class-def (pseq gr-keyw-class gr-ident gr-class-block)))

; Global objects

(define gr-global-var-def (peval make-global-var-def
                                 (pseq gr-keyw-global (psependby gr-var-init gr-comma))))
(define gr-global-func-def (peval make-global-func-def gr-func-def))

; Program

(define gr-define (p/ gr-class-def gr-global-func-def gr-global-var-def))
(define gr-prog (peval make-program (pseqn 1 (p* (p/ pwhite pnew-line)) (p* (pskipwl gr-define)) peof)))

(define (parse str)
  (receive [succ _ data] (gr-prog (make-parser-head str 0))
    (if succ
      data
      (error "Parse error!!"))))

