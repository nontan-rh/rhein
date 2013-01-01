;;
;; parse.scm - Parse Rhein program
;;

(require "./pcombi")

(use srfi-1)
(use util.match)

(define (chain-bin-op lft cont)
  (match-let1 (op rht) cont
    (list 'bin-op-s0 (obj-to-string op) lft rht)))

(define (make-literal obj) (list 'int-literal-s0 obj))

(define (make-funcall-name lis)
  (list 'funcall-name-s0 lis))

(define (make-funcall-expr lis)
  (list 'funcall-expr-s0 lis))

(define (make-stmt-seq lis)
  (filter (^x (not (null? x))) lis))

(define (make-decl-seq lis)
  (define (func? x)
    (match x
      [('defun-s0 _ _ _ _) #t]
      [_ #f]))
  (let ([funcs (filter func? lis)]
        [vars  (remove func? lis)])
    (list (concatenate vars) funcs)))

(define (make-binary-expr lis)
  (define (combine-exprs lft rht)
    (match-let1 (op rhtexpr) rht
      (list 'bin-op-s0 op lft rhtexpr)))
  (match-let1 (lft rhts) lis
    (fold-left combine-exprs lft rhts)))

(define (make-asgn-expr lis)
  (match-let1 (lft rht) lis
    (list 'set-s0 lft rht)))

(define (make-block lis)
  (match-let1 ((vars funcs) sseq) lis
    (list 'block-s0 vars funcs sseq)))

(define (make-func-def lis)
  (match-let1 (_ name label params code) lis
    (list 'defun-s0 name label params code)))

(define (make-if-expr lis)
  (define (make-cond-clause x)
    (match-let1 (_ cnd code) x
      (list cnd code)))
  (match-let1 (if-clause elif-clause else-clause) lis
    (let1 consed-else-clause (if (null? else-clause) '() (match-let1 (_ code) else-clause code))
      (list 'cond-branch-s0 (map make-cond-clause (cons if-clause elif-clause)) consed-else-clause))))

(define (make-while-expr lis)
  (match-let1 (_ label cnd code) lis
    (list 'loop-s0 label cnd code)))

(define (make-proc-literal lis)
  (match-let1 (args code) lis
    (list 'proc-literal-s0 (if (char? args) '() args) code)))

(define (make-break-stmt lis)
  (match-let1 (_ label expr) lis
    (list 'label-break-s0 label expr)))

(define (report-statement-error ph)
  (receive [lineno column] (lineno-and-column ph)
    (format #t "line:~A col:~A Syntax error, Invalid statement\n" lineno column)
    (exit 1)))

(define (make-ref-ident lis)
  (match-let1 (hat ident) lis
    (if (null? hat)
      (list 'raw-ident ident)
      (list 'hat-ident ident))))

(define (make-ref-index lis)
  (list 'index lis))

(define (make-funcall-meth lis)
  (match-let1 (_ name args) lis
    (list 'funcall-meth-s0 name args)))

(define (make-ref-member lis)
  (match-let1 (_ ident) lis
    (list 'member ident)))

(define (make-post-expr lis)
  (match-let1 (base (cont ...)) lis
    (list 'post-expr-s0 base cont)))

(define (make-rvalue-ref lis)
  (list 'ref-s0 lis))

(define (last-and-other lis)
  (let1 rlis (reverse lis)
    (values (car rlis) (reverse (cdr rlis)))))

(define (make-string-literal lis)
  (match-let1 (_ (strbody ...)) lis
    (receive [_ xs] (last-and-other strbody)
      (list 'string-literal-s0 (list->string xs)))))

(define (make-char-literal lis)
  (match-let1 (_ ch) lis
    (list 'char-literal-s0 ch)))

(define (xdigit->char lis)
  (match-let1 (x1 x2) lis
    (integer->char (+ (* 16 (digit->integer x1)) (digit->integer x2)))))

(define (make-member-decl lis) lis)

(define (make-class-block lis) lis)

(define (make-class-def lis)
  (match-let1 (_ name mem) lis
    (list 'class-def-s0 name mem)))

; Lexical

; Keywords
(define gr-keyw-local (pkeyword "local"))
(define gr-keyw-defun (pkeyword "defun"))
(define gr-keyw-if (pkeyword "if"))
(define gr-keyw-elif (pkeyword "elif"))
(define gr-keyw-else (pkeyword "else"))
(define gr-keyw-while (pkeyword "while"))
(define gr-keyw-break (pkeyword "break"))
(define gr-keyw-class (pkeyword "class"))
(define gr-keyw (p/ gr-keyw-local
                    gr-keyw-defun
                    gr-keyw-if
                    gr-keyw-elif
                    gr-keyw-else
                    gr-keyw-while
                    gr-keyw-break))

; Symbols
(define gr-lp (pskipwl (pc #[\u0028])))
(define gr-rp (pskipwl (pc #[\u0029])))
(define gr-lb (pskipwl (pc #[\u007b])))
(define gr-rb (pskipwl (pc #[\u007d])))
(define gr-lbracket (pskipwl (pc #[\u005b])))
(define gr-rbracket (pskipwl (pc #[\u005d])))
(define gr-at (pskipwl (pc #[@])))
(define gr-dq (pskipwl (pc #[\"])))
(define gr-hat (pskipwl (pc #[\^])))
(define gr-hatlp (pskipwl (ps "^(")))
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
  (pskipwl (pseqn 1 (p! gr-keyw) (psc (pseq palpha (p* palphanum))))))
(define gr-label-decl (pseqn 1 gr-at gr-ident))
(define gr-fname-param-list (pbetween gr-lp (psependby gr-ident gr-comma) gr-rp))
(define gr-fname-arg-list (pbetween gr-lp (psependby gr-relat-expr gr-comma) gr-rp))
(define gr-fexpr-param-list (pbetween gr-hatlp (psependby gr-ident gr-comma) gr-rp))
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
(define gr-digit (peval make-literal (pskipwl (psn (p+ pdigit)))))
(define gr-string-char (p/ (peval xdigit->char (pseqn 1 (ps "\\x") (pcount 2 (pc #[[:xdigit:]]))))
                           (pseqn 1 (pc #[\\]) gr-dq)
                           pprint))
(define gr-char (p/ (peval xdigit->char (pseqn 1 (ps "\\x") (pcount 2 (pc #[[:xdigit:]]))))
                    (pseqn 1 (pc #[\\]) gr-dq)
                    pgraph))
(define gr-char-literal (peval make-char-literal (pseq gr-question gr-char)))
(define gr-string (peval make-string-literal (pseq gr-dq (pmtill gr-string-char gr-dq))))
(define gr-paren-expr (pbetween gr-lp gr-relat-expr gr-rp))

; Identifier
(define gr-ref-ident (peval make-ref-ident (pseq (popt gr-hat) gr-ident)))

; Expressions

(define gr-prim-expr (p/ gr-if-expr
                         gr-while-expr
                         gr-proc-literal
                         gr-char-literal
                         gr-string
                         gr-ref-ident
                         gr-digit
                         gr-paren-expr))

; Postfix
(define gr-postfix-index (peval make-ref-index (pbetween gr-lbracket gr-relat-expr gr-rbracket)))
(define gr-postfix-fname (peval make-funcall-name gr-fname-arg-list))
(define gr-postfix-fexpr (peval make-funcall-expr gr-fexpr-arg-list))
(define gr-postfix-fmeth (peval make-funcall-meth (pseq gr-dot gr-ref-ident gr-fname-arg-list)))
(define gr-postfix-member (peval make-ref-member (pseq gr-dot gr-ref-ident)))
(define gr-postfixs (p/ gr-postfix-index
                        gr-postfix-fname
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

; Function

(define gr-vdecl (p/ (pseqn 2 gr-skip-indent gr-keyw-local (psependby gr-ident gr-comma))))
(define gr-lfdef (pseqn 1 gr-skip-indent gr-func-def))
(define gr-decl-seq (peval make-decl-seq (psependby (p/ gr-lfdef gr-vdecl) gr-delim)))
(define gr-block (peval make-block (pbetween gr-lb (pseq gr-decl-seq gr-stmt-seq) gr-rb)))
(define gr-func-def (peval make-func-def
                           (pseq gr-keyw-defun gr-ident (popt gr-label-decl)
                                 gr-fname-param-list gr-block)))

; Class

(define gr-member-decl (peval make-member-decl (psependby gr-ident gr-comma)))
(define gr-class-block (peval make-class-block (pbetween gr-lb gr-member-decl gr-rb)))
(define gr-class-def (peval make-class-def (pseq gr-keyw-class gr-ident gr-class-block)))

; Program

(define gr-define (p/ gr-class-def gr-func-def))
(define gr-prog (pseqn 1 (p* (p/ pwhite pnew-line)) (p* (pskipwl gr-define)) peof))

(define (parse str)
  (receive [succ _ data] (gr-prog (make-parser-head str 0))
    (if succ
      data
      (error "Parse error!!"))))

