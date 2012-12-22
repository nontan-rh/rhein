;;
;; parse.scm - Parse Rhein program
;;

(require "./pcombi")
(require "./scmutil")

(use srfi-1)
(use util.match)

(define (chain-bin-op lft cont)
  (match-let1 (op rht) cont
    (list 'bin-op-s0 (obj-to-string op) lft rht)))

(define (make-literal obj) (list 'int-literal-s0 obj))
(define (make-varref obj) (list 'var obj))

(define (make-funcall-name lis)
  (match-let1 (name args) lis
    (list 'funcall-name-s0 name args)))

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

(define (make-break-stmt lis)
  (match-let1 (_ label expr) lis
    (list 'label-break-s0 label expr)))

(define (report-statement-error ph)
  (receive [lineno column] (lineno-and-column ph)
    (format #t "line:~A col:~A Syntax error, Invalid statement\n" lineno column)
    (exit 1)))

(define (make-rvalue-ref lis) (list 'ref-s0 lis))

(define gr-keyw-local (pkeyword "local"))
(define gr-keyw-defun (pkeyword "defun"))
(define gr-keyw-if (pkeyword "if"))
(define gr-keyw-elif (pkeyword "elif"))
(define gr-keyw-else (pkeyword "else"))
(define gr-keyw-while (pkeyword "while"))
(define gr-keyw-break (pkeyword "break"))
(define gr-keyw (p/ gr-keyw-local
                    gr-keyw-defun
                    gr-keyw-if
                    gr-keyw-elif
                    gr-keyw-else
                    gr-keyw-while
                    gr-keyw-break))
(define gr-lp (pskipwl (pc #[\u0028] )))
(define gr-rp (pskipwl (pc #[\u0029] )))
(define gr-lb (pskipwl (pc #[\u007b] )))
(define gr-rb (pskipwl (pc #[\u007d] )))
(define gr-at (pskipwl (pc #[@])))
(define gr-delim-symbol (pskipwl (p/ (pc #[\u003b]))))
(define gr-delim (pskipwl (p/ (pc #[\u003b]) (pseq))))
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
(define gr-arg-list (pbetween gr-lp (psependby gr-relat-expr gr-comma) gr-rp))
(define gr-if-expr
  (peval make-if-expr
         (pseq (pseq gr-keyw-if gr-relat-expr gr-block)
               (p* (pseq gr-keyw-elif gr-relat-expr gr-block))
               (popt (pseq gr-keyw-else gr-block)))))
(define gr-while-expr
  (peval make-while-expr
         (pseq gr-keyw-while (popt gr-label-decl) gr-relat-expr gr-block)))
(define gr-prim-expr (p/ gr-if-expr
                         gr-while-expr
                         (peval make-literal gr-digit)
                         (peval make-funcall-name (pseq gr-ident gr-arg-list))
                         (peval make-rvalue-ref gr-ref-seq)
                         (pbetween gr-lp gr-relat-expr gr-rp)))
(define gr-ref-seq (pseq (peval make-varref gr-ident)))
(define gr-digit (pskipwl (psn (p+ pdigit))))
(define gr-mul-expr (peval make-binary-expr (pseq gr-prim-expr (p* (pseq gr-mul-op gr-prim-expr)))))
(define gr-add-expr (peval make-binary-expr (pseq gr-mul-expr (p* (pseq gr-add-op gr-mul-expr)))))
(define gr-relat-expr (peval make-binary-expr (pseq gr-add-expr (p* (pseq gr-relat-op gr-add-expr)))))
(define gr-asgn-expr (peval make-asgn-expr
                            (pseq (p+ (pseqn 0 gr-ref-seq gr-asgn-op)) gr-relat-expr)))
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
(define gr-vdecl (p/ (pseqn 2 gr-skip-indent gr-keyw-local (psependby gr-ident gr-comma))))
(define gr-lfdef (pseqn 2 gr-skip-indent gr-func-def))
(define gr-decl-seq (peval make-decl-seq (psependby (p/ gr-lfdef gr-vdecl) gr-delim)))
(define gr-block (peval make-block (pbetween gr-lb (pseq gr-decl-seq gr-stmt-seq) gr-rb)))
(define gr-param-list (pbetween gr-lp (psependby gr-ident gr-comma) gr-rp))
(define gr-func-def (peval make-func-def
                           (pseq gr-keyw-defun gr-ident (popt gr-label-decl)
                                 gr-param-list gr-block)))
(define gr-prog (pseqn 1 (p* (p/ pwhite pnew-line)) (p* (pskipwl gr-func-def)) peof))

(define (parse str)
  (receive [succ _ data] (gr-prog (make-parser-head str 0))
    (if succ
      data
      (error "Parse error!!"))))

