;;
;; parse.scm - Parse Rhein program
;;
;; Written for gauche
;;
;; Requires srfi-1 and Andrew Wright's match macros
;;

(require "./pcombi")
(require "./scmutil")

(use srfi-1)
(use util.match)

(define-syntax define-gr-cons-other
  (syntax-rules ()
    [(_ name symbol)
     (define (name obj)
       (list 'symbol obj))]))

(define (chain-bin-op lft rht)
  (list 'bin-op-s0 (obj-to-string (car rht)) lft (cadr rht)))

(define-gr-cons-other cons-literal int-literal-s0)
(define-gr-cons-other cons-varref var)

(define (cons-funcall-name lst)
  (match lst
    [(name args) (list 'funcall-name-s0 name args)]))

(define (cons-stmt-seq lst)
  (filter (^x (not (null? x))) lst))

(define (cons-decl-seq lst)
  (let1 func? (^x (and (pair? x) (eq? (car x) 'defun-s0)))
    (let ([funcs (filter func? lst)]
          [vars  (remove func? lst)])
      (list (concatenate vars) funcs))))

(define (cons-asgn-expr lst)
  (match lst
    [(lft rht)
     (list 'set-s0 lft rht)]))

(define (cons-block lst)
  (match lst
    [((vars funcs) sseq) (list 'block-s0 vars funcs sseq)]))

(define (cons-func-def lst)
  (match lst
    [(_ name label params code _) (list 'defun-s0 name label params code)]))

(define (cons-if-expr lst)
  (match lst
    [(if-clause elif-clauses else-clause _)
     (let1 cns (lambda (x)
                 (match x
                   [(_ cnd _ code) (list cnd code)]))
       (list 'cond-branch-s0 (map cns (cons if-clause elif-clauses))
             (if (not (null? else-clause))
                 (match else-clause
                   [(_ code) code])
                 '() )))]))

(define (cons-while-expr lst)
  (match lst
    [(_ label cnd _ code _)
     (list 'loop-s0 label cnd code)]))

(define (cons-break-stmt lst)
  (match lst
    [(_ label expr)
     (list 'label-break-s0 label expr)]))

(define (cons-rvalue-ref lst)
  `(ref-s0 ,lst))

(define gr-keyw-local (pkeyword "local"))
(define gr-keyw-defun (pkeyword "defun"))
(define gr-keyw-end (pkeyword "end"))
(define gr-keyw-if (pkeyword "if"))
(define gr-keyw-elif (pkeyword "elif"))
(define gr-keyw-else (pkeyword "else"))
(define gr-keyw-while (pkeyword "while"))
(define gr-keyw-break (pkeyword "break"))
(define gr-keyw (p/ gr-keyw-local
                    gr-keyw-defun
                    gr-keyw-end
                    gr-keyw-if
                    gr-keyw-elif
                    gr-keyw-else
                    gr-keyw-while
                    gr-keyw-break))
(define gr-lp (pskipw (pc #\( )))
(define gr-rp (pskipw (pc #\) )))
(define gr-at (pskipw (pc #\@)))
(define gr-delim (p/ (pc #\;) pnl))
(define gr-comma (pskipw (pc #\,)))
(define gr-mul-op (p/ (pkeysym "*") (pkeysym "/")))
(define gr-add-op (p/ (pkeysym "+") (pkeysym "-")))
(define gr-relat-op (p/ (pkeyword "is")
                        (pkeyword "isnot")
                        (pkeysym ">")
                        (pkeysym "<")
                        (pkeysym ">=")
                        (pkeysym "<=")))
(define gr-asgn-op (pkeysym "="))
(define gr-ident (pskipw (pseqn 1 (p! gr-keyw) (psc (pseq palpha (p* palphanum))))))
(define gr-label-decl (pseqn 1 gr-at gr-ident))
(define gr-arg-list (pbetween gr-lp (psependby gr-relat-expr gr-comma) gr-rp))
(define gr-if-expr (peval cons-if-expr
                          (pseq (pseq gr-keyw-if gr-relat-expr (pskipw gr-delim) gr-block)
                                (p* (pseq gr-keyw-elif gr-relat-expr (pskipw gr-delim) gr-block))
                                (popt (pseq gr-keyw-else gr-block)) gr-keyw-end)))
(define gr-while-expr (peval cons-while-expr
                             (pseq gr-keyw-while (popt gr-label-decl) gr-relat-expr (pskipw gr-delim)
                                   gr-block gr-keyw-end)))
(define gr-prim-expr (p/ gr-if-expr
                         gr-while-expr
                         (peval cons-literal gr-digit)
                         (peval cons-funcall-name (pseq gr-ident gr-arg-list))
                         (peval cons-rvalue-ref gr-ref-seq)
                         (pbetween gr-lp gr-relat-expr gr-rp)))
(define gr-ref-seq (pseq (peval cons-varref gr-ident)))
(define gr-digit (pskipw (psn (p+ pdigit))))
(define gr-mul-expr (pchainl1 gr-prim-expr gr-mul-op chain-bin-op))
(define gr-add-expr (pchainl1 gr-mul-expr gr-add-op chain-bin-op))
(define gr-relat-expr (pchainl1 gr-add-expr gr-relat-op chain-bin-op))
(define gr-asgn-expr (peval cons-asgn-expr
                            (pseq (p+ (pseqn 0 gr-ref-seq gr-asgn-op)) gr-relat-expr)))
(define gr-break-stmt (peval cons-break-stmt
                             (pseq gr-keyw-break (popt gr-label-decl)
                                   (popt gr-relat-expr))))
(define gr-stmt (p/ (pseqn 0 (pskipm pwhite) gr-break-stmt)
                    (pseqn 0 (pskipm pwhite) (p/ gr-asgn-expr gr-relat-expr))
                    (pseq (pskipm pwhite) pnothing)))
(define gr-stmt-seq (peval cons-stmt-seq (psependby gr-stmt gr-delim)))
(define gr-vdecl (p/ (pseqn 1 (pskipm pwhite) gr-keyw-local (psependby gr-ident gr-comma))
                     (pseq (pskipm pwhite) pnothing)))
(define gr-lfdef (pseqn 0 (pskipm pwhite) gr-func-def))
(define gr-decl-seq (peval cons-decl-seq (psependby (p/ gr-lfdef gr-vdecl) gr-delim)))
(define gr-block (peval cons-block (pseq gr-decl-seq gr-stmt-seq)))
(define gr-param-list (pbetween gr-lp (psependby gr-ident gr-comma) gr-rp))
(define gr-func-def (peval cons-func-def
                           (pseq gr-keyw-defun gr-ident (popt gr-label-decl)
                                 gr-param-list gr-block gr-keyw-end)))
(define gr-prog (pseqn 1 (p* pwhite) (p* (pskipwl gr-func-def)) peof))

(define (parse str)
  (let1 r (gr-prog (make-pobj str 0))
    (if (pres-result r)
        (pres-pdata r)
        (error "Parse error!!"))))

