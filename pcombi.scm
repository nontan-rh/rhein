;;
;; pcombi.scm - Parser combinator
;;

;;
;; Parsing expression grammar library
;;

(use srfi-9)

(define-record-type pres (make-pres result savest parser pdata) pres?
  (result pres-result pres-result-set!)
  (savest pres-savest pres-savest-set!)
  (parser pres-parser pres-parser-set!)
  (pdata  pres-pdata  pres-pdata-set!))

(set! (setter pres-result) pres-result-set!)
(set! (setter pres-savest) pres-savest-set!)
(set! (setter pres-parser) pres-parser-set!)
(set! (setter pres-pdata ) pres-pdata-set!)

(define-record-type pobj (make-pobj text pos) pobj?
  (text pobj-text pobj-text-set!)
  (pos  pobj-pos  pobj-pos-set!))

(set! (setter pobj-text) pobj-text-set!)
(set! (setter pobj-pos ) pobj-pos-set!)

(define (change-result pr x)
  (let ((cl (pres-clone pr)))
    (set! (pres-result cl) x)
    cl))

(define (change-savest pr x)
  (let ((cl (pres-clone pr)))
    (set! (pres-savest cl) x)
    cl))

(define (change-parser pr x)
  (let ((cl (pres-clone pr)))
    (set! (pres-parser cl) x)
    cl))

(define (change-pdata pr x)
  (let ((cl (pres-clone pr)))
    (set! (pres-pdata cl) x)
    cl))

(define (data-cons datum pr)
  (let ((cl (pres-clone pr)))
    (set! (pres-pdata cl) (cons datum (pres-pdata cl)))
    cl))

(define (pobj-clone p)
  (make-pobj (pobj-text p) (pobj-pos p)))

(define (pres-clone p)
  (make-pres (pres-result p)
             (pres-savest p)
             (pres-parser p)
             (pres-pdata p)))

(define (add-pos p delta)
  (let ((cl (pobj-clone p)))
    (set! (pobj-pos cl) (+ delta (pobj-pos cl)))
    cl))

(define (plabel label gram)
  (lambda (parser)
    (data-cons label (gram parser))))

(define-syntax peval
  (syntax-rules ()
    ((_ fn gram)
     (lambda (parser)
       (let ((r (gram parser)))
         (if (pres-result r)
             (change-pdata r (fn (pres-pdata r)))
             r))))))

(define-syntax pseq
  (syntax-rules ()
    ((_ seq ...)
     (lambda (parser)
       (let lp ((p parser) (s (list seq ...)))
         (if (null? s)
             (make-pres #t #t p '() )
             (let ((chres ((car s) p)))
               (if (not (pres-result chres))
                   chres
                   (let ((cores (lp (pres-parser chres) (cdr s))))
                     (if (not (pres-result cores))
                         (change-parser cores parser)
                         (make-pres #t
                                    (or (pres-savest chres) (pres-savest cores))
                                    (pres-parser cores)
                                    (if (pres-savest chres)
                                        (cons (pres-pdata chres) (pres-pdata cores))
                                        (pres-pdata cores)))))))))))))

(define-syntax pseqn
  (syntax-rules ()
    ((_ n seq ...)
     (let ((sq (pseq seq ...)))
       (lambda (parser)
         (let ((pr (sq parser)))
           (if (pres-result pr)
               (change-pdata pr (list-ref (pres-pdata pr) n))
               pr)))))))

(define-syntax p/
  (syntax-rules ()
    ((_ vari ...)
     (lambda (parser)
       (let lp ((s (list vari ...)))
         (if (null? s)
             (make-pres #f #t parser '() )
             (let ((res ((car s) parser)))
               (if (pres-result res)
                   res
                   (lp (cdr s))))))))))

(define-syntax p*
  (syntax-rules ()
    ((_ gram)
     (lambda (parser)
       (let lp ((p parser) (s gram))
         (let ((res (s p)))
           (if (not (pres-result res))
               (make-pres #t #t p '() )
               (data-cons (pres-pdata res) (lp (pres-parser res) s)))))))))

(define-syntax p+
  (syntax-rules ()
    ((_ gram)
     (let ((st (p* gram)))
       (lambda (parser)
         (let ((fres (gram parser)))
           (if (not (pres-result fres))
               fres
               (data-cons (pres-pdata fres) (st (pres-parser fres))))))))))

(define-syntax p?
  (syntax-rules ()
    ((_ gram)
     (lambda (parser)
       (change-parser (gram parser) parser)))))

(define-syntax p!
  (syntax-rules ()
    ((_ gram)
     (let ((que (p? gram)))
       (lambda (parser)
         (let ((res (pres-clone (que parser))))
           (change-result res (not (pres-result res)))))))))

(define-syntax pign
  (syntax-rules ()
    ((_ gram)
     (lambda (parser)
       (let ((res (gram parser)))
         (change-savest res #f))))))

(define (psatc fn)
  (lambda (parser)
    (if (< (pobj-pos parser) (string-length (pobj-text parser)))
        (let ((ch (string-ref (pobj-text parser) (pobj-pos parser))))
          (if (fn ch)
              (make-pres #t #t (add-pos parser 1) ch)
              (make-pres #f #t parser '() )))
        (make-pres #f #t parser '() ))))

(define (pc . clist)
  (let ((pred (lambda (ch) (charmatch ch clist))))
    (psatc pred)))

(define (pnc . clist)
  (let ((pred (lambda (ch) (not (charmatch ch clist)))))
    (psatc pred)))

(define (charmatch ch clist)
  (if (null? clist)
      #f
      (cond ((and (pair? (car clist))
                  (char<=? (caar clist) ch)
                  (char<=? ch (cdar clist))) #t)
            ((and (char? (car clist))
                  (char=? (car clist) ch)) #t)
            (else (charmatch ch (cdr clist))))))

(define (ps str)
  (lambda (parser)
    (let* ((start (pobj-pos parser))
           (end (+ start (string-length str)))
           (target (pobj-text parser)))
      (if (and (<= end (string-length target))
               (string=? (substring (pobj-text parser) start end) str))
          (make-pres #t #t (add-pos parser (- end start)) str)
          (make-pres #f #t parser '() )))))

(define-syntax ptrans
  (syntax-rules ()
    ((_ fn gram)
     (lambda (parser)
       (let ((res (pres-clone (gram parser))))
         (change-pdata res (fn (pres-pdata res))))))))

(define (psc gram)
  (ptrans obj-to-string gram))

(define (psn gram)
  (ptrans obj-to-number gram))

(define (obj-to-string obj)
  (cond
   ((null? obj) "")
   ((pair? obj) (string-append (obj-to-string (car obj)) (obj-to-string (cdr obj))))
   ((char? obj) (string obj))
   ((symbol? obj) (symbol->string obj))
   ((string? obj) obj)))

(define (obj-to-number obj)
  (string->number (obj-to-string obj)))

;;
;; Parser combinator library
;;

(define-syntax popt
  (syntax-rules ()
    ((_ gram)
     (lambda (parser)
       (change-result (gram parser) #t)))))

(define-syntax pcount
  (syntax-rules ()
    ((_ n gram)
     (lambda (parser)
       (let lp ((cnt n) (p parser))
         (if (= cnt 0)
             (make-pres #t #f p '() )
             (let ((pr (gram p)))
               (if (not (pres-result pr))
                   (make-pres #f #t parser '() )
                   (data-cons (pres-pdata pr) (lp (- cnt 1) (pres-parser pr)))))))))))

(define-syntax pbetween
  (syntax-rules ()
    ((_ opn contain cls)
     (pseqn 1 opn contain cls))))

(define-syntax pskipm
  (syntax-rules ()
    ((_ gram)
     (pign (p* gram)))))

(define-syntax pskipm1
  (syntax-rules ()
    ((_ gram)
     (pign (p+ gram)))))

(define-syntax pdefine-nofail2
  (syntax-rules ()
    ((_ name combi)
     (define-syntax name
       (syntax-rules ()
         ((_ arg1 arg2)
          (let ((sb (combi arg1 arg2)))
            (lambda (parser)
              (let ((r (sb parser)))
                (if (pres-result r)
                    r
                    (make-pres #t #t parser '() )))))))))))

(define-syntax pdefine-nofail3
  (syntax-rules ()
    ((_ name combi)
     (define-syntax name
       (syntax-rules ()
         ((_ arg1 arg2 arg3)
          (let ((sb (combi arg1 arg2 arg3)))
            (lambda (parser)
              (let ((r (sb parser)))
                (if (pres-result r)
                    r
                    (make-pres #t #t parser '() )))))))))))

(define-syntax psepby1
  (syntax-rules ()
    ((_ gram sep)
     (let ((st (p* (pseqn 0 (pign sep) gram))))
       (lambda (parser)
         (let ((r1 (gram parser)))
           (if (not (pres-result r1))
               (make-pres #f #t parser '() )
               (data-cons (pres-pdata r1) (st (pres-parser r1))))))))))

(pdefine-nofail2 psepby psepby1)

(define-syntax psependby1
  (syntax-rules ()
    ((_ gram sep)
     (let ((sb (psepby1 gram sep)))
       (lambda (parser)
         (let ((r1 (sb parser)))
           (if (not (pres-result r1))
               r1
               (let ((r2 (sep (pres-parser r1))))
                 (if (not (pres-result r2))
                     r1
                     (change-parser r1 (pres-parser r2)))))))))))

(pdefine-nofail2 psependby psependby1)

(define-syntax pchainl1
  (syntax-rules ()
    ((_ gram op fn)
     (let ((grcn (pseq op gram)))
       (lambda (parser)
         (let ((r1 (gram parser)))
           (if (not (pres-result r1))
               r1
               (let lp ((cont r1))
                 (let ((r2 (grcn (pres-parser cont))))
                   (if (not (pres-result r2))
                       cont
                       (lp (make-pres #t #t (pres-parser r2)
                                      (fn (pres-pdata cont) (pres-pdata r2))))))))))))))

(pdefine-nofail3 pchainl pchainl1)

(define-syntax pchainr1
  (syntax-rules ()
    ((_ gram op fn)
     (let ((grcn (pseq gram op (pign (p? gram)))))
       (lambda (parser)
         (let lp ((p parser))
           (let ((r1 (grcn p)))
             (if (pres-result r1)
                 (let ((r2 (lp (pres-parser r1))))
                   (if (not (pres-result r2))
                       r2
                       (change-pdata (fn (pres-pdata r1) (pres-pdata r2)))
                       (let ((r3 (gram p)))
                         (if (not (pres-result r3))
                             (make-pres #f #t parser '() )
                             (make-pres #t #f (pres-parser r3) (pres-pdata r3))))))))))))))

(pdefine-nofail3 pchainr pchainr1)

(define-syntax pmtill
  (syntax-rules ()
    ((_ gr end)
     (lambda (parser)
       (let lp ((p parser))
         (let ((r1 (end p)))
           (if (pres-result r1)
               (change-pdata r1 '())
               (let ((r2 (gr p)))
                 (if (not (pres-result r2))
                     (make-pres #f #t parser '() )
                     (data-cons (pres-pdata r2) (lp (pres-parser r2))))))))))))

;;
;; Grammar of charactor
;;

(define (panyc parser)
  (if (< (pobj-pos parser) (string-length (pobj-text parser)))
      (make-pres #t #t (add-pos parser 1) (string-ref (pobj-text parser) (pobj-pos parser)))
      (make-pres #f #t parser '() )))

(define peof (p! panyc))

(define psp (pc #\x20))
(define pht (pc #\x09))
(define plf (pc #\x0a))
(define pcr (pc #\x0d))
(define pnl (p/ plf pcr (pseq pcr plf)))
(define pwhite (p/ psp pht))
(define palphanum (pc '(#\a . #\z) '(#\A . #\Z) '(#\0 . #\9)))
(define psymbol (pc #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\-
                    #\= #\^ #\~ #\\ #\| #\` #\[ #\] #\{ #\}
                    #\@ #\; #\: #\+ #\* #\, #\. #\< #\> #\/
                    #\? #\_))
(define palpha (pc '(#\a . #\z) '(#\A . #\Z)))
(define pcapital (pc '(#\A . #\Z)))
(define psmall (pc '(#\a . #\z)))
(define pdigit (pc '(#\0 . #\9)))

;;
;; Syntax definition utilities
;;

(define-syntax pskipw
  (syntax-rules ()
    [(_ gr)
     (pseqn 0 gr (pskipm pwhite))]))

(define-syntax pskipwl
  (syntax-rules ()
    [(_ gr)
     (pseqn 0 gr (pskipm (p/ pwhite pnl)))]))

(define-syntax pkeyword
  (syntax-rules ()
    [(_ str)
     (peval string->symbol (pskipw (pseqn 0 (ps str) (p! palphanum))))]))

(define-syntax pkeysym
  (syntax-rules ()
    [(_ str)
     (peval string->symbol (pskipw (pseqn 0 (ps str) (p! psymbol))))]))

(define pnothing (pign (pseq)))

