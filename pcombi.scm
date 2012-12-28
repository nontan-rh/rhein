;;
;; pcombi.scm - Parser combinator
;;

(use srfi-1)
(use gauche.record)

(define-record-type parser-head #t #t
  (text) (position))

(define (clone-parser-head ph)
  (make-parser-head (~ ph 'text) (~ ph 'position)))

(define (increment-position ph delta)
  (rlet1 cl (clone-parser-head ph)
    (inc! (~ cl 'position) delta)))

(define (lineno-and-column ph)
  (let ([lineno 1]
        [column 1]
        [str (string->list (~ ph 'text))])
    (dotimes [_ (~ ph 'position)]
      (cond
        [(null? str) (error "String index overflow")]
        [(and (char=? (first str) #\nl) (not (null? (cdr str))) (char=? (second str) #\lf))
         (pop! str)
         (inc! lineno)
         (set! column 1)]
        [(or (char=? (first str) #\nl) (char=? (first str) #\lf))
         (inc! lineno)
         (set! column 1)]
        [else (inc! column)])
      (pop! str))
    (values lineno column)))

;;
;; Parser combinators
;; Returns succeed?, next-parser-head, parse-data
;;

(define (plabel lbl grm)
  (^(p) (receive [succ next data] (grm p)
          (values succ next (cons lbl data)))))

(define-syntax perror
  (syntax-rules ()
    [(_ fn grm)
     (^(p) (receive [succ next data] (grm p)
             (if succ
               (values succ next data)
               (fn p))))]))

(define-syntax perror-if
  (syntax-rules ()
    [(_ fn grm cnd)
     (^(p) (receive [succ1 next1 data1] (grm p)
             (if succ1
               (values succ1 next1 data1)
               (receive [succ2 _ _] (cnd p)
                 (if succ2
                   (fn p)
                   (values succ1 next1 data1))))))]))

(define-syntax peval
  (syntax-rules ()
    [(_ fn grm)
     (^(p) (receive [succ next data] (grm p)
             (if succ
               (values succ next (fn data))
               (values succ next data))))]))

(define-syntax pseq
  (syntax-rules ()
    [(_ seq ...)
     (^(p) (let/cc br
             (let ([buf '()]
                   [px p])
               (dolist [g (list seq ...)]
                 (receive [succ next data] (g px)
                   (unless succ
                     (br #f p '() )) 
                   (push! buf data)
                   (set! px next)))
               (values #t px (reverse buf)))))]))

(define-syntax pseqn
  (syntax-rules ()
    [(_ n seq ...)
     (let1 sq (pseq seq ...)
       (^(p) (receive [succ next data] (sq p)
               (if succ
                 (values #t next (list-ref data n))
                 (values #f p '() )))))]))

(define-syntax p/
  (syntax-rules ()
    [(_ vari ...)
     (^(p) (let/cc br
             (dolist [g (list vari ...)]
               (receive [succ next data] (g p)
                 (when succ
                   (br #t next data))))
             (values #f p '() )))]))

(define-syntax p*
  (syntax-rules ()
    [(_ grm)
     (^(p) (let/cc br
             (let ([buf '()]
                   [px p])
               (while #t
                 (receive [succ next data] (grm px)
                   (unless succ
                     (br #t px (reverse buf)))
                   (push! buf data)
                   (set! px next))))))]))

(define-syntax p+
  (syntax-rules ()
    [(_ grm)
     (let1 st (p* grm)
       (^(p) (receive [succ1 next1 data1] (grm p)
               (if succ1
                 (receive [succ2 next2 data2] (st next1)
                   (values #t next2 (cons data1 data2)))
                 (values #f p '() )))))]))

(define-syntax p?
  (syntax-rules ()
    [(_ grm)
     (^(p)
       (receive [succ _ _] (grm p)
         (values succ p '() )))]))

(define-syntax p!
  (syntax-rules ()
    [(_ grm)
     (^(p)
       (receive [succ _ _] (grm p)
         (values (not succ) p '() )))]))

(define (psatc pred?)
  (^(p)
    (if (< (~ p 'position) (string-length (~ p 'text)))
      (let1 ch (string-ref (~ p 'text) (~ p 'position))
        (if (pred? ch)
          (values #t (increment-position p 1) ch)
          (values #f p '() )))
      (values #f p '() ))))

(define (pc chset)
  (psatc (^x (char-set-contains? chset x))))

(define (pnc chset)
  (psatc (^x (not (char-set-contains? chset x)))))

(define (ps str)
  (^(p) (let* ([start (~ p 'position)]
               [end (+ start (string-length str))]
               [target (~ p 'text)])
          (if (and (<= end (string-length target)) (string=? (substring target start end) str))
            (values #t (increment-position p (- end start)) str)
            (values #f p '() )))))

(define-syntax psc
  (syntax-rules ()
    [(_ grm) (peval obj-to-string grm)]))

(define-syntax psn
  (syntax-rules ()
    [(_ grm) (peval obj-to-number grm)]))

(define (obj-to-string obj)
  (cond
   [(null? obj) ""]
   [(pair? obj) (string-append (obj-to-string (car obj)) (obj-to-string (cdr obj)))]
   [(char? obj) (string obj)]
   [(symbol? obj) (symbol->string obj)]
   [(string? obj) obj]))

(define (obj-to-number obj)
  (string->number (obj-to-string obj)))

;;
;; Parser combinator library
;;

(define-syntax popt
  (syntax-rules ()
    [(_ grm)
     (^(p) (receive [_ next data] (grm p)
             (values #t next data)))]))

(define-syntax pcount
  (syntax-rules ()
    [(_ n grm)
     (^(p) (let ([buf '()]
                 [px p])
             (let/cc br
               (dotimes [_ n]
                 (receive [succ next data] (grm px)
                   (unless succ
                     (br #f p '() ))
                   (push! buf data)
                   (set! px next))))
             (values #t px (reverse buf))))]))

(define-syntax pbetween
  (syntax-rules ()
    [(_ opn contain cls)
     (pseqn 1 opn contain cls)]))

(define-syntax %pdefine-nofail2
  (syntax-rules ()
    [(_ name combi)
     (define-syntax name
       (syntax-rules ()
         [(_ arg1 arg2)
          (let1 sb (combi arg1 arg2)
            (^(p) (receive [succ next data] (sb p)
                    (values #t next data))))]))]))

(define-syntax psepby1
  (syntax-rules ()
    [(_ grm sep)
     (let1 st (p* (pseqn 1 sep grm))
       (^(p) (receive [succ1 next1 data1] (grm p)
               (if succ1
                 (receive [succ2 next2 data2] (st next1)
                   (values #t next2 (cons data1 data2)))
                 (values #f p '() )))))]))

(%pdefine-nofail2 psepby psepby1)

(define-syntax psependby1
  (syntax-rules ()
    [(_ grm sep)
     (let1 sb (psepby1 grm sep)
       (^(p) (let/cc br
               (receive [succ1 next1 data1] (sb p)
                 (unless succ1
                   (br #f p '() ))
                 (receive [_ next2 _] (sep next1)
                   (values #t next2 data1))))))]))

(%pdefine-nofail2 psependby psependby1)

(define-syntax pmtill
  (syntax-rules ()
    [(_ grm end)
     (^(p) (let ([px p]
                 [buf '()])
             (let/cc br
               (while #t
                 (receive [succ next data] (end px)
                   (when succ
                     (br #t next (reverse (cons data buf)))))
                 (receive [succ next data] (grm px)
                   (unless succ
                     (br #f p '() ))
                   (set! px next)
                   (push! buf data))))))]))

;;
;; Grammar of characters
;;

(define (panyc p)
  (if (< (~ p 'position) (string-length (~ p 'text)))
    (values #t (increment-position p 1) (string-ref (~ p 'text) (~ p 'position)))
    (values #f p '() )))

(define peof (p! panyc))

(define pspace-character (pc #[\u0020]))
(define pholizontal-tab (pc #[\u0009]))
(define pline-feed (pc #[\u000a]))
(define pcarrige-return (pc #[\u000d]))
(define pnew-line (pc #[\u0009\u000a\u000d]))
(define pwhite (pc #[\u0020\u0009]))
(define pspace (pc #[[:space:]]))
(define palphanum (pc #[[:alnum:]]))
(define psymbol (pc #[[:punct:]]))
(define palpha (pc #[[:alpha:]]))
(define pupper (pc #[[:upper:]]))
(define plower (pc #[[:lower:]]))
(define pdigit (pc #[[:digit:]]))
(define pgraph (pc #[[:graph:]]))
(define pprint (pc #[[:print:]]))

(define pnothing (pseq))

;;
;; Typical syntax utilities
;;

(define-syntax pskipw
  (syntax-rules ()
    [(_ grm)
     (pseqn 0 grm (p* pwhite))]))

(define-syntax pskipwl
  (syntax-rules ()
    [(_ grm)
     (pseqn 0 grm (p* pspace))]))

(define-syntax pkeyword
  (syntax-rules ()
    [(_ str)
     (peval string->symbol (pskipwl (pseqn 0 (ps str) (p! palphanum))))]))

(define-syntax pkeysym
  (syntax-rules ()
    [(_ str)
     (peval string->symbol (pskipwl (pseqn 0 (ps str) (p! psymbol))))]))

