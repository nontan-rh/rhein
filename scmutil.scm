;;
;; scmutil.scm - Common utility on scheme
;;
;; Written for gauche
;;
;; Requires srfi-9,23
;;

(use srfi-1)
(use srfi-9)
(use srfi-27)

;; Increment
(define (inc n)
  (+ n 1))

;; Conversion to symbol
(define (char->symbol c)
  (list->symbol (list c)))

(define (list->symbol l)
  (string->symbol (list->string l)))

(define (some->symbol s)
  (cond
   ((pair? s) (list->symbol s))
   ((char? s) (char->symbol s))
   ((string? s) (string->symbol s))
   ((symbol? s) s)
   (else (error "some->symbol: could not convert to symbol"))))

;; File utility
(define (read-all-file port)
  (let ([buf '()])
    (let lp ()
      (let ([ch (read-char port)])
        (if (eof-object? ch)
            (list->string (reverse buf))
            (begin (set! buf (cons ch buf)) (lp)))))))

;; Code consing
(define-record-type kodekons #t #t (kar) (kdr))

(define kkons make-kodekons)

(define (klist . lst)
  (cond
   [(null? lst) 'kknil]
   [(eq? (car lst) 'kknil) (apply klist (cdr lst))]
   [else (kkons (car lst) (apply klist (cdr lst)))]))

(define (list->klist lst)
  (fold-right kkons 'kknil lst))

(define (kkdebug kk)
  (display "kkdebug :")
  (if (kodekons? kk)
      (display (kodekons-flatten kk))
      (display kk))
  (newline)
  (flush)
  kk)

(define (kodekons-flatten x)
  (if (eq? x 'kknil)
      '()
      (let* ([start (cons '() '())]
             [head start])
        (letrec ([kk-flat! (^x
                             (if (kodekons? (kodekons-kar x))
                                 (kk-flat! (kodekons-kar x))
                                 (begin (set-cdr! head (cons (kodekons-kar x) '())) (set! head (cdr head))))
                             (cond
                              [(eq? 'kknil (kodekons-kdr x))]
                              [(kodekons? (kodekons-kdr x))
                               (kk-flat! (kodekons-kdr x))]
                              [else
                               (set-cdr! head (cons (kodekons-kdr x) '()))
                               (set! head (cdr head))]))])
          (kk-flat! x)
          (cdr start)))))

;; Conditional apply
(define-syntax if-apply
  (syntax-rules ()
    [(_ cnd fn x)
     (let1 res x
       (if cnd (fn res) res))]))

;; Procedure with prologue & epilogue
(define-syntax proc/pe
  (syntax-rules ()
    [(_ pro main epi) (begin pro (begin0 main epi))]))

;; Pure map
(define-record-type pure-node #t #t (key) (value) (nums) (left) (right))
(define-record-type pure-map #t #t (eqpred) (gtpred) (node))

(define (make-num-pure-map) (make-pure-map = > '() ))
(define (make-str-pure-map) (make-pure-map string=? string>? '() ))

(define (pure-node-nums-s n)
  (if (null? n)
      0
      (pure-node-nums n)))

(define (restruct-pure-node n left right)
  (make-pure-node (pure-node-key n) (pure-node-value n)
                  (+ (pure-node-nums-s left) (pure-node-nums-s right) 1)
                  left right))

(define (pure-node-value-set n v)
  (make-pure-node (pure-node-key n) v
                  (pure-node-nums-s n)
                  (pure-node-left n) (pure-node-right n)))

(define (pure-node-left-rotate n)
  (let ([n1 (pure-node-left n)]
        [n2 n]
        [n3 (pure-node-left (pure-node-right n))]
        [n4 (pure-node-right n)]
        [n5 (pure-node-right (pure-node-right n))])
    (restruct-pure-node n4 (restruct-pure-node n2 n1 n3) n5)))

(define (pure-node-right-rotate n)
  (let ([n1 (pure-node-left (pure-node-left n))]
        [n2 (pure-node-left n)]
        [n3 (pure-node-right (pure-node-left n))]
        [n4 n]
        [n5 (pure-node-right n)])
    (restruct-pure-node n2 n1 (restruct-pure-node n4 n3 n5))))

(define (pure-map-find m k)
  (let ([n (pure-map-node m)]
        [meq? (pure-map-eqpred m)]
        [mgt? (pure-map-gtpred m)])
    (cond
     [(null? n) #f]
     [(meq? k (pure-node-key n)) n]
     [(mgt? (pure-node-key n) k) (pure-map-find (pure-node-right n) k)]
     [else (pure-map-find (pure-node-left n) k)])))

(define (pure-map-node-set m n)
  (make-pure-map (pure-map-eqpred m) (pure-map-gtpred m) n))

(define (pure-map-set m k v)
  (let ([meq? (pure-map-eqpred m)]
        [mgt? (pure-map-gtpred m)])
    (letrec ([insert-normal
              (^n
                (let1 ninsert (if (> (/ (inc (pure-node-nums-s n))) (random-real))
                                  insert-root
                                  insert-normal)
                  (cond
                   [(null? n) (make-pure-node k v 1 '() '())]
                   [(meq? k (pure-node-key n)) (pure-node-value-set n v)]
                   [(mgt? k (pure-node-key n))
                    (restruct-pure-node n (pure-node-left n) (ninsert (pure-node-right n)))]
                   [else
                    (restruct-pure-node n (ninsert (pure-node-left n)) (pure-node-right n))])))]
             [insert-root
              (^n
                (cond
                 [(null? n) (make-pure-node k v 1 '() '())]
                 [(meq? k (pure-node-key n)) (pure-node-value-set n v)]
                 [(mgt? k (pure-node-key n))
                  (pure-node-left-rotate
                   (restruct-pure-node n (pure-node-left n) (insert-root (pure-node-right n))))]
                 [else
                  (pure-node-right-rotate
                   (restruct-pure-node n (insert-root (pure-node-left n)) (pure-node-right n)))]))])
      (pure-map-node-set m (insert-normal (pure-map-node m))))))

(define (alist->pure-map alist)
  (fold (^(a m) (pure-map-set m (car a) (cdr a))) (make-num-pure-map) alist))

(define (pure-node-merge l r)
  (cond
   [(or (null? l) (null? r)) '()]
   [(< (random-real) (/ (pure-node-nums-s l) (+ (pure-node-nums-s l) (pure-node-nums-s r))))
    (restruct-pure-node l (pure-node-left l) (pure-node-merge (pure-node-right l) r))]
   [else
    (restruct-pure-node r (pure-node-merge l (pure-node-left r)) (pure-node-right r))]))

(define (pure-map-delete m k)
  (let ([meq? (pure-map-eqpred m)]
        [mgt? (pure-map-gtpred m)])
    (let lp ([n (pure-map-node m)])
      (cond
       [(null? n) #f]
       [(meq? k (pure-node-key n))
        (pure-node-merge (pure-node-left n) (pure-node-right n))]
       [(mgt? k (pure-node-key n))
        (restruct-pure-node n (pure-node-left n) (lp (pure-node-right n)))]
       [else
        (restruct-pure-node n (lp (pure-node-left n)) (pure-node-right n))]))))

;; Pritty printer (by BI)

(define (pretty-print-sexp s)
  (define (do-indent level)
    (dotimes (_ level) (write-char #\space)))
  (define (pp-parenl)
    (write-char #\())
  (define (pp-parenr)
    (write-char #\)))
  (define (pp-atom e prefix)
    (when prefix (write-char #\space))
    (write e))
  (define (pp-list s level prefix)
    (and prefix (do-indent level))
    (pp-parenl)
    (let loop ((s s)
               (prefix #f))
      (if (null? s)
          (pp-parenr)
          (let1 e (car s)
            (if (list? e)
                (begin (and prefix (newline))
                       (pp-list e (+ level 1) prefix))
                (pp-atom e prefix))
            (loop (cdr s) #t)))))
  (if (list? s)
      (pp-list s 0 #f)
      (write s))
  (newline))
