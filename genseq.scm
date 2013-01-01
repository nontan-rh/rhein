;;
;; genseq.scm - Generic sequence library for Rhein
;;

(use gauche.record)
(use srfi-27)

(require "./typesys")

;; Internal structure for generic sequence

(define-record-type pure-sequence #t #t
  (weight) (left) (right) (value))

(define (%get-weight n)
  (if (null? n)
    0
    (~ n 'weight)))

(define (%sum-weight lft rht)
  (+ (%get-weight lft) (%get-weight rht)))

(define (%restruct-pure-sequence n lft rht)
  (when (null? n)
    (error "Tried to restruct nil"))
  (make-pure-sequence (+ (%sum-weight lft rht) 1) lft rht (~ n 'value)))

(define (%safe-left x)
  (if (null? x)
    '()
    (~ x 'left)))

(define (%safe-right x)
  (if (null? x)
    '()
    (~ x 'right)))

(define (%left-rotate n)
  (let ([n1 (%safe-left n)]
        [n2 n]
        [n3 (%safe-left (%safe-right n))]
        [n4 (%safe-right n)]
        [n5 (%safe-right (%safe-right n))])
    (%restruct-pure-sequence n4 (%restruct-pure-sequence n2 n1 n3) n5)))

(define (%right-rotate n)
  (let ([n1 (%safe-left (%safe-left n))]
        [n2 (%safe-left n)]
        [n3 (%safe-right (%safe-left n))]
        [n4 n]
        [n5 (%safe-right n)])
    (%restruct-pure-sequence n2 n1 (%restruct-pure-sequence n4 n3 n5))))

(define (pure-sequence-size node)
  (if (null? node)
    0
    (+ (%sum-weight (~ node 'left) (~ node 'right)) 1)))

(define (pure-sequence-ref node index)
  (let lp ([cnt 0] [n node])
    (let1 pos (+ (pure-sequence-size (%safe-left n)) cnt)
      (cond
        [(null? n) (error "Index out of range")]
        [(< index pos) (lp cnt (~ n 'left))]
        [(= index pos) (~ n 'value)]
        [(> index pos) (lp (+ cnt (%get-weight (~ n 'left)) 1) (~ n 'right))]))))

(define (pure-sequence-set node index value)
  (let lp ([cnt 0] [n node])
    (let1 pos (+ (pure-sequence-size (%safe-left n)) cnt)
      (cond
        [(null? n) (error "Index out of range")]
        [(< index pos) (%restruct-pure-sequence n (lp cnt (~ n 'left)) (~ n 'right))]
        [(= index pos)
         (let ([lft (%safe-left n)]
               [rht (%safe-right n)])
           (make-pure-sequence (+ 1 (%sum-weight lft rht)) lft rht value))]
        [(> index pos) (%restruct-pure-sequence
                         n (~ n 'left) (lp (+ cnt (%get-weight (~ n 'left)) 1) (~ n 'right)))]))))

(define (%insert-normal rest n value)
  (let ([next-insert (if (> (/ (+ (pure-sequence-size n) 1)) (random-real))
                       %insert-root
                       %insert-normal)]
        [left-size (pure-sequence-size (%safe-left n))])
    (cond
      [(and (zero? rest) (null? n))
       (make-pure-sequence 1 '() '() value)]
      [(<= rest left-size)
       (%restruct-pure-sequence
         n (next-insert rest (%safe-left n) value) (%safe-right n))]
      [(> rest left-size)
       (%restruct-pure-sequence
         n (%safe-left n) (next-insert (- rest left-size 1) (%safe-right n) value))]
      [(null? n) (error "Index out of range")]
      [else (d n)])))

(define (%insert-root rest n value)
  (let1 left-size (pure-sequence-size (%safe-left n))
    (cond
      [(and (zero? rest) (null? n))
       (make-pure-sequence 1 '() '() value)]
      [(<= rest left-size)
       (%right-rotate
         (%restruct-pure-sequence
           n (%insert-root rest (%safe-left n) value) (%safe-right n)))]
      [(> rest left-size)
       (%left-rotate
         (%restruct-pure-sequence
           n (%safe-left n) (%insert-root (- rest left-size 1) (%safe-right n) value)))]
      [(null? n) (error "Index out of range")]
      [else (d n)])))

(define (pure-sequence-insert node index value)
  (let1 next-insert (if (> (/ (+ (pure-sequence-size node) 1)) (random-real))
                      %insert-root
                      %insert-normal)
    (next-insert index node value)))

(define (pure-sequence-append node value)
  (pure-sequence-insert node (pure-sequence-size node) value))

(define (pure-sequence-push node value)
  (pure-sequence-insert node 0 value))

(define (string->pure-sequence lis)
  (list->pure-sequence (string->list lis)))

(define (list->pure-sequence lis)
  (fold (^(x s) (pure-sequence-append s x)) '() lis))

(define (pure-sequence-print x)
  (unless (null? x)
    (pure-sequence-print (~ x 'left))
    (print (~ x 'value))
    (pure-sequence-print (~ x 'right))))

;; Wrapper with side effect for Rhein

(define-record-type generic-sequence #t #t
  (pseq) (type-restrict))

(define (string->generic-sequence str)
  (make-generic-sequence (string->pure-sequence str) 'builtin-character))

(define (list->generic-sequence lis)
  (make-generic-sequence (list->pure-sequence str) 'builtin-any))

(define (generic-sequence-ref seq index)
  (pure-sequence-ref (~ seq 'pseq) index))

(define (generic-sequence-set! seq index value)
  (unless (rhein-type-match? value (~ seq 'type-restrict))
    (error "Type restriction unmatched"))
  (set! (~ seq 'pseq) (pure-sequence-set (~ seq 'pseq) index value)))

(define (generic-sequence-push! seq value)
  (unless (rhein-type-match? value (~ seq 'type-restrict))
    (error "Type restriction unmatched"))
  (set! (~ seq 'pseq) (pure-sequence-push (~ seq 'pseq) value)))

(define (generic-sequence-append! seq value)
  (unless (rhein-type-match? value (~ seq 'type-restrict))
    (error "Type restriction unmatched"))
  (set! (~ seq 'pseq) (pure-sequence-append (~ seq 'pseq) value)))

