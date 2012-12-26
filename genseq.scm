;;
;; genseq.scm - Generic sequence library for Rhein
;;

(use gauche.record)
(use srfi-27)

(define-record-type generic-sequence #t #t
  (weight) (left) (right) (value))

(define (%get-weight n)
  (if (null? n)
    0
    (~ n 'weight)))

(define (%sum-weight lft rht)
  (+ (%get-weight lft) (%get-weight rht)))

(define (%restruct-generic-sequence n lft rht)
  (when (null? n)
    (error "Tried to restruct nil"))
  (make-generic-sequence (+ (%sum-weight lft rht) 1) lft rht (~ n 'value)))

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
    (%restruct-generic-sequence n4 (%restruct-generic-sequence n2 n1 n3) n5)))

(define (%right-rotate n)
  (let ([n1 (%safe-left (%safe-left n))]
        [n2 (%safe-left n)]
        [n3 (%safe-right (%safe-left n))]
        [n4 n]
        [n5 (%safe-right n)])
    (%restruct-generic-sequence n2 n1 (%restruct-generic-sequence n4 n3 n5))))

(define (generic-sequence-size node)
  (if (null? node)
    0
    (+ (%sum-weight (~ node 'left) (~ node 'right)) 1)))

(define (generic-sequence-refer node index)
  (let lp ([cnt 0] [n node])
    (let1 pos (+ (generic-sequence-size (%safe-left n)) cnt)
      (cond
        [(null? n) (error "Index out of range")]
        [(< index pos) (lp cnt (~ n 'left))]
        [(= index pos) (~ n 'value)]
        [(> index pos) (lp (+ cnt (%get-weight (~ n 'left)) 1) (~ n 'right))]))))

(define (generic-sequence-set node index value)
  (let lp ([cnt 0] [n node])
    (let1 pos (+ (generic-sequence-size (~ n 'left)) cnt)
      (cond
        [(null? n) (error "Index out of range")]
        [(< index pos) (%restruct-generic-sequence n (lp cnt (~ n 'left)) (~ n 'right))]
        [(= index pos) (make-generic-sequence 1 '() '() value)]
        [(> index pos) (%restruct-generic-sequence
                         n (~ n 'left) (lp (+ cnt (%get-weight (~ n 'left)) 1) (~ n 'right)))]))))

(define (%insert-normal rest n value)
  (let ([next-insert (if (> (/ (+ (generic-sequence-size n) 1)) (random-real))
                       %insert-root
                       %insert-normal)]
        [left-size (generic-sequence-size (%safe-left n))])
    (cond
      [(and (zero? rest) (null? n))
       (make-generic-sequence 1 '() '() value)]
      [(<= rest left-size)
       (%restruct-generic-sequence
         n (next-insert rest (%safe-left n) value) (%safe-right n))]
      [(> rest left-size)
       (%restruct-generic-sequence
         n (%safe-left n) (next-insert (- rest left-size 1) (%safe-right n) value))]
      [(null? n) (error "Index out of range")]
      [else (d n)])))

(define (%insert-root rest n value)
  (let1 left-size (generic-sequence-size (%safe-left n))
    (cond
      [(and (zero? rest) (null? n))
       (make-generic-sequence 1 '() '() value)]
      [(<= rest left-size)
       (%right-rotate
         (%restruct-generic-sequence
           n (%insert-root rest (%safe-left n) value) (%safe-right n)))]
      [(> rest left-size)
       (%left-rotate
         (%restruct-generic-sequence
           n (%safe-left n) (%insert-root (- rest left-size 1) (%safe-right n) value)))]
      [(null? n) (error "Index out of range")]
      [else (d n)])))

(define (generic-sequence-insert node index value)
  (let1 next-insert (if (> (/ (+ (generic-sequence-size node) 1)) (random-real))
                      %insert-root
                      %insert-normal)
    (next-insert index node value)))

(define (generic-sequence-append node value)
  (generic-sequence-insert node (generic-sequence-size node) value))

(define (generic-sequence-push node value)
  (generic-sequence-insert node 0 value))

(define (list->generic-sequence lis)
  (fold (^(x s) (generic-sequence-append s x)) '() lis))

(define (generic-sequence-print x)
  (unless (null? x)
    (generic-sequence-print (~ x 'left))
    (print (~ x 'value))
    (generic-sequence-print (~ x 'right))))

