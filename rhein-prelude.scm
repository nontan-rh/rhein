;;
;; Rhein prelude library
;;

(require "./genseq")
(require "./typesys")
(require "./hash")

(define (rhein-print . objs)
  (define (print-obj obj)
    (if (and (rhein-array? obj) (rhein-array-string? obj))
      (rhein-array-string-print obj)
      (display obj)))
  (if (not (null? objs))
    (print-obj (car objs))
    (for-each (^x (display " ") (print-obj x)) (cdr objs)))
  (newline))

(define (rhein-append seq . objs)
  (fold (^(x s) (rhein-array-append! s x)) seq objs))

(define (rhein-push seq . objs)
  (fold (^(x s) (rhein-array-push! s x)) seq objs))

(define (rhein-copy seq . objs)
  (make-rhein-array (~ seq 'pseq)))

(define (rhein-literal type . objs)
  (case type
    ['builtin-hash (vector->rhein-hash (first objs) (second objs))]
    ['builtin-array (vector->rhein-array (first objs))]))

(define (rhein-ref obj index)
  (cond
    [(rhein-type-match? obj 'builtin-array) (rhein-array-ref obj index)]
    [(rhein-type-match? obj 'builtin-hash) (rhein-hash-ref obj index)]
    [else (error "No reference")]))

(define (rhein-set! obj index value)
  (cond
    [(rhein-type-match? obj 'builtin-array) (rhein-array-set! obj index value)]
    [(rhein-type-match? obj 'builtin-hash) (rhein-hash-set! obj index value)]
    [else (error "No reference")]))

