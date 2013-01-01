;;
;; Rhein prelude library
;;

(require "./genseq")
(require "./typesys")

(define (rhein-print . objs)
  (define (print-obj obj)
    (if (and (generic-sequence? obj) (generic-sequence-string? obj))
      (generic-sequence-string-print obj)
      (display obj)))
  (if (not (null? objs))
    (print-obj (car objs))
    (for-each (^x (display " ") (print-obj x)) (cdr objs)))
  (newline))

(define (rhein-append seq . objs)
  (fold (^(x s) (generic-sequence-append! s x)) seq objs))

(define (rhein-push seq . objs)
  (fold (^(x s) (generic-sequence-push! s x)) seq objs))

(define (rhein-copy seq . objs)
  (make-generic-sequence (~ seq 'pseq)))

