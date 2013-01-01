;;
;; Rhein prelude library
;;

(require "./genseq")

(define (rhein-print . objs)
  (for-each (^x (display x) (display " ")) objs)
  (newline))

(define (rhein-append seq . objs)
  (fold (^(x s) (generic-sequence-append! s x)) seq objs))

(define (rhein-push seq . objs)
  (fold (^(x s) (generic-sequence-push! s x)) seq objs))

(define (rhein-copy seq . objs)
  (make-generic-sequence (~ seq 'pseq)))

