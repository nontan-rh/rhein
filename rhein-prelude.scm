;;
;; Rhein prelude library
;;

(require "./genseq")

(define (rhein-print . objs)
  (for-each (^x (display x) (display " ")) objs)
  (newline))

(define (rhein-append seq . objs)
  (set! (~ seq 'pseq) (fold (^(x s) (pure-sequence-append s x)) (~ seq 'pseq) objs)))

(define (rhein-push seq . objs)
  (set! (~ seq 'pseq) (fold (^(x s) (pure-sequence-push s x)) (~ seq 'pseq) objs)))

(define (rhein-copy seq . objs)
  (make-generic-sequence (~ seq 'pseq)))

