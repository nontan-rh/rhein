;;
;; Rhein prelude library
;;

(define (rhein-print . objs)
  (for-each (^x (display x) (display " ")) objs)
  (newline))

