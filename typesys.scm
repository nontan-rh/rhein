;;
;; typesys.scm - Rhein type system
;;

(define (type-match? value type)
  (case type
    ['everything #t]
    ['character (char? value)]
    ['integer (integer? value)]))

