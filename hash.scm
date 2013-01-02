;;
;; hash.scm - Hash in Rhein
;;

(define-record-type rhein-hash #t #t
  (hashtable))

(define (vector->rhein-hash keyvec valuevec)
  (let1 ihash (make-hash-table 'equal?)
    (vector-for-each (^(_ k v) (hash-table-put! ihash k v)) keyvec valuevec)
    (make-rhein-hash ihash)))

(define (rhein-hash-ref hsh index)
  (hash-table-get (~ hsh 'hashtable) index))

(define (rhein-hash-set! hsh index value)
  (hash-table-put! (~ hsh 'hash-table) index value))

