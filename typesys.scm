;;
;; typesys.scm - Rhein type system
;;

(define-record-type rhein-type #t #t
  (body) (slotc))

(define-record-type rhein-object #t #t
  (type) (slots))

(define (make-rhein-class body)
  (let ([symbody (map string->symbol body)]
        [slotc (length body)])
    (make-rhein-type (alist->hash-table (map cons symbody (iota slotc)) 'eq?)
                     slotc)))

(define (rhein-member-ref obj mem)
  (unless (rhein-object? obj)
    (error "Not rhein object"))
  (vector-ref (~ obj 'slots) (hash-table-get (~ obj 'type 'body) mem)))

(define (rhein-member-set! obj mem value)
  (unless (rhein-object? obj)
    (error "Not rhein object"))
  (vector-set! (~ obj 'slots) (hash-table-get (~ obj 'type 'body) mem) value))

(define (rhein-new type)
  (make-rhein-object type (make-vector (~ type 'slotc) 0)))

(define (rhein-get-type value)
  (cond
    [(char? value) (get-type-by-id (*environment*) 'char)]
    [(integer? value) (get-type-by-id (*environment*) 'int)]
    [(rhein-object? value) (~ value 'type)]))

(define (rhein-type-match? value type)
  (case type
    ['builtin-any #t]
    ['builtin-nothing #f]
    [else (eq? (rhein-get-type value) type)]))

