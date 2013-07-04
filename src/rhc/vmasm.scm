;;
;; vmasm.scm
;;

(use rfc.json)
(use binary.io)
(use srfi-43)

(define *class-sign* 0)
(define *func-sign* 1)
(define *var-sign* 2)

(define *int-sign* 0)
(define *char-sign* 1)
(define *symbol-sign* 2)
(define *string-sign* 3)

(define *insn-list*
  '(add sub mul div mod inc dec neg not eq ne gt lt ge le jump ifjump unlessjump call ret ranew raref 
    raset iref iset mref mset lfref lfset lvref lvset laref laset gfref gvref gvset load loadklass
    loadundef loadnull loadtrue loadfalse enclose dup pop escape break))
(define *insn-table* (alist->hash-table (map cons *insn-list* (iota (length *insn-list*))) 'eq?))

(define (main args)
  (assemble (parse-json)))

;; write-byte val
;; write-4byte val
;; write-string val
;; write-ber val
;; write-int val
;; write-ch val

; (define (write-byte val) ...)

(define (write-4byte val)
  (write-u32 val (current-output-port) 'big-endian))

(define (write-ber val)
  (write-ber-integer val))

(define (write-string val)
  (write-ber (string-length val))
  (display val))

(define (write-int val)
  (write-ber (if (negative? val) (- (ash (- val) 1) 1) (ash val 1))))

(define (write-ch val)
  (unless (= 1 (string-length val))
    (error "Not a character"))
  (write-4byte (char->integer (string-ref val 0))))

(define (assemble jasm)
  (unless (list? jasm)
    (error "top level must be an object"))
  (write-string (cdr (assoc "initializer" jasm)))
  (let1 objects (cdr (assoc "objects" jasm))
    (write-ber (vector-length objects))
    (vector-for-each (^(_ v) (assemble-obj v)) objects)))

(define (assemble-obj jasm)
  (let1 type (assoc "type" jasm)
    (unless type
      (error "No type"))
    (case (string->symbol (cdr type))
      [(class) (assemble-class jasm)]
      [(function) (assemble-func jasm)]
      [(variable) (assemble-var jasm)]
      [else (error "Unknown type")])))

(define (assemble-class jasm)
  (write-byte *class-sign*)
  (let ([name (assoc "name" jasm)]
        [parent (assoc "parent" jasm)]
        [slots (assoc "members" jasm)])
    (unless (and name parent slots (vector? (cdr slots)))
      (error "Lack of element"))
    (write-string (cdr name))
    (write-string (cdr parent))
    (write-ber (vector-length (cdr slots)))
    (vector-for-each (^(_ v)
                       (unless (string? v)
                         (error "slot name must be a string"))
                       (write-string v)) (cdr slots))))

(define (assemble-func jasm)
  (define (write-arg _ t)
    (let1 kind (string->symbol (cdr (assoc "kind" t)))
      (write-byte (case kind [(class) 0] [(instance) 1])))
    (write-string (cdr (assoc "value" t))))
  (write-byte *func-sign*)
  (let ([name (assoc "name" jasm)]
        [variable-arg (assoc "varg" jasm)]
        [arg-type (assoc "argument_type" jasm)]
        [func-size (assoc "function_size" jasm)]
        [var-size (assoc "variable_size" jasm)]
        [stack-size (assoc "stack_size" jasm)]
        [code (assoc "code" jasm)])
    (unless (and name variable-arg arg-type func-size var-size stack-size code)
      (error "Lack of element"))
    (write-string (cdr name))
    (write-byte (case (cdr variable-arg) [(true) 1] [(false) 0] [else (error "Boolean required")]))
    (write-ber (vector-length (cdr arg-type)))
    (vector-for-each write-arg (cdr arg-type))
    (write-ber (cdr func-size))
    (write-ber (cdr var-size))
    (write-ber (cdr stack-size))
    (assemble-code (cdr code))))

(define (assemble-code jasm)
  (unless (vector? jasm)
    (error "Code must be an array"))
  (receive [stripped label] (strip-label jasm)
    (let ([ctable (make-hash-table 'equal?)]
          [cindex 0])
      (dolist [i stripped]
        (case (string->symbol (vector-ref i 0))
          [(mref mset gfref gvref gvset enclose loadklass)
           (let1 literal-obj (list (cons "type" "symbol") (cons "value" (vector-ref i 1)))
             (vector-set! i 1 literal-obj)
             (unless (hash-table-exists? ctable literal-obj)
               (hash-table-put! ctable literal-obj cindex)
               (inc! cindex)))]
          [(load)
           (unless (hash-table-exists? ctable (vector-ref i 1))
             (hash-table-put! ctable (vector-ref i 1) cindex)
             (inc! cindex))]))
      (write-ber (hash-table-num-entries ctable))
      (dolist [c (map car (sort (hash-table->alist ctable) (^(l r) (< (cdr l) (cdr r)))))]
        (case (string->symbol (cdr (assoc "type" c)))
          ['int (write-byte *int-sign*) (write-int (cdr (assoc "value" c)))]
          ['char (write-byte *char-sign*) (write-ch (cdr (assoc "value" c)))]
          ['symbol (write-byte *symbol-sign*) (write-string (cdr (assoc "value" c)))]
          ['string (write-byte *string-sign*) (write-string (cdr (assoc "value" c)))]
          [else (error "Not constant")]))
      (write-ber (length stripped))
      (dolist [i stripped]
        (let* ([op (string->symbol (vector-ref i 0))]
               [code (hash-table-get *insn-table* op)])
          (case op
            [(add sub mul div mod inc dec neg not eq ne gt lt ge le ret ranew raref raset iref iset
              dup pop loadundef loadnull loadtrue loadfalse break)
             (write-4byte code)]
            [(call escape)
             (write-4byte (logior code (ash (vector-ref i 1) 8)))]
            [(jump ifjump unlessjump)
             (let1 dest (hash-table-get label (vector-ref i 1))
               (write-4byte (logior code (ash dest 8))))]
            [(mref mset gfref gvref gvset enclose load loadklass)
             (let1 name (hash-table-get ctable (vector-ref i 1))
               (write-4byte (logior code (ash name 8))))]
            [(lfref lfset lvref lvset laref laset)
             (let ([depth (vector-ref i 1)]
                   [offset (vector-ref i 2)])
               (write-4byte (logior code (ash depth 8) (ash offset 16))))]))))))

(define (strip-label jasm)
  (let ([rc 0]
        [rl (make-hash-table 'equal?)]
        [rs '()])
    (dotimes [c (vector-length jasm)]
      (let1 i (vector-ref jasm c)
        (case (string->symbol (vector-ref i 0))
          [(label) (hash-table-put! rl (vector-ref i 1) rc)]
          [else (push! rs i) (inc! rc)])))
    (values (reverse rs) rl)))

(define (assemble-var jasm)
  (write-byte *var-sign*)
  (let1 name (assoc "name" jasm)
    (unless name
      (error "Lack of element"))
    (write-string (cdr name))))

