;;
;; tinypeg.scm - Naive PEG parser combinator library
;;

(define-module tinypeg
  (use gauche.parameter)
  (use srfi-14)
  (export-all))

(select-module tinypeg)

;; Grammar objects

(define-class <grammar> () ())

(define-class <grammar-literal-string> (<grammar>)
  ((literal-list :init-keyword :literal-list)
   (literal-string :init-keyword :literal-string)))

(define-class <grammar-character-class> (<grammar>)
  ((character-class :init-keyword :character-class)))

(define-class <grammar-optional> (<grammar>)
  ((contains :init-keyword :contains)))

(define-class <grammar-more> (<grammar>)
  ((repeat-num :init-keyword :repeat-num)
   (contains :init-keyword :contains)))

(define-class <grammar-and> (<grammar>)
  ((contains :init-keyword :contains)))

(define-class <grammar-not> (<grammar>)
  ((contains :init-keyword :contains)))

(define-class <grammar-sequence> (<grammar>)
  ((sequence :init-keyword :sequence)))

(define-class <grammar-choice> (<grammar>)
  ((choice :init-keyword :choice)))

(define-class <grammar-action> (<grammar>)
  ((contains :init-keyword :contains)
   (action :init-keyword :action)))

(define-class <grammar-any> (<grammar>)
  ())

(define-class <grammar-memoize> (<grammar>)
  ((contains :init-keyword :contains)))

;; Do parse naively

(define *current-grammar* (make-parameter #f))
(define *memoization-table* (make-parameter #f))

(define-method parse-peg ((g <grammar-literal-string>) head)
  (let lp ([match-rest (~ g 'literal-list)] [target-rest head])
    (cond
      [(null? match-rest)
       (values #f (~ g 'literal-string) target-rest)]
      [(null? target-rest)
       (values 'caught-eof (undefined) target-rest)]
      [(not (eq? (car match-rest) (car target-rest)))
       (values 'mismatch (undefined) target-rest)]
      [else
       (lp (cdr match-rest) (cdr target-rest))])))

(define-method parse-peg ((g <grammar-character-class>) head)
  (cond
    [(null? head)
     (values 'caught-eof (undefined) head)]
    [(char-set-contains? (~ g 'character-class) (car head))
     (values #f (car head) (cdr head))]
    [else
     (values 'mismatch (undefined) head)]))

(define-method parse-peg ((g <grammar-optional>) head)
  (receive [e r h] (parse-peg (~ g 'contains) head)
    (if e
      (values #f '() head)
      (values #f r h))))

(define-method parse-peg ((g <grammar-more>) head)
  (let lp ([rest-num (~ g 'repeat-num)] [result '()] [h head])
    (receive [e r nh] (parse-peg (~ g 'contains) h)
      (cond
        [(and (<= rest-num 0) e)
         (values #f (reverse result) h)]
        [e
         (values e (undefined) head)]
        [else
         (lp (- rest-num 1) (cons r result) nh)]))))

(define-method parse-peg ((g <grammar-and>) head)
  (receive [e r _] (parse-peg (~ g 'contains) head)
    (values e r head)))

(define-method parse-peg ((g <grammar-not>) head)
  (receive [e r _] (parse-peg (~ g 'contains) head)
    (values (not e) r head)))

(define-method parse-peg ((g <grammar-sequence>) head)
  (let lp ([match-rest (~ g 'sequence)] [result '()] [target-rest head])
    (if (null? match-rest)
      (values #f (reverse result) target-rest)
      (receive [e r h] (parse-peg (car match-rest) target-rest)
        (if e
          (values e r h)
          (lp (cdr match-rest) (cons r result) h))))))

(define-method parse-peg ((g <grammar-choice>) head)
  (let lp ([match-rest (~ g 'choice)])
    (if (null? match-rest)
      (values 'mismatch (undefined) head)
      (receive [e r h] (parse-peg (car match-rest) head)
        (if (not e)
          (values #f r h)
          (lp (cdr match-rest)))))))

(define-method parse-peg ((g <grammar-action>) head)
  (receive [e r h] (parse-peg (~ g 'contains) head)
    (if e
      (values e r h)
      (values #f (apply (~ g 'action) r) h))))

(define-method parse-peg ((g <grammar-any>) head)
  (if (null? head)
    (values 'caught-eof (undefined) head)
    (values #f (car head) (cdr head))))

(define-method parse-peg ((g <symbol>) head)
  (let1 contains (hash-table-get (*current-grammar*) g #f)
    (unless contains
      (error "No such grammar" g))
    (parse-peg contains head)))

(define-method parse-peg ((g <grammar-memoize>) head)
  (unless (hash-table-exists? (*memoization-table*) g)
    (hash-table-put! (*memoization-table*) g (make-hash-table 'eq?)))
  (let1 ht (hash-table-get (*memoization-table*) g)
    (unless (hash-table-exists? ht head)
      (hash-table-put! ht head
                       (call-with-values (lambda () (parse-peg (~ g 'contains) head)) list)))
    (apply values (hash-table-get ht head))))

;; Parser defining utilities

(define-syntax define-parser
  (syntax-rules ()
    [(_ name grammar)
     (define name (make-grammar grammar))]))

(define (make-grammar grammar)
  (alist->hash-table grammar 'eq?))

(define ($s str)
  (make <grammar-literal-string> :literal-string str
                                 :literal-list (string->list str)))

(define ($c cset)
  (make <grammar-character-class> :character-class (string->char-set cset)))

(define ($? gram)
  (make <grammar-optional> :contains gram))

(define ($+ n gram)
  (make <grammar-more> :repeat-num n :contains gram))

(define ($* gram)
  ($+ 0 gram))

(define ($& gram)
  (make <grammar-and> :contains gram))

(define ($! gram)
  (make <grammar-not> :contains gram))

(define ($seq . rest)
  (make <grammar-sequence> :sequence rest))

(define ($/ . rest)
  (make <grammar-choice> :choice rest))

(define ($action action gram)
  (make <grammar-action> :action action :contains gram))

(define $. (make <grammar-any>))

(define-syntax $do-aux
  (syntax-rules (<-)
    [(_ (bind-list ...) (parser-list ...) () code ...)
     ($action (lambda (bind-list ...) code ...) ($seq parser-list ...))]
    [(_ (bind-list ...) (parser-list ...) ((bind <- parser) rest ...) code ...)
     ($do-aux (bind-list ... bind) (parser-list ... parser) (rest ...) code ...)]
    [(_ (bind-list ...) (parser-list ...) (parser rest ...) code ...)
     ($do-aux (bind-list ... _) (parser-list ... parser) (rest ...) code ...)]))

(define-syntax $do
  (syntax-rules ()
    [(_ (bind-list ...) code ...)
     ($do-aux () () (bind-list ...) code ...)]))

;; Combinators

(define ($sep-by body sep)
  ($/ ($do ([f <- body]
            [c <- ($* ($seq sep body))])
        (cons f (map cadr c)))
      ($seq)))

(define ($sep-by-1 body sep)
  ($do ([f <- body]
        [c <- ($* ($seq sep body))])
    (cons f (map cadr c))))

(define ($sep-end-by body sep)
  ($/ ($do ([f <- body]
            [c <- ($* ($seq sep body))]
            ($? sep))
        (cons f (map cadr c)))
      ($seq)))

(define ($sep-end-by-1 body sep)
  ($do ([f <- body]
        [c <- ($* ($seq sep body))]
        ($? sep))
    (cons f (map cadr c))))

;; proc left op right

(define ($chain-left operand op proc)
  ($do ([f <- operand]
        [c <- ($* ($seq op operand))])
    (fold-left proc f (map car c) (map cadr c))))

(define ($chain-right operand op proc)
  ($do ([f <- operand]
        [c <- ($* ($seq op operand))])
    (fold-left proc f (reverse (map car c)) (reverse (map cadr c)))))

;; Memoization

(define ($memoize gram)
  (make <grammar-memoize> :contains gram))

;; Parser driver

(define-method parse-string ((p <hash-table>) (entry <symbol>) (s <string>))
  (parameterize ([*current-grammar* p]
                 [*memoization-table* (make-hash-table 'eq?)])
    (parse-peg entry (string->list s))))

