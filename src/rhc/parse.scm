;;
;; parse.scm - Parsing rhein source code
;;

(define-module parse
  (use tinypeg)
  (use ast)
  (use gauche.parameter)
  (export-all))

(select-module parse)

(define (combine-postfix p b)
  (cond
    [(is-a? p <rh-named-function-call>)
     (push! (~ p 'arguments) b)
     p]
    [(is-a? p <rh-nameless-function-call>)
     (set! (~ p 'function) b)
     p]
    [(or (is-a? p <rh-index-reference>) (is-a? p <rh-member-reference>))
     (set! (~ p 'base-expression) b)
     p]))

(define ($post-concat str)
  ($do (($s str)
        'implicit-delimiter)
    (string->symbol str)))

(define ($skip-space str)
  ($do (($s str)
        'space)
    (string->symbol str)))

(define lower-list "abcdefghijklmnopqrstuvwxyz")
(define upper-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define digit-start "123456789")
(define digit-list "0123456789")
(define hex-list (string-append digit-list "abcdefABCDEF"))
(define ident-start (string-append lower-list upper-list "_"))
(define ident-cont (string-append ident-start digit-list))
(define printable-list (string-append lower-list
                                      upper-list
                                      digit-list
                                      "!\"#$%&'()~-=^~\\|@`[{;+:*]},<.>/?_"))

(define ($keyword-concat str)
  ($do (($s str)
        ($! ($c ident-cont))
        'implicit-delimiter)
    (string->symbol str)))

(define ($keyword-space str)
  ($do (($s str)
        ($! ($c ident-cont))
        'space)
    (string->symbol str)))

(define *operator-list* (make-parameter #f))
(define *unary-operator-list* (make-parameter #f))
(define *binary-operator-list* (make-parameter #f))

(define-class <unary-operator> ()
  ((syntax :init-keyword :syntax)))

(define-class <binary-operator> ()
  ((syntax :init-keyword :syntax)))

(define default-operators
  (list
    (make <unary-operator> :syntax '(NOT NEG))
    (make <binary-operator> :syntax '(MUL DIV))
    (make <binary-operator> :syntax '(ADD SUB))
    (make <binary-operator> :syntax '(EQ NE GE GT LE LT))))

(define-method gen-syntax (child (ops <unary-operator>))
  ($do ([o <- ($* (apply $/ (~ ops 'syntax)))]
        [e <- child])
    (fold-right (cut make <rh-unary-expression>
                     :operator <>
                     :expression <>) e o)))

(define-method gen-syntax (child (ops <binary-operator>))
  ($chain-left child (apply $/ (~ ops 'syntax))
               (cut make <rh-binary-expression>
                    :left-expression <>
                    :operator <>
                    :right-expression <>)))

(define (gen-unary-list lst)
  (let* ([unary-layers (filter (cut is-a? <> <unary-operator>) lst)]
         [unary-ops (apply append (map (cut ~ <> 'syntax) unary-layers))])
    (apply $/ unary-ops)))

(define (gen-binary-list lst)
  (let* ([binary-layers (filter (cut is-a? <> <binary-operator>) lst)]
         [binary-ops (apply append (map (cut ~ <> 'syntax) binary-layers))])
    (apply $/ binary-ops)))

(define (set-operator-list-grammar! lst)
  (*operator-list* (fold-left gen-syntax 'postfix-expression lst))
  (*unary-operator-list* (gen-unary-list lst))
  (*binary-operator-list* (gen-binary-list lst)))

(define (parse-expression head)
  (parse-peg (*operator-list*) head))

(define (parse-unary-op head)
  (parse-peg (*unary-operator-list*) head))

(define (parse-binary-op head)
  (parse-peg (*binary-operator-list*) head))

(set-operator-list-grammar! default-operators)

(define-parser rhein-parser
  `((start . ,($seq 'program ($! $.)))
    (implicit-delimiter . ,($* ($c "\n\r \t")))
    (delimiter . ,($seq ($* ($c " \t")) ($c ";\n\r") ($* ($c "\n\r \t;"))))
    (space . ,($/ ($seq ($* ($c " \t"))
                        ($? 'implicit-delimiter)
                        ($& 'pre-concatenative-token))
                  ($* ($c " \t"))))
    (LPAREN . ,($post-concat "("))
    (RPAREN . ,($skip-space ")"))
    (LBRACE . ,($post-concat "{"))
    (RBRACE . ,($skip-space "}"))
    (LBRACKET . ,($post-concat "["))
    (RBRACKET . ,($skip-space "]"))
    (COLON . ,($post-concat ":"))
    (DQUOTE . ,($s "\""))
    (QUESTION . ,($s "?"))
    (HAT . ,($s "^"))
    (TILDE . ,($s "~"))
    (DOT . ,($post-concat "."))
    (COMMA . ,($post-concat ","))
    (ARROW . ,($post-concat "->"))
    (MUL . ,($post-concat "*"))
    (DIV . ,($post-concat "/"))
    (ADD . ,($post-concat "+"))
    (SUB . ,($post-concat "-"))
    (EQ . ,($keyword-concat "eq"))
    (NE . ,($keyword-concat "ne"))
    (GT . ,($post-concat ">"))
    (GE . ,($post-concat ">="))
    (LT . ,($post-concat "<"))
    (LE . ,($post-concat "<="))
    (NEG . ,($keyword-concat "neg"))
    (NOT . ,($keyword-concat "not"))
    (WHILE . ,($keyword-concat "while"))
    (IF . ,($keyword-concat "if"))
    (ELIF . ,($keyword-concat "elif"))
    (ELSE . ,($keyword-concat "else"))
    (AND . ,($keyword-concat "and"))
    (OR . ,($keyword-concat "or"))
    (BREAK . ,($keyword-concat "break"))
    (ASSIGN . ,($post-concat "="))
    (IDENT . ,($do (($! 'disabled-keyword)
                    [f <- ($c ident-start)]
                    [c <- ($* ($c ident-cont))]
                    'space)
                (string->symbol (list->string (cons f c)))))
    (DIGIT . ,($/ ($do ([f <- ($c digit-start)]
                        [c <- ($* ($c digit-list))]
                        'space)
                    (string->number (list->string (cons f c))))
                  ($do (($s "0")
                        'space)
                    0)))
    (TRUE . ,($keyword-space "true"))
    (FALSE . ,($keyword-space "false"))
    (NIL . ,($memoize ($keyword-space "nil")))
    (LOCAL . ,($keyword-concat "local"))
    (DEF . ,($keyword-concat "def"))
    (REST . ,($keyword-concat "&rest"))
    (CLASS . ,($keyword-concat "class"))
    (GLOBAL . ,($keyword-concat "global"))
    (pre-concatenative-token . ,($memoize ($/ 'RPAREN 'RBRACE 'RBRACKET
                                              'COLON 'DOT 'COMMA 'MUL 'DIV
                                              'ADD 'SUB 'EQ 'NE 'GT 'GE 'LT
                                              'LE 'ASSIGN 'ARROW)))
    (unary-ops . ,($dynamic parse-unary-op))
    (binary-ops . ,($dynamic parse-binary-op))
    (disabled-keyword . ,($memoize ($/ 'NEG 'NOT 'EQ 'NE 'IF 'ELIF 'ELSE
                                       'WHILE 'AND 'OR 'BREAK 'TRUE 'FALSE
                                       'NIL 'LOCAL 'DEF 'CLASS 'GLOBAL)))
    (parameter-opt . ,($/ ($do ('REST
                                [id <- 'IDENT])
                            (make <rh-parameter> :id id :attr 'rest))
                          ($do ([id <- 'IDENT])
                            (make <rh-parameter> :id id :attr '() ))))
    (parameter-type . ,($/ ($do ('LT
                                 [type <- 'IDENT])
                             (list 'instance type))
                           ($do ('LE
                                 [type <- 'IDENT])
                             (list 'class type))))
    (parameter . ,($do ([p <- 'parameter-opt]
                        [type <- ($? 'parameter-type)])
                    (set! (~ p 'type) (if (null? type)
                                        (list 'instance 'any)
                                        type))
                    p))
    (parameter-list . ,($sep-end-by 'parameter 'COMMA))
    (named-function-argument-list . ,($do ('LPAREN
                                           [a <- ($sep-end-by 'expression 'COMMA)]
                                           'RPAREN)
                                       a))
    (if-expression . ,($do ([if-clause <- ($seq 'IF 'implicit-delimiter
                                                'expression 'implicit-delimiter
                                                'block)]
                            [elif-clauses <- ($* ($seq 'implicit-delimiter
                                                       'ELIF 'implicit-delimiter
                                                       'expression 'implicit-delimiter
                                                       'block))]
                            [else-clause <- ($? ($seq 'implicit-delimiter
                                                      'ELSE 'implicit-delimiter
                                                      'block))])
                        (let ([else-block (if (null? else-clause) '() (list-ref else-clause 3))]
                              [conditional (cons (cons '() if-clause) elif-clauses)])
                          (define (extract x)
                            (make <rh-conditional-clause>
                                  :condition-expression (list-ref x 3)
                                  :code (list-ref x 5)))
                          (make <rh-if-expression>
                                :conditional-clauses (map extract conditional)
                                :else-clause else-block))))
    (while-expression . ,($do ('WHILE 'implicit-delimiter
                               [c <- 'expression] 'implicit-delimiter
                               [b <- 'block])
                           (make <rh-while-expression> :condition-expression c :code b)))
    (and-expression . ,($do ('AND 'implicit-delimiter
                             [b <- 'block])
                         (make <rh-and-expression> :code (~ b 'code))))
    (or-expression . ,($do ('OR 'implicit-delimiter
                            [b <- 'block])
                        (make <rh-or-expression> :code (~ b 'code))))
    (nameless-function-literal . ,($do ('HAT
                                        [l <- 'lambda-block])
                                    (make <rh-function-literal>
                                          :parameters (list-ref l 0)
                                          :code (list-ref l 1))))
    (numeric-literal . ,($do ([n <- 'DIGIT])
                          (make <rh-integer-literal> :value n)))
    (STRING-CHARACTER . ,($/ ($do (($s "\\x")
                                   [h <- ($c hex-list)]
                                   [l <- ($c hex-list)])
                               (integer->char (string->number (string h l) 16)))
                             ($do (($s "\\")
                                   [c <- $.])
                               c)
                             ($do (($! ($s "\""))
                                   [c <- $.])
                               c)))
    (LITERAL-CHARACTER . ,($/ ($do (($s "\\x")
                                    [h <- ($c hex-list)]
                                    [l <- ($c hex-list)])
                                (integer->char (string->number (string h l) 16)))
                              ($do (($s "\\")
                                    [c <- $.])
                                c)
                              ($c printable-list)))
    (symbol-literal . ,($do (($s ":")
                             [s <- 'IDENT])
                         (make <rh-symbol-literal> :value (symbol->string s))))
    (string-literal . ,($do ('DQUOTE
                             [s <- ($* 'STRING-CHARACTER)]
                             'DQUOTE
                             'space)
                         (make <rh-string-literal> :value (list->string s))))
    (character-literal . ,($do ('QUESTION
                                [c <- 'LITERAL-CHARACTER]
                                'space)
                            (make <rh-character-literal> :value c)))
    (array-literal . ,($do ('LBRACKET
                            [e <- ($sep-end-by 'expression 'COMMA)]
                            'RBRACKET)
                        (make <rh-array-literal> :contains e)))
    (key-value-pair . ,($do ([k <- 'expression]
                             'ASSIGN
                             [v <- 'expression])
                         (make <rh-key-value> :key k :value v)))
    (hash-literal . ,($do ('LBRACE
                           [p <- ($sep-end-by 'key-value-pair 'COMMA)]
                           'RBRACE)
                       (make <rh-hash-literal> :contains p)))
    (reference-prefixes . ,($/ 'HAT 'TILDE))
    (identifier-reference . ,($do ([p <- ($? 'reference-prefixes)]
                                   [n <- 'IDENT])
                               (cond
                                 [(null? p)
                                  ;; Do nothing
                                  ]
                                 [(string=? p "~")
                                  (set! p 'tilde)]
                                 [(string=? p "^")
                                  (set! p 'hat)])
                               (make <rh-identifier-reference> :prefix p :id n)))
    (named-function-call . ,($do ([n <- 'IDENT]
                                  [a <- 'named-function-argument-list])
                              (make <rh-named-function-call>
                                    :id n
                                    :arguments a)))
    (special-literal . ,($do ([l <- ($/ 'TRUE 'FALSE 'NIL)])
                          (make <rh-special-literal>
                                :value l)))
    (primary-expression . ,($memoize ($/ 'if-expression
                                         'while-expression
                                         'and-expression
                                         'or-expression
                                         'nameless-function-literal
                                         'numeric-literal
                                         'character-literal
                                         'array-literal
                                         'hash-literal
                                         'symbol-literal
                                         'string-literal
                                         'named-function-call
                                         'special-literal
                                         'identifier-reference
                                         ($do ('LPAREN
                                               [e <- 'expression]
                                               'RPAREN)
                                           e))))
    (index-postfix . ,($do ('LBRACKET
                            [i <- 'expression]
                            'RBRACKET)
                           (make <rh-index-reference> :index-expression i)))
    (nameless-function-call-postfix . ,($do ('HAT
                                             [a <- 'named-function-argument-list])
                                            (make <rh-nameless-function-call>
                                                  :arguments a)))
    (method-call-postfix . ,($do ('DOT
                                  [m <- 'IDENT]
                                  [a <- 'named-function-argument-list])
                                 (make <rh-named-function-call>
                                       :id m
                                       :arguments a)))
    (member-postfix . ,($do ('DOT
                             [m <- 'IDENT])
                            (make <rh-member-reference> :member-id m)))
    (postfixes . ,($/ 'method-call-postfix 'index-postfix
                      'nameless-function-call-postfix 'member-postfix))
    (postfix-expression . ,($do ([e <- 'primary-expression]
                                 [p <- ($* 'postfixes)])
                             (if (null? p)
                               e
                               (fold combine-postfix e p))))
    (expression . ,($dynamic parse-expression))
    (assign-statement . ,($do ([t <- ($+ 1 ($seq 'postfix-expression 'ASSIGN))]
                               [f <- 'expression])
                           (make <rh-assign-statement>
                                 :destinations (map car t)
                                 :expression f)))
    (break-statement . ,($do ('BREAK
                              [e <- ($? 'expression)])
                          (make <rh-break-statement>
                                :expression e)))
    (variable-declaration . ,($do ([n <- 'IDENT]
                                   [e <- ($? ($seq 'ASSIGN 'expression))])
                                  (make <rh-variable-declaration>
                                        :id n
                                        :initial-value (if (null? e) '() (cadr e)))))
    (variable-declaration-list . ,($sep-by-1 'variable-declaration 'COMMA))
    (local-declaration . ,($do ('LOCAL
                                [v <- 'variable-declaration-list])
                               (make <rh-local-declaration>
                                     :declarations v)))
    (global-declaration . ,($do ('GLOBAL
                                 [v <- 'variable-declaration-list])
                                (make <rh-global-declaration>
                                      :declarations v)))
    (statement . ,($memoize ($/ 'break-statement
                                'assign-statement
                                'expression
                                'local-declaration
                                'global-declaration
                                'function-definition
                                'class-definition)))
    (block . ,($do ('LBRACE
                    [s <- ($* ($seq 'statement 'delimiter))]
                    [t <- ($? 'statement)]
                    'RBRACE)
                   (make <rh-block>
                         :code (append (map car s)
                                       (if (null? t) '() (list t))))))
    (lambda-block . ,($do ('LBRACE
                           [p <- 'parameter-list]
                           'ARROW
                           [c <- ($sep-end-by 'statement 'delimiter)]
                           'RBRACE)
                       (list p (make <rh-block> :code c))))
    (function-definition . ,($do ('DEF
                                  [n <- 'IDENT] 'implicit-delimiter
                                  'LPAREN 'implicit-delimiter
                                  [p <- 'parameter-list] 'implicit-delimiter
                                  'RPAREN 'implicit-delimiter
                                  [b <- 'block])
                                 (make <rh-function>
                                       :id n
                                       :parameters p
                                       :code b)))
    (member-declaration . ,($sep-end-by 'IDENT 'COMMA))
    (class-block . ,($do ('LBRACE
                          [m <- 'member-declaration]
                          'RBRACE)
                      m))
    (class-definition . ,($do ('CLASS
                               [n <- 'IDENT]
                               'implicit-delimiter
                               [p <- ($? ($seq 'LT 'IDENT))]
                               'implicit-delimiter
                               [b <- 'class-block])
                           (make <rh-class>
                                 :id n
                                 :parent (if (null? p) 'any (cadr p))
                                 :members b)))
    (program . ,($do (($* 'delimiter)
                      [s <- ($* ($seq 'statement 'delimiter))]
                      [t <- ($? 'statement)])
                  (append (map car s) (if (null? t) '() (list t)))))))

