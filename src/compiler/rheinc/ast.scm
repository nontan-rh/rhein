;;
;; AST classes
;;

(define-module rheinc.ast
  (export-all)
  )

(select-module rheinc.ast)

(define-class <rh-ast> () ())

(define-class <rh-program> (<rh-ast>)
  ((function-definitions :init-keyword :function-definitions)
   (class-definitions :init-keyword :class-definitions)
   (variable-definitions :init-keyword :variable-definitions)))

(define-class <rh-class> (<rh-ast>)
  ((name :init-keyword :name)
   (members :init-keyword :members)))

(define-class <rh-function> (<rh-ast>)
  ((name :init-keyword :name)
   (label :init-keyword :label)
   (parameters :init-keyword :parameters)
   (argument-types :init-keyword :argument-types)
   (code :init-keyword :code)
   (argument-count)
   (variable-count)
   (function-count)))

(define-class <rh-parameter> (<rh-ast>)
  ((name :init-keyword :name)
   (type :init-keyword :type)))

(define-class <rh-closure-function> (<rh-function>)
  ((reference :init-value (undefined))))

(define-class <rh-global-variable> (<rh-ast>)
  ((variable-definitions :init-keyword :variable-definitions)))

(define-class <rh-block> (<rh-ast>)
  ((variable-definitions :init-keyword :variable-definitions)
   (function-definitions :init-keyword :function-definitions)
   (code :init-keyword :code)))

(define-class <rh-variable-definition> (<rh-ast>)
  ((name :init-keyword :name)
   (initial-value :init-keyword :initial-value)
   (reference :init-value (undefined))))

(define-class <rh-if-expression> (<rh-ast>)
  ((conditional-clauses :init-keyword :conditional-clauses)
   (else-clause :init-keyword :else-clause)))

(define-class <rh-conditional-clause> (<rh-ast>)
  ((condition-expression :init-keyword :condition-expression)
   (code :init-keyword :code)))

(define-class <rh-while-expression> (<rh-ast>)
  ((label :init-keyword :label)
   (condition-expression :init-keyword :condition-expression)
   (code :init-keyword :code)))

(define-class <rh-break-statement> (<rh-ast>)
  ((label :init-keyword :label)
   (expression :init-keyword :expression)))

(define-class <rh-identifier-reference> (<rh-ast>)
  ((prefix :init-keyword :prefix)
   (name :init-keyword :name)
   (reference :init-value (undefined))))

(define-class <rh-index-reference> (<rh-ast>)
  ((base-expression :init-keyword :base-expression)
   (index-expression :init-keyword :index-expression)))

(define-class <rh-member-reference> (<rh-ast>)
  ((base-expression :init-keyword :base-expression)
   (member-name :init-keyword :member-name)))

(define-class <rh-identifier-call> (<rh-ast>)
  ((name :init-keyword :name)
   (reference :init-value (undefined))
   (arguments :init-keyword :arguments)))

(define-class <rh-assign-expression> (<rh-ast>)
  ((destinations :init-keyword :destinations)
   (expression :init-keyword :expression)))

(define-class <rh-unary-expression> (<rh-ast>)
  ((operator :init-keyword :operator)
   (expression :init-keyword :expression)))

(define-class <rh-binary-expression> (<rh-ast>)
  ((operator :init-keyword :operator)
   (left-expression :init-keyword :left-expression)
   (right-expression :init-keyword :right-expression)))

(define-class <rh-expression-call> (<rh-ast>)
  ((function :init-keyword :function)
   (arguments :init-keyword :arguments)))

(define-class <rh-hash-literal> (<rh-ast>)
  ((contains :init-keyword :contains)))

(define-class <rh-key-value> (<rh-ast>)
  ((key :init-keyword :key)
   (value :init-keyword :value)))

(define-class <rh-array-literal> (<rh-ast>)
  ((contains :init-keyword :contains)))

(define-class <rh-function-literal> (<rh-function>)
  ((parameters :init-keyword :parameters)
   (code :init-keyword :code)))

(define-class <rh-character-literal> (<rh-ast>)
  ((value :init-keyword :value)))

(define-class <rh-string-literal> (<rh-ast>)
  ((value :init-keyword :value)))

(define-class <rh-integer-literal> (<rh-ast>)
  ((value :init-keyword :value)))

