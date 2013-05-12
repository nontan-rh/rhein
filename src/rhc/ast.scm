;;
;; AST classes
;;

(define-module ast
  (export-all))

(select-module ast)

(define-class <rh-ast> () ())

(define-class <rh-class> (<rh-ast>)
  ((id :init-keyword :id)
   (parent :init-keyword :parent)
   (members :init-keyword :members)))

(define-class <rh-function> (<rh-ast>)
  ((id :init-keyword :id)
   (parameters :init-keyword :parameters)
   (code :init-keyword :code)
   (variable-slot-count)
   (function-slot-count)
   (reference)))

(define-class <rh-parameter> (<rh-ast>)
  ((id :init-keyword :id)
   (type :init-keyword :type)))

(define-class <rh-global-declaration> (<rh-ast>)
  ((declarations :init-keyword :declarations)))

(define-class <rh-local-declaration> (<rh-ast>)
  ((declarations :init-keyword :declarations)))

(define-class <rh-block> (<rh-ast>)
  ((code :init-keyword :code)))

(define-class <rh-variable-declaration> (<rh-ast>)
  ((id :init-keyword :id)
   (initial-value :init-keyword :initial-value)
   (reference)))

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

(define-class <rh-and-expression> (<rh-block>)
  ())

(define-class <rh-or-expression> (<rh-block>)
  ())

(define-class <rh-identifier-reference> (<rh-ast>)
  ((prefix :init-keyword :prefix)
   (id :init-keyword :id)
   (reference :init-value (undefined))))

(define-class <rh-index-reference> (<rh-ast>)
  ((base-expression)
   (index-expression :init-keyword :index-expression)))

(define-class <rh-member-reference> (<rh-ast>)
  ((base-expression)
   (member-id :init-keyword :member-id)))

(define-class <rh-assign-statement> (<rh-ast>)
  ((destinations :init-keyword :destinations)
   (expression :init-keyword :expression)))

(define-class <rh-unary-expression> (<rh-ast>)
  ((operator :init-keyword :operator)
   (expression :init-keyword :expression)))

(define-class <rh-binary-expression> (<rh-ast>)
  ((operator :init-keyword :operator)
   (left-expression :init-keyword :left-expression)
   (right-expression :init-keyword :right-expression)))

(define-class <rh-named-function-call> (<rh-ast>)
  ((id :init-keyword :id)
   (reference)
   (arguments :init-keyword :arguments)))

(define-class <rh-nameless-function-call> (<rh-ast>)
  ((function)
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

(define-class <rh-special-literal> (<rh-ast>)
  ((value :init-keyword :value)))


