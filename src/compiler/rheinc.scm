;;
;; rhein.scm - Programming language Rhein
;;

(use rheinc.parse)
(use rheinc.compile)

(define (main args)
  (if (= (length args) 1)
      (begin (display "Usage: rhein.scm <script-file>") (newline) (exit 1)))
  (call-with-input-file (cadr args)
    (lambda (port) (compile (port->string port))))
  0)

(define (string->vector s)
  (list->vector (string->list s)))

(define (compile str)
  (rhein-compile (rhein-parse (string->vector str)) (standard-output-port)))

