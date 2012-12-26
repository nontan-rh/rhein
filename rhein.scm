;;
;; rhein.scm - Programming language Rhein
;;

(require "./parse")
(require "./compile")

(define (main args)
  (if (= (length args) 1)
      (begin (display "Usage: rhein.scm <script-file>") (newline) (exit 1)))
  (call-with-input-file (cadr args)
    (lambda (port) (compile (port->string port))))
  0)

(define (compile str)
  (write (rhein-compile (parse str))))

