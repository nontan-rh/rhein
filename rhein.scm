;;
;; rhein.scm - Programming language Rhein
;;

(require "./parse")
(require "./compile")
(require "./scmutil")

(define (main args)
  (if (= (length args) 1)
      (begin (display "Usage: rhein.scm <script-file>") (newline) (exit 1)))
  (call-with-input-file (cadr args)
    (lambda (port) (compile (read-all-file port))))
  0)

(define (compile str)
  (pretty-print-sexp (rhein-compile (parse str))))

