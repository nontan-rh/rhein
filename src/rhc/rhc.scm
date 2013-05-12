;;
;; rhc.scm
;;

(use parse)
(use tinypeg)
(use compile)
(use output)

(define (main args)
  (unless (= 2 (length args))
    (error "Usage: rhc.scm <filename>"))
  (receive [s r h] (call-with-input-file (cadr args)
                                         (^p (parse-string rhein-parser 'start (port->string p))))
    (output-assembly (list->vector (compile-all (car r))) (standard-output-port))))
