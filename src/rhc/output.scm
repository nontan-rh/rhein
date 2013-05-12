;;
;; output.scm
;;

(define-module output
  (use rfc.json)
  (export-all))

 (select-module output)

 (define (output-assembly o p)
  (construct-json o p))
 