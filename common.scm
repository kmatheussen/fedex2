
;;(use-modules (ice-9 debug))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print))

;;(set! (show-backtrace) #t)
;;(debug-enable 'debug)
(read-enable 'positions)

(use-modules (ice-9 optargs)
	     (ice-9 format)
	     (srfi srfi-1)
	     (srfi srfi-13))

(define (error-no-match)
  (error 'no-match))

(define :where ':where)

(load "common-common.scm")
