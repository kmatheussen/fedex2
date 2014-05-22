
;; run like this: GUILE_AUTO_COMPILE=0 guile bootstrap-from-bootstrapped.scm



(define *bootstrap-fd* (open-file "/home/ksvalast/fedex2/bootstrapped.scm" "w"))

(define is-bootstrapping #t)
(load "/home/ksvalast/fedex2/common.scm")

(load "/home/ksvalast/fedex2/bootstrapped-from-shen.scm")

(define-macro (define-match funcname . matchers)
  (let ((generated-code (create-matcher-func funcname matchers)))
    (display "gen: ")(pretty-print generated-code)
    (pretty-print generated-code *bootstrap-fd*)
    generated-code))

(load "converter.scm")

(close *bootstrap-fd*)

