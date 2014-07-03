(load "bootstrapped.scm")

(define-macro (define-match funcname . matchers)
  (create-matcher-func funcname matchers))

#!
(define-macro (define-match funcname . matchers)
  (let ((ret (create-matcher-func funcname matchers)))
    (pretty-print ret)
    ret))
!#

