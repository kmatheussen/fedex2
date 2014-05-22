(load "bootstrapped.scm")

(define-macro (define-match funcname . matchers)
  (create-matcher-func funcname matchers))

#!
!#
