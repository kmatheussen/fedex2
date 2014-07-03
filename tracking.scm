;; qi/shen style track/untrack.

(use-modules (srfi srfi-1)
             (srfi srfi-13))

(define *tracking* (make-hash-table))

#!
(hash-set! *tracking* 'a 50)
(hash-ref *tracking* 'a)
(hash-remove! *tracking* 'a)
!#

(load "common.scm")

(define *level* 0)

(define (print-level n)
  (let loop ((N 0))
  (if (< N n)
      (begin
        (display " ")
        (print-level (- N 1))))))

(define-macro (track func)
  (let ((output (gensym "output"))
        (old-func (gensym "old-func"))
        (x (gensym "x"))
        (display-input (gensym "display-input")))
    `(let* ((,old-func (or (hash-ref *tracking* ',func) ,func))
            (,display-input (lambda (x)
                              (print-level *level*)
                              (display ',func)(display " in: ")(display " ")
                              (cond ((not (null? x))
                                     (for-each (lambda (arg) (display arg)(display ", "))
                                               (reverse (cdr (reverse x))))
                                     (display (last x))))))
            (track-func (lambda ,x
                          (,display-input ,x)(newline)
                          (set! *level* (+ *level* 1))
                          (let ((,output (apply ,old-func ,x)))
                            (,display-input ,x)
                            (c-display ", out:" ,output)
                            (set! *level* (- *level* 1))
                            ,output))))
       (hash-set! *tracking* ',func ,old-func)
       (set! ,func track-func))))

(define-macro (untrack func)
  `(set! ,func (hash-ref *tracking* ',func)))

(define (add a b)
  (+ a b))

#!
(track add)
(untrack add)

(add 5 6)
!#

(define (add2 a . b)
  (+ a (car b)))

#!
(track add2)
(untrack add2)

(add2 5 6)
!#

(define (fa b)
  (if (= 0 b)
      50
      (fa (- b 1))))

(track fa)

(fa 5)

(define-match fib
  0 :> 0
  1 :> 1
  N :> (+ (fib (- N 2))
          (fib (- N 1))))

(track fib)
(fib 5)

