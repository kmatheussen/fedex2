Provides a new macro called 'define-match', which make it possible
to write pattern matcher functions which are similar in syntax to
Qi and Shen.


See test.scm for examples. Here is one of the tests:

```
;;;;;;;; Quick sort

(define-match keep
  []         _    :> '[]
  [A . Rest] Pred :> (cons A (keep Rest Pred)) :where (Pred A)
  [_ . Rest] Pred :> (keep Rest Pred))
 
(define-match quicksort
  []      :> '[]
  [A . R] :> (append (quicksort (keep R (lambda (B) (>= A B))))
                     (list A)
                     (quicksort (keep R (lambda (B) (< A B))))))

(test (quicksort '[6 8 5 9 3 2 2 1 4 7])
      '[1 2 2 3 4 5 6 7 8 9])
```


The macro is by itself written using the 'define-match' macro, bootstrapped from shen. Here is one of the functions:

```
(define-match transform-to-check-similarities-0
  R                            _ R _       :> R
  [if Test Body F]             F R Varlist :> `[if ,Test ,(transform-to-check-similarities-0 Body F R Varlist) ,F]
  [let [[Varname Value]] Body] F R Varlist :> (let ((Prev-value (varlist-value Varname Varlist)))
                                                (if (eq? Prev-value '___not-in-varlist)
                                                    `[let [[,Varname ,Value]]
                                                       ,(transform-to-check-similarities-0 Body F R (cons-varlist Varname Value Varlist))]
                                                    `[if [equal? ,Varname ,Value]
                                                        ,(transform-to-check-similarities-0 Body F R Varlist)
                                                        ,F]))
  [let Vars Body]              F R Varlist :> `[let ,Vars ,(transform-to-check-similarities-0 Body F R Varlist)])

```
