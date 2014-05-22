Provides a macro called 'define-match'. 'define-match' makes it possible
to write pattern matcher functions which are similar to
Qi and Shen.


See test.scm for examples. Here is one of the tests:

```scheme
;;;;;;;; Quick sort

(define-match keep
  [        ] ____ :> '[]
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

```scheme
(define-match transform-to-check-similarities-0
  R                            _ R _______ :> R
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



License
==========

Copyright 2014 SUN/OPSYS at University of Oslo.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at [apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
