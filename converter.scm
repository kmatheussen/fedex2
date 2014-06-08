
;; All functions in this file must be defined with define-match. If not, they will not be included in bootstrapped.scm

#!


(define-macro (define-match funcname . matchers)
  (run-converted-code (<-> funcname)
                      (convert-matchers matchers)))


(define-match newprime2
  A  2 :> 1
  BW _ :> 2)

(newprime 2 1)
(newprime 2 2)

(define-match fib
  0 :> 0
  N :> (begin (display "hello!") (newline)) :where (= N 1)
  N :> (+ (fib (- N 1))
          (fib (- N 2))))

(fib 1)

(substring "asdf" 0 1)

(eval-string (remove-everything-before (get-system-output "sh /home/ksvalast/fedex2/run-script.sh")))

(prime? 90 5)

(eval-string "[define [newprime2 -__Arg1 -__Arg2] [define [-__Func1] [let [[A -__Arg1]] [if [eqv? -__Arg2 2] 1 [-__Func2]]]] [define [-__Func2] [let [[BW -__Arg1]] 2]] [-__Func1]]")
!#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match test
  A A :> (begin (pretty-print "Correct: ") (pretty-print A) (pretty-print "") #t)
  A B :> (begin (pretty-print "Wrong. Result: ") (pretty-print A) (pretty-print ". Correct: ") (pretty-print B) (pretty-print "") #f))

(test (+ 2 3) 5)
(test '(5 2) '(5 2))
(test (test (+ 3 4) 3) #f)

;;(if (defined? 'is-bootstrapping)
;;    (set! test (lambda (a b) #t)))

(if (defined? 'is-testing)
    (let ((old-test test))
      (set! test (lambda (a b)
                   (if (not (old-test a b))
                       (error 'failure'))))))

;;;;;;;;;;;;;;;

(define-match upper-case?
  S :> (if (member (car (string->list S)) (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
           #t
           #f))

(test (upper-case? "A") #t)
(test (upper-case? "b") #f)
(test (upper-case? "5") #f)

(define-match var?
  A :> (and (symbol? A) (upper-case? (symbol->string A))))

(test (var? 5) #f)
(test (var? "b") #f)
(test (var? 'ef) #f)
(test (var? 'Bs) #t)

(define-match scheme-symbol?
  A :> (and (symbol? A) (not (var? A))))

(test (scheme-symbol? 5) #f)
(test (scheme-symbol? "b") #f)
(test (scheme-symbol? 'ef) #t)
(test (scheme-symbol? 'Bs) #f)

;;;;;;;;;;;;;;;

(define-match with-clean-gensym
  A :> (begin (set! *my-gensym-N* 0)
              (A)))

(define-match gensym-from-symbol-and-number
  S N :> (<_> S N))
  
(define-match my-gensym
  V :> (begin
         (set! *my-gensym-N* (+ 1 *my-gensym-N*))
         (gensym-from-symbol-and-number V *my-gensym-N*)))

(test (with-clean-gensym (lambda () (my-gensym 'gakkgakk)))
      'gakkgakk1)

;;;;;;;;;;;;;;;

(define-match create-varlist
  :> '())

(define-match cons-varlist
  Var Value Varlist :> `[[,Var ,Value] ,@Varlist])

(define-match varlist-value
  ___ [              ] :> '-__not-in-varlist
  A   [[A Value] . __] :> Value
  Var [_________ . Vs] :> (varlist-value Var Vs))

(test (varlist-value 5 (create-varlist)) '-__not-in-varlist)
(test (varlist-value 5 (cons-varlist 1 "a" (create-varlist))) '-__not-in-varlist)
(test (varlist-value 5 (cons-varlist 1 "a" (cons-varlist 5 "f" (create-varlist)))) "f")


;;;;;;;;;;;;;;;

(define-match transform-to-check-similarities-0
  R                            _ R _______ :> R
  [if Test Body F]             F R Varlist :> `[if ,Test ,(transform-to-check-similarities-0 Body F R Varlist) ,F]
  [let [[Varname Value]] Body] F R Varlist :> (let ((Prev-value (varlist-value Varname Varlist)))
                                                (if (eq? Prev-value '-__not-in-varlist)
                                                    `[let [[,Varname ,Value]]
                                                       ,(transform-to-check-similarities-0 Body F R (cons-varlist Varname Value Varlist))]
                                                    `[if [equal? ,Varname ,Value]
                                                        ,(transform-to-check-similarities-0 Body F R Varlist)
                                                        ,F]))
  [let Vars Body]              F R Varlist :> `[let ,Vars ,(transform-to-check-similarities-0 Body F R Varlist)])


(define-match transform-to-check-similarities
  Body Failure Result :> (transform-to-check-similarities-0 Body Failure Result (create-varlist)))

(display "HOOOLA")(newline)
(test (transform-to-check-similarities '[if [pair? A]
                                           [let [[-__MatchCar1 [car A]]
                                                 [-__MatchCdr2 [cdr A]]]
                                             [let [[B -__MatchCar1]]
                                               [let [[B -__MatchCar3]]
                                                 "r"]]]
                                           "f"]
                                       "f"
                                       "r")
      '[if [pair? A]
          [let [[-__MatchCar1 [car A]]
                [-__MatchCdr2 [cdr A]]]
            [let [[B -__MatchCar1]]
              [if [equal? B -__MatchCar3]
                  "r"
                  "f"]]]
          "f"])
(display "HOOOLA finished")(newline)


;;;;;;;;;;;;;;;

(define-match has-pipe
  M   :> #f :where (not (pair? M))
  []  :> #f
  M   :> #t :where (eq? (car M) (string->symbol "|"))
  M   :> (has-pipe (cdr M)))

(test (has-pipe #f) #f)
(test (has-pipe '()) #f)
(test (has-pipe '(| b)) #t)
(test (has-pipe '(a | b)) #t)
(test (has-pipe '(a (|))) #f)

(define-match pipe-to-dot
  [] :> '[]
  M  :>  (cadr M) :where (eq? (car M) (string->symbol "|"))
  M  :> (cons (car M) (pipe-to-dot (cdr M))))

;; (string->symbol "|")) (this commented line is here only in order to fix colors in emacs)

(test (pipe-to-dot '(a b)) '(a b))
(test (pipe-to-dot '(a | b)) '(a . b))
(test (pipe-to-dot '(a b | c)) '(a b . c))


;;;;;;;;;;;;;;;

(define-match create-single-matcher
  [] [] _ R :> R
  I  [] F R :> `[if [null? ,I]
                    ,R
                    ,F]
  Input-var Matcher-var F R :> (create-single-matcher Input-var (pipe-to-dot Matcher-var) F R)
                               :where (has-pipe Matcher-var)
  Input-var Matcher-var F R :> (let* ((Car-var (my-gensym '-__MatchCar))
                                      (Cdr-var (my-gensym '-__MatchCdr))
                                      (Inner-Result (create-single-matcher Cdr-var (cdr Matcher-var) F R))
                                      (New-Result (create-single-matcher Car-var (car Matcher-var) F Inner-Result)))
                                 `[if [pair? ,Input-var]
                                      [let [[,Car-var [car ,Input-var]]
                                            [,Cdr-var [cdr ,Input-var]]]
                                        ,New-Result]
                                      ,F])
                               :where (pair? Matcher-var)
  Input-var Matcher-var F R :> `[let [[,Matcher-var ,Input-var]]
                                 ,R]
                               :where (var? Matcher-var)
  Input-var Matcher-var F R :> R :where (and (symbol? Matcher-var)
                                             (string=? "_" (substring (symbol->string Matcher-var) 0 1)))
  Input-var Matcher-var F R :> `[if [eq? ,Input-var [quote ,Matcher-var]]
                                    ,R
                                    ,F]
                               :where (scheme-symbol? Matcher-var)
  Input-var Matcher-var F R :> `[if [string? ,Input-var ,Matcher-var]
                                    ,R
                                    ,F]
                               :where (string? Matcher-var)
  Input-var Matcher-var F R :> `[if [eqv? ,Input-var ,Matcher-var]
                                    ,R
                                    ,F])

(test (with-clean-gensym (lambda () (create-single-matcher 'A '_ "f" "r")))
      "r")
(test (with-clean-gensym (lambda () (create-single-matcher 'A '__ewf "f" "r")))
      "r")
(test (with-clean-gensym (lambda () (create-single-matcher 'A 2 "f" "r")))
      `[if [eqv? A 2]
          "r"
          "f"])
(test (with-clean-gensym (lambda () (create-single-matcher 'A 'a "f" "r")))
      `[if [eq? A [quote a]]
          "r"
          "f"])
(test (with-clean-gensym (lambda () (create-single-matcher 'A 'B "f" "r")))
      `[let [[B A]]
        "r"])

(test (with-clean-gensym (lambda () (create-single-matcher 'A '_ "f" "r")))
      "r")

(test (with-clean-gensym (lambda () (create-single-matcher 'A '[B . B] "f" "r")))
      '(if (pair? A)
           (let ((-__MatchCar1 (car A))
                 (-__MatchCdr2 (cdr A)))
             (let ((B -__MatchCar1))
               (let ((B -__MatchCdr2))
                 "r")))
           "f"))

(test (with-clean-gensym (lambda () (create-single-matcher 'A '[B B] "f" "r")))
      `[if [pair? A]
          [let [[-__MatchCar1 [car A]]
                [-__MatchCdr2 [cdr A]]]
            [let [[B -__MatchCar1]]
              [if [pair? -__MatchCdr2]
                  [let [[-__MatchCar3 [car -__MatchCdr2]]
                        [-__MatchCdr4 [cdr -__MatchCdr2]]]
                    [let [[B -__MatchCar3]]
                      [if [null? -__MatchCdr4]
                          "r" 
                          "f"]]]
                  "f"]]]
          "f"])

(test (with-clean-gensym (lambda () (create-single-matcher 'A '[[2] 4] "f" "r")))
      `[if [pair? A]
          [let [[-__MatchCar1 [car A]]
                [-__MatchCdr2 [cdr A]]]
            [if [pair? -__MatchCar1]
                [let [[-__MatchCar5 [car -__MatchCar1]]
                      [-__MatchCdr6 [cdr -__MatchCar1]]] 
                  [if [eqv? -__MatchCar5 2]
                      [if [null? -__MatchCdr6]
                          [if [pair? -__MatchCdr2] 
                              [let [[-__MatchCar3 [car -__MatchCdr2]]
                                    [-__MatchCdr4 [cdr -__MatchCdr2]]] 
                                [if [eqv? -__MatchCar3 4] 
                                    [if [null? -__MatchCdr4] 
                                        "r"
                                        "f"]
                                    "f"]]
                              "f"]
                          "f"]
                      "f"]]
                "f"]]
          "f"])


;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match create-matcher-matcher-0
  [      ] [      ] ______ Result :> Result
  [I . Is] [M . Ms] Failed Result :> (create-single-matcher I M
                                                            Failed
                                                            (create-matcher-matcher-0 Is Ms Failed Result)))

(define-match create-matcher-matcher
  Inputs Matchers Failure Result :> (transform-to-check-similarities (create-matcher-matcher-0 Inputs Matchers Failure Result)
                                                                     Failure
                                                                     Result))

(test (with-clean-gensym (lambda () (create-matcher-matcher '[A B] '[1 b] "n" "r")))
      `[if [eqv? A 1]
           [if [eq? B [quote b]]
               "r"
               "n"]
           "n"])

(test (with-clean-gensym (lambda () (create-matcher-matcher '[A B] '[1 2] "n" "r")))
      `[if [eqv? A 1]
           [if [eqv? B 2]
               "r"
              "n"]
           "n"])

(test (with-clean-gensym (lambda () (create-matcher-matcher '[A B] '[C C] "n" "r")))
      `[let [[C A]]
         [if [equal? C B]
             "r"
             "n"]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match list-split-0
  Before [             ] Where Func :> (Func Before '[])
  Before [Where . After] Where Func :> (Func Before After)
  Before [A     . After] Where Func :> (list-split-0 (append Before [list A]) After Where Func))

(define-match list-split
  All Where Func :> (list-split-0 '() All Where Func))

(test (list-split '[A B :> C D] ':> (lambda (B A) [list B A]))
      '[[A B] [C D]])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match get-matcher-where
  [Where What . Rest] Kont :> (Kont What Rest) :where (eqv? Where :where)
  Rest                Kont :> (Kont '-__no_where Rest))

(define-match get-matchers
  []  :> '[]
  All :> (list-split All 
                     ':>
                     (lambda (Before After)
                       (let ((Result (car After)))
                         (get-matcher-where (cdr After)
                                            (lambda (Where Rest)
                                              `[[,Before ,Result ,Where] ,@(get-matchers Rest)]))))))

(test (get-matchers '[A B :> 1 BW _ :> 2])
      `[[[A B] 1 -__no_where]
       [[BW _] 2 -__no_where]])

(test (get-matchers '[A B :> 1 :where [> A 2]
                      BW _ :> 2])
      `[[[A B] 1 [> A 2]]
       [[BW _] 2 -__no_where]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match get-args
  0 :> '[]
  N :> (append (get-args (- N 1))
               `[,(gensym-from-symbol-and-number '-__Arg N)]))

(test (get-args 2)
      '[-__Arg1 -__Arg2])

(define-match get-function-names
  0 :> '[]
  N :> (append (get-function-names (- N 1))
               `[[,(gensym-from-symbol-and-number '-__Func N)]]))

(test (get-function-names 2)
      '[[-__Func1] [-__Func2]])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match make-local-func
  Name Body :> `[define ,Name ,Body])

(define-match create-local-funcs-0
  __ [      ] [Error-func] :> '[]
  Is [M . Ms] [F1 F2 . Fs] :> (let* ((Left-side  (nth 1 M))
                                     (Right-side (nth 2 M))
                                     (Where      (nth 3 M)))
                                `[,(make-local-func F1 (create-matcher-matcher Is
                                                                               Left-side
                                                                               F2
                                                                               (if (eq? Where '-__no_where)
                                                                                   Right-side
                                                                                   `[if ,Where
                                                                                        ,Right-side
                                                                                        ,F2])))
                                  ,@(create-local-funcs-0 Is Ms `[,F2 ,@Fs])]))

(define-match create-local-funcs
  All :> (let* ((Matchers (get-matchers All))
                (Args     (get-args (length (car (car Matchers)))))
                (Function-names (append (get-function-names (length Matchers)) `[[error-no-match]])))
           (create-local-funcs-0 Args Matchers Function-names)))

(test (create-local-funcs '[A  2 :> 1 :where [> A 5]
                            BW _ :> 2])
      `[[define [-__Func1]
         [let [[A -__Arg1]]
           [if [eqv?  -__Arg2 2]
               [if [> A 5]
                   1
                   [-__Func2]]
               [-__Func2]]]]
       [define [-__Func2]
         [let [[BW -__Arg1]]
           2]]])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match create-matcher-func
  Name Matchers :> (let* ((Num-args (list-split Matchers
                                                ':>
                                                (lambda (Before After)
                                                  (length Before))))
                          (Args (get-args Num-args)))
                     (append '[define] `[[,Name ,@Args]]
                             (create-local-funcs Matchers)
                             (get-function-names 1))))

(test (create-matcher-func 'prime? '[A  2 :> 1
                                    BW _ :> 2])
      '[define [prime? -__Arg1 -__Arg2]
        [define [-__Func1]
          [let [[A -__Arg1]]
            [if [eqv? -__Arg2 2]
                1
                [-__Func2]]]]
        [define [-__Func2] 
          [let [[BW -__Arg1]]
            2 ]]
        [-__Func1]])


(test (create-matcher-func 'hepp '[ :> 1])
      '[define [hepp] [define [-__Func1] 1] [-__Func1]])



