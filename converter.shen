(define pretty-print
  A -> ((protect PPRINT) A))

(define test
  A A -> (do (pretty-print "Correct: ") (pretty-print A) (pretty-print "") true)
  A B -> (do (pretty-print "Wrong. Result: ") (pretty-print A) (pretty-print ". Correct: ") (pretty-print B) (pretty-print "") false))

(test (+ 2 3) 5)
(test (+ 3 4) 3)


\\ ;;;;;;;;;;;;;;;

(define upper-case?
  A -> (element? (head (explode A)) (explode "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(test (upper-case? "A") true)
(test (upper-case? "b") false)
(test (upper-case? "5") false)

(define var?
  A -> (and (symbol? A) (upper-case? ((protect STRING) A))))

(test (var? 5) false)
(test (var? "b") false)
(test (var? ef) false)
(test (var? Bs) true)

(define scheme-symbol?
  A -> (and (symbol? A) (not (var? A))))

(test (scheme-symbol? 5) false)
(test (scheme-symbol? "b") false)
(test (scheme-symbol? ef) true)
(test (scheme-symbol? Bs) false)



\\ ;;;;;;;;;;;;;;;

(set *my-gensym-N* 0)
(define with-clean-gensym
  A -> (do (set *my-gensym-N* 0)
           (A 0)))

(define gensym-from-symbol-and-number
  S N -> ((protect INTERN)
          ((protect CONCATENATE) (protect STRING)
           ((protect SYMBOL-NAME) S) 
           ((protect WRITE-TO-STRING) N))))
  
(define my-gensym
  V -> (let N (set *my-gensym-N* (+ 1 (value *my-gensym-N*)))
         (gensym-from-symbol-and-number V N)))

(test (with-clean-gensym (/. _ (my-gensym gakkgakk)))
      gakkgakk1)


\\ ;;;;;;;;;;;;;;;

(set *s* (INTERN ":>"))
(set *u* (INTERN "_"))
(set *where* (INTERN ":where"))

\\ ;;;;;;;;;;;;;;;


(define create-varlist
  -> [])

(define cons-varlist
  Var Value Varlist -> [[Var Value] | Varlist])

(define varlist-value
  _   []              -> ___not-in-varlist
  A   [[A Value] | _] -> Value
  Var [V | Vs]        -> (varlist-value Var Vs))

(test (varlist-value 5 (create-varlist)) ___not-in-varlist)
(test (varlist-value 5 (cons-varlist 1 "a" (create-varlist))) ___not-in-varlist)
(test (varlist-value 5 (cons-varlist 1 "a" (cons-varlist 5 "f" (create-varlist)))) "f")

;;;;;;;;;;;;;;;

(define transform-to-check-similarities-0
  R                            _ R _       -> R
  [if Test Body F]             F R Varlist -> [if Test (transform-to-check-similarities-0 Body F R Varlist) F]
  [let [[Varname Value]] Body] F R Varlist -> (let Prev-value (varlist-value Varname Varlist)                                                    
                                                (if (= Prev-value ___not-in-varlist)
                                                    [let [[Varname Value]]
                                                      (transform-to-check-similarities-0 Body F R (cons-varlist Varname Value Varlist))]
                                                    [if [equal? Varname Value]
                                                        (transform-to-check-similarities-0 Body F R Varlist)
                                                        F]))
  [let Vars Body]              F R Varlist -> [let Vars (transform-to-check-similarities-0 Body F R Varlist)])


(define transform-to-check-similarities
  Body Failure Result -> (transform-to-check-similarities-0 Body Failure Result (create-varlist)))

\*
(untrack transform-to-check-similarities-0)
*\

(test (transform-to-check-similarities [if [pair? A]
                                           [let [[___MatchCar1 [car A]]
                                                 [___MatchCdr2 [cdr A]]]
                                             [let [[B ___MatchCar1]]
                                               [let [[B ___MatchCar3]]
                                                 "r"]]]
                                           "f"]
                                       "f"
                                       "r")
      [if [pair? A]
          [let [[___MatchCar1 [car A]]
                [___MatchCdr2 [cdr A]]]
            [let [[B ___MatchCar1]]
              [if [equal? B ___MatchCar3]
                  "r"
                  "f"]]]
          "f"])
      

\\ ;;;;;;;;;;;;;;;

(define create-single-matcher
  [] [] _ R -> R
  I  [] F R -> [if [null? I]
                   R
                   F]
  Input-var Matcher-var F R -> (let Car-var (my-gensym ___MatchCar)
                                    Cdr-var (my-gensym ___MatchCdr)
                                    Inner-Result (create-single-matcher Cdr-var (tail Matcher-var) F R)
                                    New-Result (create-single-matcher Car-var (head Matcher-var) F Inner-Result)
                                  [if [pair? Input-var]
                                      [let [[Car-var [car Input-var]]
                                            [Cdr-var [cdr Input-var]]]
                                        New-Result]
                                      F])
                                 where (cons? Matcher-var)
  Input-var Matcher-var F R -> [let [[Matcher-var Input-var]]
                                 R]
                               where (var? Matcher-var)
  Input-var Matcher-var F R -> R where (= Matcher-var (value *u*))
  Input-var Matcher-var F R -> [if [eqv? Input-var [quote Matcher-var]]
                                   R
                                   F]
                               where (scheme-symbol? Matcher-var)
  Input-var Matcher-var F R -> [if [eqv? Input-var Matcher-var]
                                   R
                                   F])

\\ (track create-single-matcher)

(test (with-clean-gensym (/. _ (create-single-matcher A 2 "f" "r")))
      [if [eqv? A 2]
          "r"
          "f"])
(test (with-clean-gensym (/. _ (create-single-matcher A a "f" "r")))
      [if [eqv? A [quote a]]
          "r"
          "f"])
(test (with-clean-gensym (/. _ (create-single-matcher A B "f" "r")))
      [let [[B A]]
        "r"])

(test (with-clean-gensym (/. _ (create-single-matcher A (value *u*) "f" "r")))
      "r")

(test (with-clean-gensym (/. _ (create-single-matcher A [B B] "f" "r")))
      [if [pair? A]
          [let [[___MatchCar1 [car A]]
                [___MatchCdr2 [cdr A]]]
            [let [[B ___MatchCar1]]
              [if [pair? ___MatchCdr2]
                  [let [[___MatchCar3 [car ___MatchCdr2]]
                        [___MatchCdr4 [cdr ___MatchCdr2]]]
                    [let [[B ___MatchCar3]]
                      [if [null? ___MatchCdr4]
                          "r" 
                          "f"]]]
                  "f"]]]
          "f"])
(test (with-clean-gensym (/. _ (create-single-matcher A [[2] 4] "f" "r")))
      [if [pair? A]
          [let [[___MatchCar1 [car A]]
                [___MatchCdr2 [cdr A]]]
            [if [pair? ___MatchCar1]
                [let [[___MatchCar5 [car ___MatchCar1]]
                      [___MatchCdr6 [cdr ___MatchCar1]]] 
                  [if [eqv? ___MatchCar5 2]
                      [if [null? ___MatchCdr6]
                          [if [pair? ___MatchCdr2] 
                              [let [[___MatchCar3 [car ___MatchCdr2]]
                                    [___MatchCdr4 [cdr ___MatchCdr2]]] 
                                [if [eqv? ___MatchCar3 4] 
                                    [if [null? ___MatchCdr4] 
                                        "r"
                                        "f"]
                                    "f"]]
                              "f"]
                          "f"]
                      "f"]]
                "f"]]
          "f"])

(define create-matcher-matcher-0
  []     []     _      Result -> Result
  [I|Is] [M|Ms] Failed Result -> (create-single-matcher I M
                                                        Failed
                                                        (create-matcher-matcher-0 Is Ms Failed Result)))

(define create-matcher-matcher
  Inputs Matchers Failure Result -> (transform-to-check-similarities (create-matcher-matcher-0 Inputs Matchers Failure Result)
                                                                     Failure
                                                                     Result))

\\ (untrack create-matcher-matcher-0)
(test (with-clean-gensym (/. _ (create-matcher-matcher [A B] [1 b] "n" "r")))
      [if [eqv? A 1]
          [if [eqv? B [quote b]]
              "r"
              "n"]
          "n"])

(test (with-clean-gensym (/. _ (create-matcher-matcher [A B] [1 2] "n" "r")))
      [if [eqv? A 1]
          [if [eqv? B 2]
              "r"
              "n"]
          "n"])

(test (with-clean-gensym (/. _ (create-matcher-matcher [A B] [C C] "n" "r")))
      [let [[C A]]
        [if [eqv? C B]
            "r"
            "n"]])

\*
(PPRINT (create-matcher-matcher [A] [[2]] "n" "r"))
(PPRINT (create-matcher-matcher [A B] [C D] "n" "r"))
(PPRINT (create-matcher-matcher [A] [[2 [3 5]]] "n" "r"))
*\

\\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-split-0
  Before [] Where Func            -> (Func Before [])
  Before [Where|After] Where Func -> (Func Before After)
  Before [A|After]     Where Func -> (list-split-0 (append Before [A]) After Where Func))

(define list-split
  All Where Func -> (list-split-0 [] All Where Func))

\\ (track list-split-0)

(test (list-split [A B (value *s*) C D] (value *s*) (/. B A [B A]))
      [[A B] [C D]])


\\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-matcher-where
  [Where What | Rest] Kont -> (Kont What Rest) where (= Where (value *where*))
  Rest                Kont -> (Kont ___no_where Rest))

(define get-matchers
  []  -> []
  All -> (list-split All 
                     (value *s*)
                     (/. Before After
                         (let Result (head After)
                           (get-matcher-where (tail After)
                                              (/. Where Rest
                                                  [[Before Result Where] | (get-matchers Rest)]))))))

\\ (untrack get-matchers)
(test (get-matchers [A B (value *s*) 1 BW (value *u*) (value *s*) 2])
      [[[A B] 1 ___no_where]
       [[BW (value *u*)] 2 ___no_where]])

(test (get-matchers [A B (value *s*) 1 (value *where*) [> A 2]
                     BW _ (value *s*) 2])
      [[[A B] 1 [> A 2]]
       [[BW _] 2 ___no_where]])



\\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-args
  0 -> []
  N -> (append (get-args (- N 1))
               [(gensym-from-symbol-and-number ___Arg N)]))

(test (get-args 2)
      [___Arg1 ___Arg2])

(define get-function-names
  0 -> []
  N -> (append (get-function-names (- N 1))
               [[(gensym-from-symbol-and-number ___Func N)]]))

(test (get-function-names 2)
      [[___Func1] [___Func2]])

\\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-local-func
  Name Body -> [define Name Body])

(define create-local-funcs-0
  _  []     [Error-func] -> []
  Is [M|Ms] [F1 F2|Fs]   -> (let Left-side  (nth 1 M)
                                 Right-side (nth 2 M)
                                 Where      (nth 3 M)
                              [(make-local-func F1 (create-matcher-matcher Is
                                                                           Left-side
                                                                           F2
                                                                           (if (= Where ___no_where)
                                                                               Right-side
                                                                               [if Where
                                                                                   Right-side
                                                                                   F2])))
                               | (create-local-funcs-0 Is Ms [F2|Fs])]))

(define create-local-funcs
  All -> (let Matchers (get-matchers All)
              Args     (get-args (length (head (head Matchers))))
              Function-names (append (get-function-names (length Matchers)) [[error-no-match]])
              (create-local-funcs-0 Args Matchers Function-names)))

\\ (track create-matcher-0)
(test (create-local-funcs [A  2 (value *s*) 1 (value *where*) [> A 5]
                           BW (value *u*) (value *s*) 2])
      [[define [___Func1]
         [let [[A ___Arg1]]
           [if [eqv?  ___Arg2 2]
               [if [> A 5]
                   1
                   [___Func2]]
               [___Func2]]]]
       [define [___Func2]
         [let [[BW ___Arg1]]
           2]]])

\\;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define create-matcher-func
  Name Matchers -> (let Num-args (list-split Matchers
                                             (value *s*)
                                             (/. Before _
                                                 (length Before)))
                        Args (get-args Num-args)
                     (append [define] [[Name | Args]]
                             (create-local-funcs Matchers)
                             (get-function-names 1))))

(test (create-matcher-func prime? [A  2 (value *s*) 1
                                   BW (value *u*) (value *s*) 2])
      [define [prime? ___Arg1 ___Arg2]
        [define [___Func1]
          [let [[A ___Arg1]]
            [if [eqv? ___Arg2 2]
                1
                [___Func2]]]]
        [define [___Func2] 
          [let [[BW ___Arg1]]
            2 ]]
        [___Func1]])


(test (create-matcher-func hepp [ (value *s*) 1])
      [define [hepp] [define [___Func1] 1] [___Func1]])


