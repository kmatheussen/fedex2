#!
(load "converter.shen")
"STARTING NOW"
(PPRINT (create-matcher-func  prime? [A  2 (value *s*) 1
                                         BW (value *u*) (value *s*) 2]))
(QUIT)

!#

(use-modules (ice-9 debug))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print))

;;(set! (show-backtrace) #t)
(debug-enable 'debug)
(read-enable 'positions)

(use-modules (ice-9 optargs)
	     (ice-9 format)
	     (srfi srfi-1)
	     (srfi srfi-13))

(define (to-string a)
  (cond ((symbol? a)
         (symbol->string a))
        ((number? a)
         (number->string a))
        ((equal? #t a)
         "#t")
        ((equal? #f a)
         "#f")
        ((keyword? a)
         (<-> "#:" (to-string (keyword->symbol a))))
        (else
         a)))

(define (<-> . args) (apply string-append (map to-string args)))
(define (<_> . args) (string->symbol (apply <-> args)))

(define (c-display . args)
  (for-each (lambda (arg)
              (display arg)
              (display " "))
            args)
  (newline))

(define (get-system-output command)
  (let* ((logfilename "/tmp/snd-ls-logtemp")
         (ret (system (<-> command ">" logfilename))))
    (let* ((output "")
	   (fd (open-file logfilename "r"))
	   (line (read-line fd)))
      (while (not (eof-object? line))
        (set! output (<-> output line))
	     (set! line (read-line fd)))
      (close fd)
      (system (<-> "rm " logfilename))
      output)))

(define (write-string-to-file filename string)
   (define fd (open-file filename "w"))
   (display string fd)
   (close fd))

#!
(write-string-to-file "/tmp/tmp.shen" "hello!")
!#


(define (remove-everything-before string)
  (if (and (string=? (substring string 0 1) "S")
           (string=? (substring string 0 (string-length "STARTING NOW")) "STARTING NOW"))
      (string-drop-right (substring string (+ 2 (string-length "STARTING NOW")))
                         1)
      (remove-everything-before (substring string 1))))

(define (run-converted-code funcname string-code)
  (let ((shen-filename "/tmp/tmp.shen")
        (sh-filename "/tmp/tmp.sh"))
    (write-string-to-file shen-filename
                          (<-> "(load \"/home/ksvalast/fedex2/converter.shen\")\n"
                               "\"STARTING NOW\"\n"
                               "(create-matcher-func " funcname " " string-code ")\n"
                               "(QUIT)"))
    (write-string-to-file sh-filename
                          (<-> "echo '(load \"" shen-filename "\")' | /home/ksvalast/Shen15/ShenSource/Platforms/SBCL/Shen.exe --noprint --non-interactive"))
    (let* ((generated-code-string (remove-everything-before (get-system-output (<-> "sh " sh-filename))))
           (generated-code (read (open-input-string generated-code-string))))
      (pretty-print generated-code)
      generated-code)))


(run-converted-code "newprime"
                    (<-> "[A  2           (value *s*) 1"
                         " BW (value *u*) (value *s*) 2]"))

(define :where ':where)

(define (convert-matchers matcher)
  (c-display matcher)
  (cond ((null? matcher)
         "[]")
        ((pair? matcher)
         (<-> "[" (apply <-> (map (lambda (m)
                                    (<-> (convert-matchers m) " "))
                                  matcher))
              "]"))
        ((equal? ':> matcher)
         "(value *s*)")
        ((equal? '_ matcher)
         "(value *u*)")
        ((equal? #:where matcher)
         "(value *where*)")
        ((equal? ':where matcher)
         "(value *where*)")
        ((equal? :where matcher)
         "(value *where*)")
        ((string? matcher)
         (<-> "\"" matcher "\""))
        (else
         (<-> matcher))))

(convert-matchers '(A 2 :> 1))

(convert-matchers '( '()) )

[quote [2 3 ] ] 

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

(eval-string "[define [newprime2 ___Arg1 ___Arg2] [define [___Func1] [let [[A ___Arg1]] [if [eqv? ___Arg2 2] 1 [___Func2]]]] [define [___Func2] [let [[BW ___Arg1]] 2]] [___Func1]]")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (error-no-match)
  (error 'no-match))

(define-match test
  A A :> (begin (pretty-print "Correct: ") (pretty-print A) (pretty-print "") #t)
  A B :> (begin (pretty-print "Wrong. Result: ") (pretty-print A) (pretty-print ". Correct: ") (pretty-print B) (pretty-print "") #f))

(test (+ 2 3) 5)
(test '(5 2) '(5 2))
(test (test (+ 3 4) 3) #f)

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

(define *my-gensym-N* 0)
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
  _   []              :> '___not-in-varlist
  A   [[A Value] | _] :> Value
  Var [V | Vs]        :> (varlist-value Var Vs))

(test (varlist-value 5 (create-varlist)) '___not-in-varlist)
(test (varlist-value 5 (cons-varlist 1 "a" (create-varlist))) '___not-in-varlist)
(test (varlist-value 5 (cons-varlist 1 "a" (cons-varlist 5 "f" (create-varlist)))) "f")


;;;;;;;;;;;;;;;

(define-match transform-to-check-similarities-0
  R                            _ R _       :> R
  [if Test Body F]             F R Varlist :> `[if ,Test ,(transform-to-check-similarities-0 Body F R Varlist) ,F]
  [let [[Varname Value]] Body] F R Varlist :> (let ((Prev-value (varlist-value Varname Varlist)))
                                                (if (eq? Prev-value '___not-in-varlist)
                                                    `[let [[,Varname ,Value]]
                                                       ,(transform-to-check-similarities-0 Body F R (cons-varlist Varname Value Varlist))]
                                                    `[if [eqv? ,Varname ,Value]
                                                        ,(transform-to-check-similarities-0 Body F R Varlist)
                                                        ,F]))
  [let Vars Body]              F R Varlist :> `[let ,Vars ,(transform-to-check-similarities-0 Body F R Varlist)])


(define-match transform-to-check-similarities
  Body Failure Result :> (transform-to-check-similarities-0 Body Failure Result (create-varlist)))

(test (transform-to-check-similarities '[if [pair? A]
                                           [let [[___MatchCar1 [car A]]
                                                 [___MatchCdr2 [cdr A]]]
                                             [let [[B ___MatchCar1]]
                                               [let [[B ___MatchCar3]]
                                                 "r"]]]
                                           "f"]
                                       "f"
                                       "r")
      '[if [pair? A]
          [let [[___MatchCar1 [car A]]
                [___MatchCdr2 [cdr A]]]
            [let [[B ___MatchCar1]]
              [if [eqv? B ___MatchCar3]
                  "r"
                  "f"]]]
          "f"])
      


;;;;;;;;;;;;;;;

(define-match create-single-matcher
  [] [] _ R :> R
  I  [] F R :> `[if [null? ,I]
                    ,R
                    ,F]
  Input-var Matcher-var F R :> (let* ((Car-var (my-gensym '___MatchCar))
                                      (Cdr-var (my-gensym '___MatchCdr))
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
  Input-var Matcher-var F R :> R :where (eq? Matcher-var '_)
  Input-var Matcher-var F R :> `[if [eqv? ,Input-var [quote ,Matcher-var]]
                                    ,R
                                    ,F]
                               :where (scheme-symbol? Matcher-var)
  Input-var Matcher-var F R :> `[if [eqv? ,Input-var ,Matcher-var]
                                    ,R
                                    ,F])

(test (with-clean-gensym (lambda () (create-single-matcher 'A 2 "f" "r")))
      `[if [eqv? A 2]
          "r"
          "f"])
(test (with-clean-gensym (lambda () (create-single-matcher 'A 'a "f" "r")))
      `[if [eqv? A [quote a]]
          "r"
          "f"])
(test (with-clean-gensym (lambda () (create-single-matcher 'A 'B "f" "r")))
      `[let [[B A]]
        "r"])

(test (with-clean-gensym (lambda () (create-single-matcher 'A '_ "f" "r")))
      "r")

(test (with-clean-gensym (lambda () (create-single-matcher 'A '[B B] "f" "r")))
      `[if [pair? A]
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
(test (with-clean-gensym (lambda () (create-single-matcher 'A '[[2] 4] "f" "r")))
      `[if [pair? A]
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


;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match create-matcher-matcher-0
  []     []     _      Result :> Result
  [I|Is] [M|Ms] Failed Result :> (create-single-matcher I M
                                                        Failed
                                                        (create-matcher-matcher-0 Is Ms Failed Result)))

(define-match create-matcher-matcher
  Inputs Matchers Failure Result :> (transform-to-check-similarities (create-matcher-matcher-0 Inputs Matchers Failure Result)
                                                                     Failure
                                                                     Result))

(test (with-clean-gensym (lambda () (create-matcher-matcher '[A B] '[1 b] "n" "r")))
      `[if [eqv? A 1]
           [if [eqv? B [quote b]]
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
         [if [eqv? C B]
             "r"
             "n"]])

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match list-split-0
  Before [] Where Func            :> (Func Before '[])
  Before [Where|After] Where Func :> (Func Before After)
  Before [A|After]     Where Func :> (list-split-0 (append Before [list A]) After Where Func))

(define-match list-split
  All Where Func :> (list-split-0 '() All Where Func))

(test (list-split '[A B :> C D] ':> (lambda (B A) [list B A]))
      '[[A B] [C D]])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match get-matcher-where
  [Where What | Rest] Kont :> (Kont What Rest) :where (eqv? Where :where)
  Rest                Kont :> (Kont [quote ___no_where] Rest))

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
      `[[[A B] 1 ___no_where]
       [[BW _] 2 ___no_where]])

(test (get-matchers '[A B :> 1 :where [> A 2]
                      BW _ :> 2])
      `[[[A B] 1 [> A 2]]
       [[BW _] 2 ___no_where]])



;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match get-args
  0 :> '[]
  N :> (append (get-args (- N 1))
               `[,(gensym-from-symbol-and-number '___Arg N)]))

(test (get-args 2)
      '[___Arg1 ___Arg2])

(define-match get-function-names
  0 :> '[]
  N :> (append (get-function-names (- N 1))
               `[[,(gensym-from-symbol-and-number '___Func N)]]))

(test (get-function-names 2)
      '[[___Func1] [___Func2]])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-match make-local-func
  Name Body :> `[define ,Name ,Body])

(define (nth n list)
  (list-ref list (- n 1)))

(define-match create-local-funcs-0
  _  []     [Error-func] :> '[]
  Is [M|Ms] [F1 F2|Fs]   :> (let* ((Left-side  (nth 1 M))
                                   (Right-side (nth 2 M))
                                   (Where      (nth 3 M)))
                              `[,(make-local-func F1 (create-matcher-matcher Is
                                                                             Left-side
                                                                             F2
                                                                             (if (eq? Where '___no_where)
                                                                                 Right-side
                                                                                 `[if ,Where
                                                                                      ,Right-side
                                                                                      ,F2])))
                                ,@(create-local-funcs-0 Is Ms `[,F2 ,@Fs])]))

(define-match create-local-funcs
  All :> (let* ((Matchers (get-matchers All))
                (Args     (pretty-print Matchers) (get-args (length (car (car Matchers)))))
                (Function-names (append (get-function-names (length Matchers)) `[[error-no-match]])))
           (create-local-funcs-0 Args Matchers Function-names)))

(test (create-local-funcs '[A  2 :> 1 :where [> A 5]
                            BW _ :> 2])
      `[[define [___Func1]
         [let [[A ___Arg1]]
           [if [eqv?  ___Arg2 2]
               [if [> A 5]
                   1
                   [___Func2]]
               [___Func2]]]]
       [define [___Func2]
         [let [[BW ___Arg1]]
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
      '[define [prime? ___Arg1 ___Arg2]
        [define [___Func1]
          [let [[A ___Arg1]]
            [if [eqv? ___Arg2 2]
                1
                [___Func2]]]]
        [define [___Func2] 
          [let [[BW ___Arg1]]
            2 ]]
        [___Func1]])


(test (create-matcher-func 'hepp '[ :> 1])
      '[define [hepp] [define [___Func1] 1] [___Func1]])


