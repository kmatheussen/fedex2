
;; run like this: GUILE_AUTO_COMPILE=0 guile bootstrap-from-shen.scm

(define is-bootstrapping #t)



(define *bootstrap-fd* (open-file "/home/ksvalast/fedex2/bootstrapped-from-shen.scm" "w"))


(load "/home/ksvalast/fedex2/common.scm")



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
    (c-display "run-converted-code for" funcname)
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

      (if (defined? 'is-bootstrapping)
          (pretty-print generated-code *bootstrap-fd*))
      generated-code)))

#!
(run-converted-code "newprime"
                    (<-> "[A  2           (value *s*) 1"
                         " BW (value *u*) (value *s*) 2]"))
!#

(define (separate-improper-list improper-list k)
  (let loop ((improper-list improper-list)
             (proper-list '()))
    (if (pair? improper-list)
        (loop (cdr improper-list)
              (cons (car improper-list) proper-list))
        (k (reverse proper-list)
           improper-list))))
#!
(separate-improper-list '(2 3 . 4) list)
(separate-improper-list '(2 . 4) list)
(separate-improper-list '() list)
!#             

;; convert from scheme syntax into shen syntax
(define (convert-matchers matcher)
  ;;(c-display matcher)
  (cond ((null? matcher)
         "[]")
        ((and (pair? matcher)
              (dotted-list? matcher))
         (separate-improper-list matcher
                                 (lambda (proper-list last-element)
                                   (<-> "[ " (apply <-> (map (lambda (m)
                                                               (<-> (convert-matchers m) " "))
                                                             proper-list))
                                        " | " (convert-matchers last-element)
                                        "] "))))      
        ((pair? matcher)
         (<-> "[ " (apply <-> (map (lambda (m)
                                    (<-> (convert-matchers m) " "))
                                  matcher))
              " ] "))
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
        ;;((equal? (string->symbol "|") matcher)
        ;; " . ")
        ((string? matcher)
         (<-> "\"" matcher "\""))
        (else
         (<-> matcher))))

#!
(convert-matchers '(A 2 :> 1))
(convert-matchers '( '()) )
(convert-matchers [quote [2 3 ] ] )
(convert-matchers '[[A | B] :> 1])
(convert-matchers '[A . B])
(convert-matchers '[[A . B] :> 1])
!#


(define-macro (define-match funcname . matchers)
  (display "hepp: ")
  (display (quote funcname))
  (newline)
  (run-converted-code (<-> funcname)
                      (convert-matchers matchers)))

(load "/home/ksvalast/fedex2/converter.scm")

(close *bootstrap-fd*)

