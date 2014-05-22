
;;(use-modules (ice-9 debug))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 pretty-print))

;;(set! (show-backtrace) #t)
;;(debug-enable 'debug)
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


(define :where ':where)


(define (error-no-match)
  (error 'no-match))

(define *my-gensym-N* 0)

(define (nth n list)
  (list-ref list (- n 1)))


