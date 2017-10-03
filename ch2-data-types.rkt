#lang racket/base

(require test-engine/racket-tests)

;; 2.1.1 Booleans

(check-expect (boolean? #t) #t)
(check-expect (boolean? "Hello, World!") #f)

(check-expect (not #t) #f)
(check-expect (not #f) #t)
(check-expect (not "Hello, World!") #f)

;; 2.1.2 Numbers

(check-expect (number? 42)       #t)
(check-expect (number? #t)       #f)
(check-expect (complex? 2+3i)    #t)
(check-expect (real? 2+3i)       #f)
(check-expect (real? 3.1416)     #t)
(check-expect (real? 22/7)       #t)
(check-expect (real? 42)         #t)
(check-expect (rational? 2+3i)   #f)
;; seems odd to me that these are both true
(check-expect (rational? 3.1416) #t)
(check-expect (rational? 22/7)   #t)
(check-expect (integer? 22/7)    #f)
(check-expect (integer? 42)      #t)

(check-expect (eqv? 42 42)   #t)
(check-expect (eqv? 42 #f)   #f)
(check-expect (eqv? 42 42.0) #f)

(check-expect (= 42 42)   #t)
;; ERROR!
;; (check-expect (= 42 #f) #f)
;; evaluates to floating point?
(check-expect (= 42 42.0) #t)

(check-expect (< 3 2)    #f)
(check-expect (>= 4.5 3) #t)

(check-expect (+ 1 2 3)    6)
;; check-expect cannot compare inexact numbers.
;; (check-expect (- 5.3 2)    3.3)
(check-within (- 5.3 2)    3.3 (expt 10 -15))
(check-expect (- 5 2 1)    2)
(check-expect (* 1 2 3)    6)
(check-expect (/ 6 3)      2)
(check-expect (/ 22 7)     22/7)
(check-expect (expt 2 3)   8)
;; (check-expect (expt 4 1/2) 2.0)
(check-within (expt 4 1/2) 2.0 (expt 10 -15))

(check-expect (- 4) -4)
(check-expect (/ 4) 1/4)

(check-expect (max 1 3 4 2 3) 4)
(check-expect (min 1 3 4 2 3) 1)

(check-expect (abs  3) 3)
(check-expect (abs -4) 4)

;; 2.1.3 Characters

(check-expect (char? #\c) #t)
(check-expect (char? 1)   #f)
(check-expect (char? #\;) #t)

(check-expect (char=? #\a #\a)  #t)
(check-expect (char<? #\a #\b)  #t)
(check-expect (char>=? #\a #\b) #f)

(check-expect (char-ci=? #\a #\A) #t)
(check-expect (char-ci<? #\a #\B) #t)

(check-expect (char-downcase #\A) #\a)
(check-expect (char-upcase #\a) #\A)

;; 2.1.4 Symbols

(check-expect #t #t)
(check-expect 42 42)
(check-expect #\c #\c)

(check-expect (quote E) 'E)

(check-expect (symbol? 'xyz) #t)
(check-expect (symbol? 42)   #f)

;; racket/base must differ from
;; http://ds26gte.github.io/tyscheme/index-Z-H-4.html then, where the
;; answer is #t. Is Racket case-sensitive?
(check-expect (eqv? 'calorie 'Calorie) #f)

(define xyz 9)
(check-expect (eqv? xyz 9) #t)
(check-expect xyz 9)
;; (set! xyz #\c)
;; (check-expect (eqv? xyz #\c) #t)

;; 2.2.1 Strings

(check-expect (string #\h #\e #\l #\l #\o) "hello")

(define greeting "Hello; Hello!")
(check-expect (eqv? greeting "Hello; Hello!") #t)
;; same effect as
(check-expect greeting "Hello; Hello!")

(check-expect (string-ref greeting 0) #\H)

(check-expect (string-append "E "
                             "Pluribus "
                             "Unum") "E Pluribus Unum")

;; 2.2.2 Vectors

(check-expect (vector 0 1 2 3 4) #(0 1 2 3 4))

;; 2.2.3 Dotted pairs and lists

;; ERROR!
;; (check-expect (cons 1 #t) (1 . #t))
(check-expect (cons 1 #t) '(1 . #t))

(define x (cons 1 #t))
(check-expect (car x) 1)
(check-expect (cdr x) #t)

;; not available in #lang racket/base?
;; (set-car! x 2)
;; (set-cdr! x #f)
;; (check-expect x '(2 . #f))

(define y (cons (cons 1 2) 3))
(check-expect y '((1 . 2) . 3))
(check-expect (car (car y)) 1)
(check-expect (cdr (car y)) 2)
(check-expect (caar y) 1)
(check-expect (cdar y) 2)

(check-expect (cons 1 (cons 2 (cons 3 (cons 4 5)))) '(1 2 3 4 . 5))
(check-expect (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))) '(1 2 3 4 5))
(check-expect (list 1 2 3 4 5) '(1 2 3 4 5))
(check-expect '(1 . (2 . (3 . (4 . 5)))) '(1 2 3 4 . 5))

;; duplicate definition for identifier
(define y2 (list 1 2 3 4))

(check-expect (list-ref y2 0) 1)
(check-expect (list-ref y2 3) 4)

;; `list-tail` returns the _tail_ of the list starting from the given
;; index.
(check-expect (list-tail y2 1) (list 2 3 4))
(check-expect (list-tail y2 3) (list 4))

(check-expect (pair? '(1 . 2)) #t)
;; Why not #f?
(check-expect (pair? '(1 2))   #t)
(check-expect (pair? '())      #f)
(check-expect (list? '())      #t)
(check-expect (null? '())      #t)
(check-expect (list? '(1 2))   #t)
(check-expect (list? '(1 . 2)) #f) ;; expicitly-dotted lists aren't pairs
(check-expect (null? '(1 2))   #f)
(check-expect (null? '(1 . 2)) #f)

;; 2.2.4 Conversions between data types

(check-expect (char->integer #\d)    100)
(check-expect (integer->char 50)     #\2)
(check-expect (string->list "hello") (list #\h #\e #\l #\l #\o))

;; Other conversion procedures in the same vein are `list->string`,
;; `vector->list`, and `list->vector`.

(check-expect (number->string 16) "16")
(check-expect (string->number "16") 16)
(check-expect (string->number "Am I a hot number?") #f)
(check-expect (string->number "16" 8) 14)

(check-expect (symbol->string 'symbol) "symbol")
(check-expect (string->symbol "string") 'string)

(test)
