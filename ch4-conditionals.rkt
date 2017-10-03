#lang racket/base

(require test-engine/racket-tests)

;; 4 Conditionals

(define p 80)

(check-expect (if (> p 70)
                  'safe
                  'unsafe)
              'safe)

;; else branch not optional?
;; (check-expect (if (< p 90)
;;                   'low-pressure)
;;               'low-pressure)

;; for later examples to work
(define c #\c)

;; 4.2 `cond`

(check-expect (if (char<? c #\c) -1
                  (if (char=? c #\c) 0
                      1))
              0)

(check-expect (cond ((char<? c #\c) -1)
                    ((char=? c #\c) 0)
                    (else 1))
              0)

;; 4.3 `case`

(check-expect (case c
                ((#\a) 1)
                ((#\b) 2)
                ((#\c) 3)
                (else 4))
              3)

;; 4.4 `and` and `or`

;; (check-expect 1 #t)
;; (check-expect 2 #t)
;; The actual value returned is the value of the final subform.
(check-expect (and 1 2) 2)
(check-expect (and #f 1) #f)

(check-expect (or 1 2) 1)
(check-expect (or #f 1) 1)

;; These give `unbound identifier in module`.
;; (check-expect (and 1 #f expression-guaranteed-to-cause-error) #f)
;; (check-expect (or 1 #f expression-guaranteed-to-cause-error) 1)

(test)
