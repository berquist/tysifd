#lang racket/base

(require test-engine/racket-tests)

;; 5 Lexical variables

;; reminder: lexical scope is depth-dependent/enclosing scope, as
;; opposed to purely global scope

(define x 9)
(define add2 (lambda (x) (+ x 2)))

(check-expect x 9)
(check-expect (add2 3) 5)
(check-expect (add2 x) 11)
(check-expect x 9)

;; The form `set!` modifies the lexical binding of a
;; variable. https://stackoverflow.com/questions/526082/in-scheme-whats-the-point-of-set#526348

;; (set! x 20)
;; (check-expect x 20)

(define add2-2
  (lambda (x)
    (set! x (+ x 2))
    x))

;; This modifies the `x` local to the lambda, which itself isn't
;; stateful, and *not* the global `x`.
(check-expect (add2-2 x) 11)
(check-expect (add2-2 x) 11)

(define counter 0)

(define bump-counter
  ;; zero-arg procedure is called a _thunk_
  (lambda ()
    (set! counter (+ counter 1))
    counter))

(check-expect (bump-counter) 1)
(check-expect (bump-counter) 2)
(check-expect (bump-counter) 3)

;; 5.1 `let` and `let*`

(check-expect (let ((x 1)
                    (y 2)
                    (z 3))
                (list x y z))
              (list 1 2 3))

;; "local variable initializations are not considered part of the
;; `let` body; `y` is bound to the _global_ `x`
(check-expect (let ((x 1)
                    (y x))
                (+ x y))
              10)

(check-expect (let* ((x 1)
                    (y x))
                (+ x y))
              2)

(check-expect (let ((x 1))
                (let ((y x))
                  (+ x y)))
              2)

;; `let*` is equivalent to nested `let`s; it causes later
;; initializations to be within the _lexical scope_ of earlier
;; initializations

;; let's rewrite cons (haha)
(check-expect (let ((cons (lambda (x y) (+ x y))))
                (cons 1 2))
              3)
(check-expect (cons 1 2) '(1 . 2))

;; 5.2 fluid-let

;; a non-standard special form, see 8.3 for a definition
;; (fluid-let ((counter 99))
;;            (display (bump-counter)) (newline)
;;            (display (bump-counter)) (newline)
;;            (display (bump-counter)) (newline))

(test)
