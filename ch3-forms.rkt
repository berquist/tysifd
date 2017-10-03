#lang racket/base

(require test-engine/racket-tests)

;; 3.1 Procedures

(check-expect ((lambda (x) (+ x 2)) 5) 7)

(define add2
  (lambda (x) (+ x 2)))

(check-expect (add2 4) 6)
(check-expect (add2 9) 11)

;; 3.1.1 Procedure parameters

(define area
  (lambda (length breadth)
    (* length breadth)))

(define area2 *)

(check-expect (area 4 5) (area2 4 5))

;; 3.2 Apply

(define x '(1 2 3))
(check-expect (apply + x) 6)
(check-expect (apply + 1 2 3 x) 12)

;; 3.3 Sequencing

(define display3
  (lambda (arg1 arg2 arg3)
    (begin
      (display arg1)
      (display " ")
      (display arg2)
      (display " ")
      (display arg3)
      (newline))))

(define display4
  (lambda (arg1 arg2 arg3)
    (display arg1)
    (display " ")
    (display arg2)
    (display " ")
    (display arg3)
    (newline)))

(test)
