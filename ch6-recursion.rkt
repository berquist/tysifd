#lang racket/base

(require test-engine/racket-tests)

;; 6 Recursion

(define factorial
  (lambda (n)
    (if (= n 0) 1
        (* n (factorial (- n 1))))))

(define is-even?
  (lambda (n)
    (if (= n 0) #t
        (is-odd? (- n 1)))))

(define is-odd?
  (lambda (n)
    (if (= n 0) #f
        (is-even? (- n 1)))))

(check-expect (is-even? 4) (even? 4))
(check-expect (is-even? 5) (even? 5))
(check-expect (is-odd? 4) (odd? 4))
(check-expect (is-odd? 5) (odd? 5))

;; 6.1 `letrec`

(check-expect
 (letrec ((local-even? (lambda (n)
                         (if (= n 0) #t
                             (local-odd? (- n 1)))))
          (local-odd? (lambda (n)
                        (if (= n 0) #f
                            (local-even? (- n 1))))))
   (list (local-even? 23) (local-odd? 23)))
 (list #f #t))

;; 6.2 Named `let`

(letrec ((countdown (lambda (i)
                      (if (= i 0) 'liftoff
                          (begin
                            (display i)
                            (newline)
                            (countdown (- i 1)))))))
  (countdown 10))

(let countdown ((i 10))
  (if (= i 0) 'liftoff
      (begin
        (display i)
        (newline)
        (countdown (- i 1)))))

;; 6.3 Iteration

;; `list-position` finds the index of the first occurrence of the
;; object `o` in the list `l`. If the object is not found in the list,
;; the procedure returns `#f`.
(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (eqv? (car l) o) i
              (loop (+ i 1) (cdr l)))))))

;; (define reverse!
;;   (lambda (s)
;;     (let loop ((s s) (r '()))
;;       (if (null? s) r
;;           (let ((d (cdr s)))
;;             (set-cdr! s r)
;;             (loop d s))))))

;; 6.4 Mapping a procedure across a list

(define (add2 x) (+ x 2))
(check-expect (map add2 '(1 2 3)) '(3 4 5))

(for-each display
          (list "one " "two " "buckle my shoe"))
(newline)

(check-expect (map cons '(1 2 3) '(10 20 30))
              '((1 . 10) (2 . 20) (3 . 30)))
(check-expect (map + '(1 2 3) '(10 20 30))
              '(11 22 33))

(test)
