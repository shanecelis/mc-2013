(define-module (stats)
  #:export (mean std))

(define (mean lst)
  (exact->inexact (/ (apply + lst) (length lst))))

(define (std lst)
  (define (square x) (* x x))
  (exact->inexact (sqrt (- (mean (map square lst)) (square (mean lst))))))

