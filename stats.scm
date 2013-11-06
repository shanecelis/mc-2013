(define-module (stats)
  #:export (mean std ste median))

(define (mean lst)
  (exact->inexact (/ (apply + lst) (length lst))))

(define* (median lst #:optional (less <))
  (let ((len (length lst))
        (sorted-lst (sort lst less)))
    (if (even? len)
        (let* ((i (quotient len 2))
               (left (list-ref sorted-lst (1- i)))
               (right (list-ref sorted-lst i)))
          (mean (list left right)))
        (list-ref sorted-lst (quotient len 2)))))

(define (std lst)
  (define (square x) (* x x))
  (exact->inexact (sqrt (- (mean (map square lst)) (square (mean lst))))))

(define (ste lst)
  (/ (std lst)
     (sqrt (length lst))))

