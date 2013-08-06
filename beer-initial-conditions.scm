(define-module (beer-initial-conditions)
  #:use-module (vector-math)
  #:use-module (beer-parameters)
  #:use-module (emacsy emacsy)
  #:use-module (physics)
  #:use-module (ice-9 match)
  #:use-module (infix)
  #:use-module (stats)
  #:use-module (srfi srfi-1) ;; more flexible map
  
  #:export (generate-IC generate-catchable-IC speeds-to-catch-objects can-catch-objects? gather-IC make-apply-IC random-range time-to-arrival make-parametric-IC*))


(define (random-range low high)
  (if (> low high)
      (random-range high low)
      (+ (random (- high low)) low)))


;; the initial conditions might be defined like this:
;; generate-IC :: int -> alist
;; gather-IC :: fode-params -> alist
;; apply-IC! :: (alist, fode-params) -> undefined

(define* (generate-IC #:optional (object-count body-count))
  (let ((rs (map
             (lambda (i)
               (vector (apply random-range horizontal-position)
                       max-height)) 
             (range 0 (- object-count 2))))
        (vs (map
             (lambda (i)
               (vector 
                (apply random-range horizontal-velocity)
                (apply random-range (if (odd? i) 
                                        vertical-velocity-1
                                        vertical-velocity-2))))
             (range 0 (- object-count 2)))))
    `((position . ,(cons #(0 0) rs))
      (velocity . ,(cons #(0 0) vs)))))

(define* (generate-catchable-IC #:optional 
                                (object-count body-count)
                                (max-speed motor-constant)
                                (max-tries 10000))
  (let ((params (generate-IC object-count))
        (tries 0))
    (while (not (can-catch-objects? params max-speed))
      (if (> tries max-tries)
          (throw 'too-many-tries-for-generate-catchable-IC object-count max-speed max-tries))
      (set! params (generate-IC object-count))
      (incr! tries))
    params))

(define (speeds-to-catch-objects params)
  "Return the speeds required to catch all the objects."
  (let* ((rs (assq-ref params 'position))
         (vs (assq-ref params 'velocity))
         
         (t-x1s (map time-to-arrival rs vs))
         (speeds #f))
    
    (set! t-x1s (sort! t-x1s (lambda (a b) (< (car a) (car b)))))
    (set! speeds (map
                  (match-lambda*
                   (((t1 x1) (t2 x2))
                    (/ (- x2 x1) (- t2 t1))))
                  t-x1s (cdr t-x1s)))
    (map abs speeds)))

(define* (can-catch-objects? params #:optional (max-speed (- motor-constant 1.)))
  (< (apply max (speeds-to-catch-objects params)) max-speed))

(define (gather-IC fode-params)
  (let* ((n (object-count fode-params))
         (rs (map (lambda (i)
                (vector (object-x fode-params i)
                        (object-y fode-params i))) (iota n)))
         (vs (map (lambda (i)
                (vector (object-vx fode-params i)
                        (object-vy fode-params i))) (iota n))))
    `((position . ,rs)
      (velocity . ,vs))))

(define (make-apply-IC alist)
  (lambda (fode-params . rest)
   (let ((n (object-count fode-params))
         (rs (assq-ref alist 'position))
         (vs (assq-ref alist 'velocity)))
     (if (not (= n (length rs)))
         (throw 'wrong-object-count fode-params alist))
     (map (lambda (i r v)
            (unless (= i 0)
              (set! (object-x fode-params i) (vector-ref r 0))
              (set! (object-y fode-params i) (vector-ref r 1))
              (set! (object-vx fode-params i) (vector-ref v 0))
              (set! (object-vy fode-params i) (vector-ref v 1)))) 
          (iota n) rs vs))))

(define* (time-to-arrival pos vel #:optional (y1 0.))
  "Return the amount of time until an object at #(x0 y0) with velocity #(vx vy) arrives at y1."
  (let* ((y0 (: pos @ 1))
         (vy (: vel @ 1))
         (t (if (= vy 0.)
                0.
                (/ (- y1 y0) vy)))
         (x0 (: pos @ 0))
         (vx (: vel @ 0))
         (x1 (+ x0 (* vx t))))
    (list t x1)))

(define* (make-parametric-IC* position-difference #:optional (object-count body-count))
  (let ((rs (map (lambda (i)
                   (vector (* i position-difference)
                           max-height))
                 (iota (1- object-count))))
        (vs (map (lambda (i)
                (vector 0. 
                        (min -1. (+ (* i 2.) (cadr vertical-velocity-1)))))
              (iota (1- object-count)))))
    `((position . ,(cons #(0 0) rs))
      (velocity . ,(cons #(0 0) vs)))))
