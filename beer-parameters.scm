(define-module (beer-parameters)
  #:use-module (vector-math))

(define-public (scale-by a list)
  (map (lambda (x) (* a x)) list))

(define-public sensor-count 9)
(define-public internode-count 10)
(define-public effector-count 2)
(define-public node-count (+ sensor-count internode-count effector-count))
;(define-public body-count 3) ;; 1 agent, 2 objects
(define-public body-count 2) ;; 1 agent, 1 object
(define-public agent-diameter 30)
(define-public object-diameter 26) ;; success = 15 + 13 = 28
(define-public motor-constant 5.0)
(define-public max-sight-distance 205)
(define-public visual-angle (/ pi 6))
(define-public max-sight-output 10)
(define-public max-height 180.)
(define-public horizontal-position (scale-by 80 '(-1. 1.)))
;; This is what the paper says, but it then will take max-height
;; seconds for object 2 to actually fall, which seems pretty lengthly.
(define-public horizontal-velocity (scale-by 2 '(-2. 2.)))
(define-public vertical-velocity-1 (scale-by -3 '(3. 4.)))
(define-public vertical-velocity-2 (scale-by -3 '(1. 2.)))

(define-public horizontal-velocity (scale-by 1 '(-2. 2.)))
(define-public vertical-velocity-1 (scale-by -1 '(3. 4.)))
(define-public vertical-velocity-2 (scale-by -1 '(1. 2.)))
(define-public alpha 0.7)

(define-public h 0.1) ;; time step
(define-public population-count 12)
(define-public update-ctrnn-freq 1)
(define-public record-vision-values? #f)

(define-public use-c-vision? #t)

