(define-module (bullet-physics-skateboard)
  #:use-module (bullet-physics-car)
  #:use-module (bullet-physics)
  #:use-module (oop goops)
  #:use-module (bullet)
  #:use-module (beer-parameters)
  #:use-module (vector-math)
  #:use-module (physics)
  #:use-module (emacsy util)  
  #:export (<bullet-physics-skateboard>))

(define-class <bullet-physics-skateboard> (<bullet-physics-car-ind>)
  (legs #:accessor bps:legs #:init-value '())
  (hinges #:accessor bps:hinges #:init-value '()))

(define-method (initialize (bps <bullet-physics-skateboard>) initargs)
  (define (process-object body)
    (set-friction! body 1000.)
      (sim-add-body (bp:sim bps) 
                    body
                    object-group          ; is-a
                    (logior 
                     floor-group)         ; collides with
                    ))
  (define (process-joint joint)
     (sim-add-constraint (bp:sim bps)
                         joint))
  ;; Do the regular construction.
  (next-method)
  
  (let* ((agent-height 10.)
         (leg-width 5.)
         (leg-length agent-diameter)
         (leg-mass 1.)
         (r (/ leg-length 2.)))
    (set! (bps:legs bps) 
          (list (make-box (vector 0 (+ agent-height r) 0)
                          (vector leg-width leg-length leg-width)
                          leg-mass "leg 1")
                (make-box (vector 0 (+ agent-height r leg-length) 0)
                          (vector leg-width leg-length leg-width)
                          leg-mass "leg 2")))
    (set! (bps:hinges bps)
          (let ((agent (car (bp:objects bps)))
                (leg-proximal (car (bps:legs bps)))
                (leg-distal (cadr (bps:legs bps))))
           (list
            (make-hinge agent leg-proximal
                        #(0 0 0) (vector 0 (- r) 0)
                        #(0 0 1) #(0 0 1)
                        "proximal leg axis")
            
            (make-hinge leg-distal leg-proximal 
                        (vector 0 (- r) 0) (vector 0 (+ r) 0)
                        #(0 0 1) #(0 0 1)
                        "distal leg axis"))))
    (map process-object (bps:legs bps))
    (map process-joint (bps:hinges bps))))

(define-method (step-physics (bps <bullet-physics-skateboard>) h)
  "Apply the effectors and step the physics simulation forward by h
seconds."

  (if (effector-func bps)
      (let* ((e1 ((effector-func bps) (get-time bps) 1))
             (e2 ((effector-func bps) (get-time bps) 2))
             (agent (car (bp:objects bps)))
             (motor1 (car (bpc:axes bps)))
             (motor2 (cadr (bpc:axes bps)))
             (motor3 (caddr (bpc:axes bps)))
             (motor4 (cadddr (bpc:axes bps)))
             (hinge-proximal (car (bps:hinges bps)))
             (hinge-distal (cadr (bps:hinges bps)))
             (pi/2 (/ pi 2.))
             (angle-proximal (* e1 pi/2))
             (angle-distal (* e1 pi/2))
             (max-impulse 50.))
        ;(format #t "e1 ~a e2 ~a ~%" e1 e2)        
        ;(format #t "a1 ~a a2 ~a ~%" angle-proximal angle-distal)
        ;; Turn the motors off.
        (for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           #f
                                           )) (list motor1 motor3))
        (for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           #f
                                           )) (list motor2 motor4))
        (actuate-joint hinge-proximal angle-proximal h max-impulse)
        (actuate-joint hinge-distal angle-distal h max-impulse)
        #;(for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           #f)) (list motor2 motor4))
        ))
  (update-fake-state bps)
  (sim-tick (bp:sim bps) h (step-count bps)))
