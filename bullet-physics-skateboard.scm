(define-module (bullet-physics-skateboard)
  #:use-module (bullet-physics-car)
  #:use-module (bullet-physics)
  #:use-module (oop goops)
  #:use-module (bullet)
  #:use-module (beer-parameters)
  #:use-module (vector-math)
  #:use-module (srfi srfi-1) ;;iota
  #:use-module (physics)
  #:use-module (emacsy util)  
  #:export (<bullet-physics-skateboard>
            <bullet-physics-skateboard-n>
            <bullet-physics-skateboard-1>
            <bullet-physics-skateboard-2>
            ))

(define-class <bullet-physics-skateboard> (<bullet-physics-car-ind>)
  (legs #:accessor bps:legs #:init-value '())
  (hinges #:accessor bps:hinges #:init-value '()))

(define-class <bullet-physics-skateboard-n> (<bullet-physics-skateboard>)
  (leg-count #:accessor bps:leg-count #:init-keyword #:leg-count))

(define-class <bullet-physics-skateboard-1> (<bullet-physics-skateboard-n>)
  (leg-count #:init-value 1))

(define-class <bullet-physics-skateboard-2> (<bullet-physics-skateboard-n>)
  (leg-count #:init-value 2))

(define-method (init-physics (bps <bullet-physics-skateboard>) )
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
  (add-legs bps)
  (map process-object (bps:legs bps))
  (map process-joint (bps:hinges bps)))

(define-method (add-legs (bps <bullet-physics-skateboard>))
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
                        "distal leg axis"))))))

(define-method (add-legs (bps <bullet-physics-skateboard-n>))
  (let* ((agent-height 10.)
         (leg-width 5.)
         (leg-length agent-diameter)
         (leg-mass 1.)
         (r (/ leg-length 2.)))
    (set! (bps:legs bps) 
          (map (lambda (i)
                (make-box (vector 0 (+ agent-height r) 0)
                          (vector leg-width leg-length leg-width)
                          leg-mass (format #f "leg ~a" i))) 
               (iota (bps:leg-count bps) 1)))
    (set! (bps:hinges bps)
          (let ((agent (car (bp:objects bps))))
            (map (lambda (i leg)
                   (make-hinge agent leg
                               #(0 0 0) (vector 0 (- r) 0)
                               #(0 0 1) #(0 0 1)
                               (format #f "leg ~a axis" i)))
                 (iota (bps:leg-count bps))
                 (bps:legs bps))))))

(define-method (step-physics (bps <bullet-physics-skateboard>) h)
  "Apply the effectors and step the physics simulation forward by h
seconds."
  (define (get-input index)
    "Get input from 1-based index.  [0, 1] -> [-1, 1]"
    (lerp -1 1 ((effector-func bps) (get-time bps) index))) 
  
  (if (effector-func bps)
      (let* ((agent (car (bp:objects bps)))
             ;; (hinge-proximal (car (bps:hinges bps)))
             ;; (hinge-distal (cadr (bps:hinges bps)))
             (pi/2 (/ pi 2.))
             ;; (angle-proximal (* e1 pi/2))
             ;; (angle-distal (* e1 pi/2)) ;; XXX OMFG!!! 
             (max-impulse 50.))
        ;(format #t "e1 ~a e2 ~a ~%" e1 e2)        
        ;(format #t "a1 ~a a2 ~a ~%" angle-proximal angle-distal)
        ;; Turn the motors off.
        (for-each (lambda (motor) 
                    (actuate-angular-motor motor #f)
                    #;(actuate-angular-motor motor #t))
                  (bpc:axes bps))
        
        (for-each (lambda (i hinge)
                    (let* ((effector (get-input i))
                           (angle (* effector pi/2)))
                      #;(actuate-joint hinge #f)
                      #;(actuate-joint hinge 0. h max-impulse)
                      (actuate-joint hinge angle h max-impulse)))
                  (iota (length (bps:hinges bps)) 1)
                  (bps:hinges bps))
        ;(actuate-joint hinge-proximal angle-proximal h max-impulse)
        ;(actuate-joint hinge-distal angle-distal h max-impulse)
        #;(for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           #f)) (list motor2 motor4))
        ))
  (update-fake-state bps)
  (sim-tick (bp:sim bps) h (step-count bps)))
