;; bullet-physics-car.scm

(define-module (bullet-physics-car)
  #:use-module (bullet-physics)
  #:use-module (oop goops)
  #:use-module (bullet)
  #:use-module (beer-parameters)
  #:use-module (vector-math)
  #:use-module (physics)
  #:use-module (emacsy util)  
  #:export (bpc:wheels bpc:axes bpc:slider <bullet-physics-car> <bullet-physics-car-ind>
                       )
  )

;; This is the factor that is multiplied by the wheels.  Basically the
;; wheels' motor constant.
(define velocity-constant 5.)
(define max-impulse 5.)
(define wheel-diameter (* 2 (/ agent-diameter 5.)))

(define-class <bullet-physics-car> (<bullet-physics>)
  (wheels #:accessor bpc:wheels #:init-value '())
  (axes #:accessor bpc:axes #:init-value '())
  (slider #:accessor bpc:slider #:init-value #f)
  )

;; This car has independent steering for its 4 wheels.
(define-class <bullet-physics-car-ind> (<bullet-physics-car>))

(define-method (initialize (bpc <bullet-physics-car>) initargs)
  "Initialize the bullet car."
  ;; XXX bizzarre and probably a bug. private function definitions not allowed
  ;; at the beginning of a define-method
  (let ()
   (define (process-wheel wheel)
     (rotate-body! wheel 0. 0. (/ pi 2.))
     ;(format #t "friction ~a~%" (get-friction wheel))
     (set-friction! wheel 1000.)
     (sim-add-body (bp:sim bpc) 
                   wheel
                   wheel-group          ; is-a
                   (logior 
                    floor-group)        ; collides with
                   ))
   (define (process-joint joint)
     (sim-add-constraint (bp:sim bpc)
                         joint))
   (define (make-wheel x y)
     
     ;; The cylinder will be aligned on the Y-axis, but we
     ;; want it to be on the Z-axis.
     (make-cylinder 
      ;; position
      (vector x ;x 
              0 ;y
              0)
      ;; dimensions
      (vector wheel-diameter
              (* 0.2 agent-diameter)
              wheel-diameter)
      ;; mass
      1.
      ;; name
      (format #f "wheel (~a,~a)" x y)))
   (next-method)
   
   ;; Let's make the wheels.
   (set! (bpc:wheels bpc) 
         (let ((x (/ agent-diameter 2.))
               (y (/ agent-diameter 2.)))
           
           (list (make-wheel (- x) y)
                 (make-wheel x y)
                 (make-wheel (- x) (- y))
                 (make-wheel x (- y)))))
   (for-each process-wheel (bpc:wheels bpc))
   ;; Let's make the joints.
   
   (let ((agent (car (bp:objects bpc)))
         (wheel1 (car (bpc:wheels bpc)))
         (wheel2 (cadr (bpc:wheels bpc)))
         (wheel3 (caddr (bpc:wheels bpc)))
         (wheel4 (cadddr (bpc:wheels bpc)))
         (x (/ agent-diameter 2.))
         (z (/ agent-diameter 2.)))
     
     (set! (bpc:axes bpc) 
           (map (lambda (position wheel)
                  (set-position! wheel position)
                  (make-hinge agent                            wheel
                              position                         (vector 0 0 0)
                              #(0 0 1)                         #(0 1 0)
                              "axis"))
                (list (vector (- x) 0 z)
                      (vector x 0 z)
                      (vector (- x) 0 (- z))
                      (vector x 0 (- z)))
                (bpc:wheels bpc)))
     
     (for-each process-joint (bpc:axes bpc))
     
     ;; Let's put a constraint on the car to only move along the x-axis.
     (set! (bpc:slider bpc) (make-slider agent #(1 0 0)))
                                        ;(process-joint (bpc:slider bpc))
     )))

(define-method (reset-physics (bpc <bullet-physics-car>))
  (map (lambda (joint)
         (sim-remove-constraint (bp:sim bpc) joint)) 
       (bpc:axes bpc))
  (sim-remove-constraint (bp:sim bpc) (bpc:slider bpc))

  (map (lambda (body)
         (sim-remove-body (bp:sim bpc) body)) 
       (bpc:wheels bpc))
  (set! (bpc:wheels bpc) '())
  (next-method))

(define-method (step-physics (bp <bullet-physics-car>) h)
  "Apply the effectors and step the physics simulation forward by h
seconds."

  (if (effector-func bp)
      (let* ((e1 ((effector-func bp) (get-time bp) 1))
             (e2 ((effector-func bp) (get-time bp) 2))
             (agent (car (bp:objects bp)))
             (motor1 (car (bpc:axes bp)))
             (motor2 (cadr (bpc:axes bp)))
             (motor3 (caddr (bpc:axes bp)))
             (motor4 (cadddr (bpc:axes bp)))
             (wheel-radius (/ wheel-diameter 2.))
             (angular-velocity-1 (/ (* velocity-constant e1) wheel-radius))
             (angular-velocity-2 (- (/ (* velocity-constant e2) wheel-radius)))
             )
        ;(format #t "e1 ~a e2 ~a ~%" e1 e2)
        (for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           angular-velocity-1
                                           max-impulse
                                           )) (list motor1 motor3))
        (for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           angular-velocity-2
                                           max-impulse
                                           )) (list motor2 motor4))
        #;(for-each (lambda (motor) 
                    (actuate-angular-motor motor
                                           #f)) (list motor2 motor4))
        ))
  (update-fake-state bp)
  (sim-tick (bp:sim bp) h (step-count bp)))

(define-method (step-physics (bp <bullet-physics-car-ind>) h)
  "Apply the effectors and step the physics simulation forward by h
seconds."
  (when (effector-func bp)
      (let* ((e1 ((effector-func bp) (get-time bp) 1))
             (e2 ((effector-func bp) (get-time bp) 2))
             (e3 ((effector-func bp) (get-time bp) 3))
             (e4 ((effector-func bp) (get-time bp) 4))
             (agent (car (bp:objects bp)))
             (motor1 (car (bpc:axes bp)))
             (motor2 (cadr (bpc:axes bp)))
             (motor3 (caddr (bpc:axes bp)))
             (motor4 (cadddr (bpc:axes bp)))
             (wheel-radius (/ wheel-diameter 2.)))
        ;(format #t "e1 ~a e2 ~a e3 ~a e4 ~a~%" e1 e2 e3 e4)
        (for-each (lambda (motor effector-value) 
                    (actuate-angular-motor motor
                                           (/ (* velocity-constant effector-value) wheel-radius)
                                           max-impulse
                                           )) (list motor1 motor3) (list e1 e3))
        (for-each (lambda (motor effector-value) 
                    (actuate-angular-motor motor
                                           (- (/ (* velocity-constant effector-value) wheel-radius))
                                           max-impulse
                                           )) (list motor2 motor4) (list e2 e4))
        )
      (update-fake-state bp)
      (sim-tick (bp:sim bp) h (step-count bp))))
