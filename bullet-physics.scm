;; bullet-beer.scm
;;
;; We want to have a few things that work just like the FODE
;; that we've setup in beer-experiment.  They are:
;; (define fode-params (make-fode body-count effector))
;; (define fode-state (make-fode-state fode-params))
;; (object-x fode-state i) ;; x-position of object i
;; (object-y fode-state i) ;; y-position of object i
;; (object-vx fode-params i) ;; x-speed of object i
;; (object-vy fode-params i) ;; y-speed of object i
;; (step-fode fode-state h fode-params) ;; move the simulation forward
;; (draw-fode scene fode-state) ;; draw the scene
;;
;; We want the same interface but s/fode/bullet/.  

#;(define (make-bullet body-count effector)
  )

(define-module (bullet-physics)
  #:use-module (oop goops)
  #:use-module (bullet)
  #:use-module (beer-parameters)
  #:use-module (vector-math)
  #:use-module (physics)
  #:use-module (emacsy util)
  #:use-module (scene-smob)
  #:use-module (physics-ui)
  #:use-module (physics-buffer)
  #:use-module (guile-user)
  
  #:export (bp:sim bp:objects update-fake-state)
 )

(define-class-public <bullet-physics> (<physics>)
  (sim #:accessor bp:sim #:init-keyword #:sim #:init-form (make-sim) #;(current-sim))
  (objects #:accessor bp:objects #:init-value '())
  (fake-state #:getter fp:state #:init-value #f)
  (force-constant)
  )

;; Agent and Objects need to be placed in separate collision groups.
;; http://bulletphysics.org/Bullet/BulletFull/classbtDiscreteDynamicsWorld.html#a8636fbb78c9d0d730262db987201840a
(define-public agent-group 128)
(define-public object-group 64) 
(define-public wheel-group 256) 
(define-public floor-group 3)                  ; StaticFilter
(define-method (initialize (bp <bullet-physics>) initargs)
  (define (process-agent agent)
    (set-friction! agent 0.0001)
    (sim-add-body (bp:sim bp) 
                  agent
                  agent-group           ; is-a
                  (logior 
                   floor-group)        ; collides with
                  )
    )
  (define (process-object body)
    (set-friction! body 0.0001)
    (sim-add-body (bp:sim bp) 
                  body
                  object-group          ; is-a
                  (logior 
                   floor-group)         ; collides with
                  ))
  (next-method)
  (sim-add-ground-plane2 (bp:sim bp))
  (set-time! bp 0.)
  (let ((body (make-box #(0 10 0) 
                        (vector agent-diameter
                                1 
                                agent-diameter) 1. "agent")))
    (set! 
     (bp:objects bp) 
     (cons body 
           (map 
            (lambda (i)
              (make-box (vector (- i 2.) 0 (- max-height)) 
                        (vector object-diameter 
                                1 
                                object-diameter) 
                        1. 
                        (format #f "object ~d" i)))
            (range 1 (1- (object-count bp)))))))
  (process-agent (car (bp:objects bp)))
  (for-each process-object (cdr (bp:objects bp)))
  
  ;; Make a fake state vector to interact with the c-vision-input
  (slot-set! bp 'fake-state (make-typed-array 'f64 0.0 (1+ (* 2 (object-count bp))))))

(define-method (update-fake-state (bp <bullet-physics>))
  "Update this fake state vector as a means of communicating with the
CTRNN."
  (array-set! (fp:state bp) (get-time bp) 0)
  (for-each (lambda (i)
              (array-set! 
               (fp:state bp) (object-x bp i) (+ (* 2 i) 1))
              (array-set! 
               (fp:state bp) (object-y bp i) (+ (* 2 i) 2)))
            (range 0 (1- (object-count bp)))))

(define-method (draw-physics scene (bp <bullet-physics>))
  "Draws the bullet physics simulation."
  (if (not (scene-update-physics scene (bp:sim bp)))
      (scene-add-physics scene (bp:sim bp))))

(define-method (undraw-physics scene (bp <bullet-physics>))
  "Draws the bullet physics simulation."
  (if scene 
      (scene-clear-physics scene)))

(define-method (get-time (bp <bullet-physics>))
  (sim-time (bp:sim bp)))

(define-method (set-time! (bp <bullet-physics>) t)
  (sim-time-set! (bp:sim bp) t))

(define-method (step-physics (bp <bullet-physics>) h)
  "Apply the effectors and step the physics simulation forward by h
seconds."
  (if (effector-func bp)
      (let* ((e1 ((effector-func bp) (get-time bp) 1))
             (e2 ((effector-func bp) (get-time bp) 2))
             (de (- e1 e2))
             (v (object-vx bp 0))
             (agent (car (bp:objects bp))))
        (apply-force agent 
                     (vector 
                      (* (agent-motor-constant-ref bp) (- de v)) 
                      0. 
                      0.) 
                     #(0. 0. 0.))))
  (update-fake-state bp)
  (throw 'sim-tick (bp:sim bp) h (step-count bp)))

(define-method (reset-physics (bp <bullet-physics>))
  (set-time! bp 0.)
  (map (lambda (body)
         (sim-remove-body (bp:sim bp) body)) 
       (bp:objects bp))
  (set! (bp:objects bp) '())
  (if (current-scene)
   (scene-clear-physics (current-scene))))

(define x-axis 0)
(define y-axis 2) ;; The y-axis for the experiment is the z-axis for
                  ;; Bullet.

(define-method (object-x-ref (bp <bullet-physics>) i)
  (let ((p (get-position (list-ref (bp:objects bp) i))))
    (vector-ref p 0)))

(define-method (object-y-ref (bp <bullet-physics>) i)
  (let ((p (get-position (list-ref (bp:objects bp) i))))
    (- (vector-ref p y-axis))))

(define-method (object-x-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-position body)))
    (vector-set! p 0 v)
    (set-position! body p)))

(define-method (object-y-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-position body)))
    (vector-set! p y-axis (- v))
    (set-position! body p)))

(define-method (object-vx-ref (bp <bullet-physics>) i)
  (let ((p (get-velocity (list-ref (bp:objects bp) i))))
    (from-velocity (vector-ref p 0))))

(define-method (object-vy-ref (bp <bullet-physics>) i)
  (let ((p (get-velocity (list-ref (bp:objects bp) i))))
    (- (from-velocity (vector-ref p y-axis)))))

(define-method (agent-motor-constant-set! (bp <bullet-physics>) k)
  (slot-set! bp 'force-constant k))

(define-method (agent-motor-constant-ref (bp <bullet-physics>))
  (slot-ref bp 'force-constant))

;; XXX fudge factor
(define velocity-factor 1.)
(define (to-velocity v)
  (* velocity-factor v))

(define (from-velocity v)
  (/ v velocity-factor))

(define-method (object-vx-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-velocity body)))
    (vector-set! p 0 (to-velocity v))
    (set-velocity! body p)))

(define-method (object-vy-set! (bp <bullet-physics>) i v)
  (let* ((body (list-ref (bp:objects bp) i))
        (p (get-velocity body)))
    (vector-set! p y-axis (- (to-velocity v)))
    (set-velocity! body p)))


(define eval-robot-render-speed 1)

(define (sim-add-ground-plane2 sim)
  (sim-add-fixed-box sim #(0. -10. 0.) #(400. 20. 400.) 0.0001))

#;(define (my-physics-tick)
  (for-each 
   (lambda (physics-buffer)
     (with-buffer 
      physics-buffer
      (unless (paused? physics-buffer)            
        ;; Update the simulation.
        (repeat eval-robot-render-speed
                (sim-tick (current-sim))
                ;; Do we want multiple bodies shown?
                ;; (when (= 0 (mod (tick-count robot) 120))
                ;;   (physics-add-scene (current-sim)))
                )
        ;; Update the visualization.
        (scene-update-physics (current-scene) (current-sim)))))
   (filter (cut is-a? <> <physics-buffer>) (buffer-list))))

;(define bp #f)
#;(define (bullet-physics-tick)
  (let* ((scene (current-scene)) ;; This should be attached to the buffer.
         (restart? #t))
    (when scene
      
      (step-physics bp h)
      #;(step-ctrnn ctrnn-state h ctrnn)
      (draw-physics scene bp))
      
      ;; Check if we should restart the simulation.
      (for-each 
       (lambda (i)
         (if (> (object-y bp i) 0)
             (set! restart? #f))) 
       (range 1 (1- (object-count bp))))

      (when restart?
        ;; restart
        (reset-physics))))

;(define robot #f)
#;(add-hook! post-window-open-hook 
           (lambda ()
             (set! bp (make-instance <bullet-physics> #:object-count 2))
             (set! (effector-func bp) go-left)
             
             (let ((middle (/ max-height 2.)))
              (set-parameter! 'camera-position 
                              #;(vector 0. (* max-height 1.5) (- middle))
                              (vector 0.0 260.0 -82.0))
              
              (set-parameter! 'camera-target 
                              (vector 0.0 0.0   (- middle)))))) 


#;(add-hook! physics-tick-hook #.\ (my-physics-tick))
