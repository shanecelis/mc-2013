(define-module (eval-robot)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 q)
  #:use-module (emacsy emacsy)
  #:use-module (beer-parameters)
  #:use-module (minimal-cognition ctrnn)
  #:use-module (oop goops)
  #:use-module (physics)  
  #:use-module (physics-buffer)
  #:use-module (physics-ui)
  #:use-module (srfi srfi-1)
  #:use-module (scene-smob)
  #:use-module (guile-user)
  #:use-module (phenotype)
  #:use-module (brain)
  ;#:use-module ((guile-user) #:select (reset-camera))
  #:export (
            ;; Procedures
            eval-beer-robot-render
            eval-beer-robot-headless
            fix-physics
            
            ;; Variables (Settable)
            physics-class
            make-vision-func
            eval-beer-robot
            
            ))

(define make-vision-func #f)

(define physics-class #f)

(define
  (fix-physics physics)
  "Make an FODE state and initialize it to some fixed values."
  (let ((ty physics))
    ;; agent
    (set! (object-x ty 0) 0.)
    (set! (object-y ty 0) 0.)
    ; NEVER set the initial velocity, it's actually a parameter for the input.
    (set! (object-vx ty 0) 0.)
    (set! (object-vy ty 0) 0.)
    (agent-motor-constant-set! ty motor-constant)

    ;; object positions
    (for-each (lambda (i)
                (set! (object-x ty i) (+ i -3.))
                (set! (object-y ty 1) max-height))
              (iota (1- (object-count physics)) 1))
    physics))

(define*
  (eval-beer-robot-render genome
                          #:key 
                          (step-fn identity)
                          (begin-fn identity)
                          (end-fn identity)
                          (max-tick-count 2000)
                          (init-ctrnn-state #f))
  ;(undraw-physics (current-scene) fode)
  (let* ((buffer (switch-to-buffer "*eval-robot*" <physics-buffer>))
         (scene (scene buffer)))
    (define (init-undraw)
      (undraw-vision-lines))
    (define (draw fode-state)
      (reset-camera)
      (draw-physics scene fode-state)
      (if (q-empty? event-queue)
          (block-yield)
          (primitive-command-tick)))
    (define (undraw fode-state)
      (undraw-physics scene fode-state))
    (eval-beer-robot-headless genome
                              #:step-fn (lambda (fode-state) 
                                          (draw fode-state) 
                                          (step-fn fode-state))
                              #:begin-fn (lambda (fode-state) 
                                           (init-undraw)
                                           (begin-fn fode-state))
                              #:end-fn (lambda (fode-state) 
                                         (undraw fode-state)
                                         (end-fn fode-state))
                              #:max-tick-count max-tick-count
                              #:init-ctrnn-state init-ctrnn-state
                              #:draw-vision-lines? #t)))


(define*
  (eval-beer-robot-headless genome
                   #:key 
                   (step-fn identity)
                   (begin-fn identity)
                   (end-fn identity)
                   (max-tick-count 2000)
                   (init-ctrnn-state #f)
                   (draw-vision-lines? #f))
  (let* ((ctrnn (make-brain))
         ;(_ (init-from-genome! ctrnn genome))
         (effector-func (make-brain-effector ctrnn))
         ;; fode and fode-state are the same thing now.
         ;; sorry for the confusion.
         (fode (make physics-class
                 #:object-count body-count 
                 #:effector-func effector-func))
         (phenotype (make <composite-phenotype> 
                      #:phenotypes (list ctrnn fode)))
         (fode-state (begin 
                       (init-physics fode)
                       (fix-physics fode)))
         (vision-input (make-vision-func 
                        draw-vision-lines?
                        fode-state))
         (tick-count 0))
    ;(format #t "Using brain ~a~%" ctrnn)
    ;(format #t "Using phenotype ~a~%" phenotype)
    (init-from-genome! phenotype genome)
    (init-brain-state! ctrnn)
    (set-brain-input! ctrnn vision-input)
    (begin-fn fode-state)
    (while (and 
            (< tick-count max-tick-count) 
            (step-fn fode-state))
      (if #t #;(= 0 (mod tick-count update-ctrnn-freq))
          (step-brain! ctrnn h))
      (if (not (step-physics fode-state h))
          (throw 'step-physics-error))
      (incr! tick-count (step-count fode-state))
      #;(format #t "Tick ~a~%" tick-count))
    #;(format #t "after state ~a~%" (vector-sum ctrnn-state))
    #;(format #t "step-count ~a~%"(step-count fode-state))
    (end-fn fode-state)))

(define eval-beer-robot eval-beer-robot-headless)

(define-interactive (toggle-render)
  (if (eq? eval-beer-robot eval-beer-robot-render)
      (set! eval-beer-robot eval-beer-robot-headless)
      (set! eval-beer-robot eval-beer-robot-render)))
