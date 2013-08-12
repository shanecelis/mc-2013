(define-module (eval-robot)
  #:use-module (ice-9 optargs)
  #:use-module (emacsy emacsy)
  #:use-module (beer-parameters)
  #:use-module (minimal-cognition ctrnn)
  #:use-module (oop goops)
  #:use-module (physics)
  #:use-module (srfi srfi-1)
  #:export (eval-beer-robot
            eval-beer-robot-render
            eval-beer-robot-headless
            make-effector-func
            physics-class
            make-vision-func
            ))

(define make-effector-func #f)

(define make-vision-func #f)

(define physics-class #f)

(define ;(make-fode-state* fode-params)
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
  (let* ((buffer (switch-to-buffer "*eval-robot*" <physics-buffer>))
         (scene (scene buffer)))
    (define (draw fode-state)
      (when draw-display?
          (draw-physics scene fode-state)
          (if (q-empty? event-queue)
             (block-yield)
             (primitive-command-tick))))
    (eval-beer-robot-headless genome
                              #:step-fn (lambda (fode-state) 
                                          (draw fode-state) 
                                          (step-fn fode-state))
                              #:begin-fn begin-fn
                              #:end-fn end-fn
                              #:max-tick-count max-tick-count
                              #:init-ctrnn-state init-ctrnn-state)))

(define*
  (eval-beer-robot-headless genome
                   #:key 
                   (step-fn identity)
                   (begin-fn identity)
                   (end-fn identity)
                   (max-tick-count 2000)
                   (init-ctrnn-state #f))
  (let* ((ctrnn (make-n-ctrnn node-count))
         (ctrnn-state (make-ctrnn-state ctrnn))
         (effector-func (make-effector-func ctrnn-state))
         (fode (make physics-class
                 #:object-count body-count 
                 #:effector-func effector-func))
         (fode-state (fix-physics fode))
         (vision-input (make-vision-func #f ; don't draw.
                                         fode-state))
         (tick-count 0))
    (if init-ctrnn-state
     (array-copy! init-ctrnn-state ctrnn-state))
    (genome->ctrnn genome ctrnn)
    (set! (input-func ctrnn) vision-input)
    (begin-fn fode-state)
    (while (and 
            (< tick-count max-tick-count) 
            (step-fn fode-state))
      (if #t #;(= 0 (mod tick-count update-ctrnn-freq))
          (if (not (step-ctrnn ctrnn-state h ctrnn))
              (throw 'step-ctrnn-error)))
      (if (not (step-physics fode-state h))
          (throw 'step-physics-error))
      (incr! tick-count (step-count fode-state))
      #;(format #t "Tick ~a~%" tick-count))
    #;(format #t "after state ~a~%" (vector-sum ctrnn-state))
    #;(format #t "step-count ~a~%"(step-count fode-state))
    (end-fn fode-state)))

(define eval-beer-robot eval-beer-robot-headless)
