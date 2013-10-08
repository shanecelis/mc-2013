(define-module (brain)
  #:use-module (oop goops)
  #:use-module (minimal-cognition ctrnn)
  #:use-module ((minimal-cognition fode)
                #:renamer (symbol-prefix-proc 'fode:))
  #:use-module (beer-parameters)
  #:use-module (guile-user)
  #:export (<brain> 
            make-brain-state 
            set-brain-input! 
            step-brain! 
            init-brain-from-genome!
            make-brain-effector
            <ctrnn-brain>
            ;;ctrnn
            ;;ctrnn-state
            ))

(define-class <brain> ())

;; (make-brain-state brain)
;(define-generic make-brain-state)

;; (set-brain-input! brain input)
(define-generic set-brain-input!)

;; (step-brain! brain time-step)
(define-generic step-brain!)

(define-generic init-brain-from-genome!)

;; (make-brain-effector brain)
(define-generic make-brain-effector)

(define-class <ctrnn-brain> (<brain>)
  (ctrnn #:accessor ctrnn)
  (ctrnn-state #:accessor ctrnn-state)
  (ctrnn-state-init #:accessor ctrnn-state-init)
  )

(define (array-duplicate array)
  (let ((dup (apply make-typed-array 
                    (array-type array) 
                    *unspecified* 
                    (array-dimensions array))))
    (array-copy! array dup)
    dup))

(define-method (initialize (brain <ctrnn-brain>) initargs)
  (set! (ctrnn brain) (make-n-ctrnn node-count))
  (set! (ctrnn-state brain) (make-ctrnn-state (ctrnn brain)))
  (set! (ctrnn-state-init brain) (array-duplicate (ctrnn-state brain))))

(define-method (set-brain-input! (brain <ctrnn-brain>) input)
  (set! (input-func (ctrnn brain)) input))

(define-method (step-brain! brain time-step)
  (step-ctrnn (ctrnn-state brain) time-step (ctrnn brain)))

(define-method (make-brain-effector brain)
  (make-effector-func-unified (ctrnn-state brain))
  #;(make-c-effector-func (ctrnn-state brain))
  )

(define-method (init-brain-from-genome! (brain <ctrnn-brain>) genome)
  (genome->ctrnn genome (ctrnn brain))
  (array-copy! (ctrnn-state brain) (ctrnn-state-init brain)))
