(define-module (brain)
  #:use-module (oop goops)
  #:use-module (minimal-cognition ctrnn)
  #:use-module ((minimal-cognition fode)
                #:renamer (symbol-prefix-proc 'fode:))
  #:use-module (beer-parameters)
  #:use-module (guile-user)
  #:use-module (unified-procedure)
  #:use-module (system foreign)
  #:use-module (optimize-transition)
  #:use-module (vector-math)
  #:export (<brain> 
            set-brain-input! 
            step-brain! 
            init-brain-from-genome!
            init-brain-state!
            make-brain-effector
            <ctrnn-brain>
            ;;ctrnn
            ;;ctrnn-state
            <procedure-brain>
            go-left
            go-left*
            go-right
            go-right*
            go-nowhere
            go-nowhere*
            
            make-brain
            brain-class
            
            <matrix-sandwich>
            ))

(define-class <brain> ())


;; (make-brain-state brain)
;(define-generic make-brain-state)

;; (set-brain-input! brain input)
(define-generic set-brain-input!)

(define-generic init-brain-state!)

;; (step-brain! brain time-step)
(define-generic step-brain!)

(define-generic init-brain-from-genome!)

;; (make-brain-effector brain)
(define-generic make-brain-effector)

(define-class <ctrnn-brain> (<brain>)
  (ctrnn #:accessor ctrnn)
  (ctrnn-state #:accessor ctrnn-state)
  (ctrnn-state-init #:accessor ctrnn-state-init))

(define (array->vector array)
  ;; This is terrible!
  (list->vector (array->list array)))

(define-method (display (brain <ctrnn-brain>) port)
  (apply format port "#<ctrnn-brain n ~a w ~1,1f t ~1,1f b ~1,1f g ~1,1f s ~1,1f s_i ~1,1f>"
         (n (ctrnn brain))
         (map (lambda (v) 
                (vector-mean (array->vector v)))
              (list
               (weights (ctrnn brain))
               (time-constant (ctrnn brain))
               (bias (ctrnn brain))
               (gain (ctrnn brain))
               (ctrnn-state brain)
               (ctrnn-state-init brain)))))

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

(define-method (step-brain! (brain <ctrnn-brain>) time-step)
  (if (not (step-ctrnn (ctrnn-state brain) time-step (ctrnn brain)))
      (throw 'step-ctrnn-error)))

(define-method (make-brain-effector (brain <ctrnn-brain>))
  (make-effector-func-unified (ctrnn-state brain))
  #;
  (let ((effector (make-c-effector-func (ctrnn-state brain))))
    (format #t "CREATING c effector ~a ~a~%" effector (unified-default-values effector))
                                        ;(throw 'blah)
    effector))

(define-method (init-brain-from-genome! (brain <ctrnn-brain>) genome)
  (genome->ctrnn genome (ctrnn brain))
  (init-brain-state! brain))

(define-method (init-brain-state! (brain <ctrnn-brain>))
  (array-copy! (ctrnn-state-init brain) (ctrnn-state brain)))

(define-class <procedure-brain> (<brain>)
  (brain-input #:accessor brain-input)
  (proc #:getter proc #:init-keyword #:procedure))

(define-method (set-brain-input! (brain <procedure-brain>) input)
  (set! (brain-input brain) input))

(define-method (step-brain! (brain <procedure-brain>) time-step)
  #f)

(define-method (init-brain-state! (brain <procedure-brain>))
  #f)

(define-method (make-brain-effector (brain <procedure-brain>))
  (make-unified-procedure 
   double 
   (proc brain)
   (list double int '*)))

(define-method (init-brain-from-genome! (brain <procedure-brain>) genome)
  (format #t "warning: not able to init a <procedure-brain> from genome.~%")
  #f)

(define (go-right t i . rest)
  #;(format #t "GO RIGHT ~a~%" i)
  (if (= i 1)
      1.0
      0.0))

(define (go-right* t i . rest)
  #;(format #t "GO RIGHT ~a~%" i)
  (if (= i 1)
      1.0
      -1.0))


(define (go-left t i . rest)
  #;(format #t "GO LEFT ~a~%" i)
  (if (= i 1)
      0.0 ;; or -1.0
      1.0))

(define (go-left* t i . rest)
  #;(format #t "GO LEFT ~a~%" i)
  (if (= i 1)
      -1.0 ;; or -1.0
      1.0))

(define (go-nowhere t i . rest)
  0.)

(define (go-nowhere* t i . rest)
 (if (= i 1)
     1.0 ;; or -1.0
     1.0))

(define brain-class <ctrnn-brain>)

(define (make-brain)
  (if (list? brain-class)
   (apply make brain-class)
   (make brain-class)))

(define-class <matrix-sandwich> (<brain>)
  (old-brain #:accessor old-brain #:init-keyword #:old-brain)
  (matrix-sandwich #:accessor matrix-sandwich #:init-value #f #:init-keyword #:matrix-sandwich)
  (transition-params #:accessor transition-params #:init-keyword #:transition-params)

  )

;(define-class <affine-matrix-sandwich> (<matrix-sandwich>))

(define-method (set-brain-input! (brain <matrix-sandwich>) input)
  (set-brain-input! (old-brain brain) input))

(define-method (step-brain! (brain <matrix-sandwich>) time-step)
  (step-brain! (old-brain brain) time-step))

(define-method (init-brain-state! (brain <matrix-sandwich>))
  (init-brain-state! (old-brain brain)))

(define-method (make-brain-effector (brain <matrix-sandwich>))
  (unless (matrix-sandwich brain)
    (scm-error 'invalid-matrix-sandwich "make-brain-effector" "error: no matrix-sandwich available in ~a" (list brain) #f))
  ;(format #t "Using matrix ~a~%" (matrix-sandwich brain))
  (make-transition-params-effector
   (transition-params brain)
   (make-brain-effector (old-brain brain))
   (matrix-sandwich brain)))

(define-method (init-brain-from-genome! (brain <matrix-sandwich>) genome)
  (if (matrix-sandwich brain)
      (array-copy! genome (matrix-sandwich brain))
      (set! (matrix-sandwich brain) (array-duplicate genome))))

(define-method (display (brain <matrix-sandwich>) port)
  (format port "#<matrix-sandwich M ~a tp ~a old-brain ~a>"
         (matrix-sandwich brain)
         (transition-params brain)
         (old-brain brain)))
