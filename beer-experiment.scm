;; beer-experiment.scm
;;
;; replication of the selecive attention experiment
(use-modules 
 (beer-parameters)
 (beer-initial-conditions)
 (minimal-cognition ctrnn)
 ((minimal-cognition fode)
             #:renamer (symbol-prefix-proc 'fode:))
 (minimal-cognition vision)
 (stats)
 (unified-procedure)
 (system foreign)
 (nsga-ii) 
 (fitness)
 (infix)
 (srfi srfi-1) ;; append-map
 (srfi srfi-19)
 (srfi srfi-8) ;; receive
 (srfi srfi-11) ;; let-values
 (freeze-random)
 (mathematica)
 (ice-9 q)
 (ice-9 match)
 (emacsy emacsy)
 (emacsy core)
 (oop goops)
 (fode-physics)
 (physics)
 (bullet-physics)
 (bullet-physics-car)
 (bullet-physics-skateboard)
 (vector-math)
 (physics-buffer)
 (guile-user)
 (physics-ui)
 (scene-smob)
 (mathematica-aux)
 (camera)
 (optimize-transition)
 (experiment-transition)
 (eval-robot))

(set! physics-class <fode-physics>)
;(define physics-class <bullet-physics>)
;(define physics-class <bullet-physics-car>)


(define-interactive (toggle-physics)
  (if (eq? physics-class <fode-physics>)
      (set! physics-class <bullet-physics-car>)
      (set! physics-class <fode-physics>))
  (reset-fode))

(define (make-cycle list func)
  (let ((i 0)
        (v (list->vector list)))
    (lambda* (#:optional (incr 1))
             (format #t "BEFORE i = ~a~%" i)
             (set! i (modulo (+ i incr) (vector-length v)))
             (format #t "i = ~a~%" i)
             (func (vector-ref v i)))))

(define temp (make-cycle
              (list <fode-physics> <bullet-physics> <bullet-physics-car> <bullet-physics-car-ind> <bullet-physics-skateboard> <bullet-physics-skateboard-1> <bullet-physics-skateboard-2>)
              (lambda (x)
                (message "Switched to physics class ~a." x)
                (set! physics-class x)
                (reset-fode)
                )))

;; XXX There is a bug here because I can't get rid of temp.  The macro
;; won't behave.  Adding new define-*s is tricky, maybe not worth the
;; effort.  Down with things!  Down with commands!  Lambdas forever!
(define-interactive cycle-physics
  temp)

(define-key global-map (kbd "c") 'cycle-physics)


(define ctrnn (make-n-ctrnn node-count))
(define ctrnn-state (make-ctrnn-state ctrnn))

(define init-ctrnn-state (make-ctrnn-state ctrnn))
;; XXX Let's not randomize just yet.
;(randomize-ctrnn-state! init-ctrnn-state)


;; https://gist.github.com/valvallow/413146
(define-syntax dotimes
  (syntax-rules ()
    ((_ n body ...)
     (do ((i n (- i 1)))
         ((not (< 0 i)))
       body ...))))

(define-syntax record-time
  (syntax-rules ()
    ((_ var body)
     (begin 
       (let ((start (current-time)))
         body
         (set! var (- (current-time) start)))))))

(define-syntax record-time*
  (syntax-rules ()
    ((_ var body)
     (begin 
       (let ((start (current-time time-monotonic)))
         body
         (set! var (let* ((diff (time-difference! (current-time time-monotonic) start))
                          (seconds (time-second diff))
                          (nanoseconds (time-nanosecond diff)))
                     (time->float diff)
                     )))))))

(define (time->float time)
  (string->number (format #f "~a.~a" (time-second time) (time-nanosecond time))))

(define (make-effector-func-proc ctrnn-state)
  (lambda (t i . rest)
    (let* ((first-effector-index (1+ sensor-count))
           (state (: ctrnn-state @ (first-effector-index + (i - 1)))))
      #;(format #t "ctrnn state ~a~%" state)
      (tanh state))))

(define (make-effector-func-unified ctrnn-state)
  (let ((proc (make-effector-func-proc ctrnn-state)))
    (make-unified-procedure double 
                            proc
                            (list double int '*))))

(define (make-c-effector-func ctrnn-state)
  (make-unified-procedure double 
                          fode:c-ctrnn-effector
                          (list double int (list '* (bytevector->pointer ctrnn-state)))))




;; func-proc no longer works.
;(define make-effector-func make-effector-func-proc)
(set! make-effector-func make-effector-func-unified)

;(define make-effector-func make-c-effector-func)

(define fode #f)



;; XXX huh, have to be careful about when define-method-public is
;; called; here using it instead of define-method break it.
(define-method (emacsy-mode-line (buffer <physics-buffer>))
  (if fode-state
      (format #f "~a sim-time ~1,1f agent (~1,1f, ~1,1f)~{ object (~1,1f, ~1,1f)~}"
              (next-method)
              (get-time fode-state)
              (object-x fode-state 0)
              (object-y fode-state 0)
              (append-map (lambda (i)
                            (list (object-x fode-state i)
                                  (object-y fode-state i)))
                          (range 1 (1- body-count))))
      (next-method)))

(define (in-range? x list)
  (and (>= x (car list))
       (<= x (cadr list))))


(define (beer-choose-initial-conditions fode-params fode-state)
  "This is how beer chooses his initial conditions for 1 or 2 objects."
  ;; Pick the initial x1 and v1
  (let ((ty (fp:state fode-state))
        (n (object-count fode-params))
        #;(k  (fp:k-params fode-params)))
      ;; Set the heights to the same thing.
    ;;(format #t "choosing ICs~%")
    ;; object 1 yi
    (set! (object-y fode-params 1) max-height)
    ;; object 1 position
    (set! (object-x fode-params 1) (apply random-range horizontal-position))
    ;; object 1 velocity
    (set! (object-vx fode-params 1) (apply random-range horizontal-velocity))
    (set! (object-vy fode-params 1) (apply random-range vertical-velocity-1))
    
    ;(format #t "N is ~a~%" n )
    (when (= n 3)
      ;; object 2 yi
      (set! (object-y fode-state 2) max-height)
      ;; Compute x1
      (let* ((t1 (/ 
                  #;(: ty @ 4) 
                  (object-y fode-state 1)
                  #;(: k @ 3)
                  (object-vy fode-state 1)))
             (x1 (+ #;(: ty @ 3) 
                  (object-x fode-state 1)
                  (* #;(: k @ 4) (object-vx fode-state 2)
                                  t1)))
             (v2y (apply random-range vertical-velocity-2))
             (t2 (/ #;(: ty @ 6) (object-y fode-state 2) v2y))
             (beta (* motor-constant alpha (abs (- t1 t2))))
             (x2 (random-range (- x1 beta) (+ beta x1)))
             (v2x (apply random-range horizontal-velocity))
             (x2i (- x2 (* v2x t2)))
             )
        ;; object 2 position
        (set! (object-x fode-state 2) x2i)
        ;; (: ty @ 5 := x2i)
        ;; object 2 velocity
        (set! (object-vx fode-state 2) v2x)
        (set! (object-vy fode-state 2) v2y)
        ;; (: k @ 4 := v2x) ;; vx
        ;; (: k @ 5 := v2y) ;; vy
        (if (in-range? x2i horizontal-position)
            (message "Initial conditions: object 1 r (~1,1f, ~1,1f) v (~1,1f, ~1,1f) object 2 r (~1,1f, ~1,1f) v (~1,1f, ~1,1f)" 
                     ;(: ty @ 3) (: ty @ 4) 
                     (object-x fode-state 1) (object-y fode-state 1) 
                     
                     ;(: k @ 2) (: k @ 3) 
                     (object-vx fode-state 1)  (object-vy fode-state 1)
                     ;(: ty @ 5) (: ty @ 6) 
                     (object-x fode-state 2) (object-y fode-state 2) 
                     ;(: k @ 4) (: k @ 5)
                     (object-vx fode-state 2)  (object-vy fode-state 2)
                     )
            (beer-choose-initial-conditions fode-params fode-state))))))

(define (case-1-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) 0.)
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-2-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) (car horizontal-velocity))
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-3-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (if (> n 2)
        (throw 'invalid-object-count n))
    (set! (object-x fode-state 1) 0.)
    (set! (object-y fode-state 1) max-height)
    (set! (object-vx k 1) (cadr horizontal-velocity))
    (set! (object-vy k 1) (car vertical-velocity-1))))

(define (case-4-IC fode-params fode-state)
  (let ((n (object-count fode-params))
        (k fode-params))
    (set! (object-x fode-state 0) 0)
    (set! (object-y fode-state 0) 0)
    (set! (object-vx fode-state 0) 0)
    (set! (object-vy fode-state 0) 0)
    (for-each (lambda (i)
                (set! (object-x fode-state i) 
                      (apply random-range horizontal-position))
                (set! (object-y fode-state i) max-height)

                (set! (object-vx k i) (apply random-range horizontal-velocity))
                (set! (object-vy k i) (apply random-range vertical-velocity-1)))
              (range 1 (1- n)))))

(define (make-parametric-IC position-difference)
  (lambda (fode-params fode-state)
   (let ((n (object-count fode-params))
         (k fode-params))
     (for-each (lambda (i)
                 (set! (object-x fode-state i) (* (1- i) position-difference))
                 (set! (object-y fode-state i) max-height)
                 (set! (object-vx k i) 0.)
                 (set! (object-vy k i) (min -1. (+ (* (1- i) 2.) (cadr vertical-velocity-1)))))
               (range 1 (1- n))))))





;(define choose-initial-conditions beer-choose-initial-conditions)
;(define choose-initial-conditions (make-freeze-random beer-choose-initial-conditions))
;(define choose-initial-conditions (make-parametric-IC 50.))
(define choose-initial-conditions (make-apply-IC (left-IC)))

(define-interactive (change-IC)
  (set! choose-initial-conditions (make-freeze-random beer-choose-initial-conditions)))

(define initial-conditions-index 0)
(define-interactive (next-IC)
  (set! initial-conditions-index (mod (1+ initial-conditions-index) (length initial-conditions)))
  (set! choose-initial-conditions (list-ref initial-conditions initial-conditions-index)))

(define fode-state #f)

(define vision-line-actors #f)
(define vision-line-actors-index 0)
(define (draw-vision-lines agent-position end-point)
  #;(format #t "draw-vision-lines ~a ~a~%" agent-position end-point)
  (let ((scene (current-scene)))
    (when (or (not vision-line-actors) 
              (not (= (vector-length vision-line-actors) sensor-count)))
      ;; initialize the actors
      (set! vision-line-actors (make-vector sensor-count #f))
      (set! vision-line-actors-index 0))
    (when scene
      #;(format #t "draw-vision-lines add-line ~a ~a~%" agent-position end-point)
      (if (not (: vision-line-actors @ vision-line-actors-index))
          (vector-set! vision-line-actors vision-line-actors-index 
                       (add-line scene (list (vector-append agent-position #(0.))
                                             (vector-append end-point #(0.)))))
          (update-line (: vision-line-actors @ vision-line-actors-index)
                       (list (vector-append agent-position #(0.))
                             (vector-append end-point #(0.)))))
      (set! vision-line-actors-index 
            (mod (1+ vision-line-actors-index) sensor-count)))))

(define (undraw-vision-lines)
  (let ((scene (current-scene)))
   (when (and scene vision-line-actors)
     (vector-for-each (lambda (actor) (remove-actor scene actor)) vision-line-actors)
     (set! vision-line-actors #f) 
     (set! vision-line-actors-index 0))))

(define current-genome (make-genome-for-n-ctrnn node-count))
(define gene-count (array-length current-genome))

(add-hook! physics-tick-hook #.\ (fode-physics-tick))
(define tick-count 0)


;; Instead of just having local variables, shouldn't we just have
;; buffer variables?

(define draw-display? #t)
(define pause-fode? #f)

(define-interactive (toggle-draw-display)
  (set! draw-display? (not draw-display?)))

(define-key eracs-mode-map (kbd "d") 'toggle-draw-display)

(define-interactive (toggle-pause-fode)
  (set! pause-fode? (not pause-fode?)))

(define-key eracs-mode-map (kbd "p") 'toggle-pause-fode)

(define (fode-physics-tick)
  (let* ((scene (current-scene)) ;; This should be attached to the buffer.
         (restart? #t))
    (when  (and scene fode-state)
      (unless pause-fode?
        (if (not (step-physics fode-state h))
            (throw 'step-physics-error))
        (when #t #;(= 0 (mod tick-count update-ctrnn-freq))
          #;(if draw-display?
              (for-each (lambda (actor) (remove-actor scene actor)) vision-line-actors))
          (step-ctrnn ctrnn-state h ctrnn)))
      (if draw-display?
          (draw-physics scene fode-state))
      
      ;; Check if we should restart the simulation.
      (for-each 
       (lambda (i)
         (if (> (object-y fode-state i) 0)
             (set! restart? #f))) 
       (range 1 (1- body-count)))

      (when restart?
        ;; restart
        (next-IC)
        (reset-fode)))
    (incr! tick-count)))

(define last-vision-values '())

(define (make-collect-vision-values vision-input)
  (set! last-vision-values '())
  (let ((last-time #f))
   (lambda (t i)
     (let ((output (vision-input t i)))
       (when (< i sensor-count)
           (if (or #t (not (= output 10.)))
               (format #t "output not 10 but ~a at time ~a for sensor ~a~%" output t i))
           (when (not (equal? last-time t))
             #;(format #t "Adding another vector for time ~a~%" t)
             (cons! (cons t (make-vector sensor-count 0.)) last-vision-values)
             (set! last-time t))
           (vector-set! (cdar last-vision-values) i output))
      output))))



(define* (vision-values->points vision-values #:optional (sensor-index #f))
    ;; We have a list of ((t1 . #(v11 v12 ...)) (t2 . #(v21 v22
    ;; ... ))) that we want to transform into '((t1 v11) (t2 v21) ...)
    ;; for sensor 1 or a list of all lists if no sensor is given.
    (if sensor-index
        (map (lambda (entry)
               (list (car entry) (+ (* 10 sensor-index) (vector-ref (cdr entry) sensor-index))))
             vision-values)
        (map (lambda (index)
               (vision-values->points vision-values index))
             (range 0 (1- sensor-count)))))
  
(define-interactive (show-vision-values #:optional (values last-vision-values))
  (line-plot (vision-values->points values)
             #:axes-label '("time" "sensor value")
             #:plot-legends (map number->string (range 1 sensor-count))
             #:plot-range 'Full
             ))

(define-interactive (show-input-difference #:optional (genome current-genome))
  
  (let ((prev-record record-vision-values?)
        (prev-class physics-class)
        (prev-values last-vision-values))
    
    (in-out
     (begin
       (set! record-vision-values? #t)
       (set! physics-class <fode-physics>)
       (set! last-vision-values '()))
     (let ((fode-values #f)
           (bullet-values #f)
           (diff #f)
           (points #f)
           (tick-count 600))

       (eval-beer-robot genome #:max-tick-count tick-count)
       (set! fode-values (reverse last-vision-values))
       (line-plot (vision-values->points fode-values)
                  #:axes-label '("time" "sensor value")
                  #:plot-label "FODE"
                  #:plot-legends (map number->string (range 1 sensor-count))
                  #:plot-range '(Automatic '(-.5 10.5)) #;'((0 10) (-.2 12))
                  )
       (set! physics-class <bullet-physics-car>)
       (eval-beer-robot genome #:max-tick-count tick-count)
       (set! bullet-values (reverse last-vision-values))
       (line-plot (vision-values->points bullet-values)
                  #:axes-label '("time" "sensor value")
                  #:plot-label "Bullet"
                  #:plot-legends (map number->string (range 1 sensor-count))
                  )

       (set! diff (map vector-sum (map vector-abs (map vector- 
                                                       (map cdr fode-values)
                                                       (map cdr bullet-values)))))
       (set! points (map list (map car fode-values) diff))
       (line-plot points #:axes-label '("time" "sum of abs difference")
                  #:plot-label "Sensor differences"
                  ))
     (begin
       (set! record-vision-values? prev-record)
       (set! physics-class prev-class)
       (set! last-vision-values prev-values)))))

(define* (make-current-vision-input #:optional (draw? #t) (my-fode-state fode-state))
  (if (and use-c-vision? (not record-vision-values?))
            ;; Here's how to use the C implementation of vision.
      (make-unified-procedure 
       int 
       vision-input-pointer
       (list double 
             int 
             (list '*
                   (make-c-vision-context
                    (fp:state my-fode-state)
                    (1- body-count) ;; object count
                    sensor-count
                    ;; We're going to treat the
                    ;; agent-diameter as though it
                    ;; is zero for vision
                    ;; purposes.
                    0 
                                        ;agent-diameter
                    (/ object-diameter 2)
                    max-sight-distance
                    visual-angle
                    max-sight-output
                    (and draw? draw-vision-lines))) 
             (list '* %null-pointer)))
      
      
      (let ((scheme-vision (make-vision-input (fp:state my-fode-state)
                                                         (1- body-count) ;; object count
                                                         sensor-count
                                                         ;; We're going to treat the
                                                         ;; agent-diameter as though it
                                                         ;; is zero for vision
                                                         ;; purposes.
                                                         0 
                                        ;agent-diameter
                                                         (/ object-diameter 2)
                                                         max-sight-distance
                                                         visual-angle
                                                         max-sight-output
                                                         (and draw? draw-vision-lines))))
        (if record-vision-values?
            ;; Let's use the old lisp version, because it's easier to collect the
            ;; input.
            (make-collect-vision-values scheme-vision)
            scheme-vision))))

(set! make-vision-func make-current-vision-input)

(define-interactive (reset-fode)
  (genome->ctrnn current-genome ctrnn)
  ;(randomize-ctrnn-state! ctrnn-state)
  (array-copy! init-ctrnn-state ctrnn-state)
  (undraw-vision-lines)
  (when fode
    (undraw-physics (current-scene) fode)
    (reset-physics fode))
  (set! fode (make physics-class
               #:object-count body-count 
               #:effector-func 
               (make-effector-func ctrnn-state)
               
               ;; XXX I broke the old interface.  Now it _requires_ a unified-procedure.
               ;; Not what I intended.
#;               (make-unified-procedure 
                 double 
;               go-nowhere
;                 go-left
                 go-right
                 (list double int '*))
               
               ))
  
  (set! fode-state (fix-physics fode))
  (choose-initial-conditions fode fode-state)
  (set! (input-func ctrnn) (make-current-vision-input))
  (set! vision-line-actors #f))

(define (go-right t i . rest)
  (if (= i 1)
      1.0
      -1.0))

(define (go-left t i . rest)
  ;(format #t "GO LEFT~%")
  (if (= i 1)
      0.0 ;; or -1.0
      1.0))

(define (go-nowhere t i)
  0.)


(define-interactive (randomize-brain)
  (randomize-genome! current-genome)
  (genome->ctrnn current-genome ctrnn)
  (set! (input-func ctrnn) (make-current-vision-input))
  (randomize-ctrnn-state! ctrnn-state))

(define-interactive (reset-camera)
  (set-parameter! 'camera-position (vector 0 (/ max-height 2) 300))
  (set-parameter! 'camera-target (vector 0 (/ max-height 2) 0))
  (set-parameter! 'camera-up #(0 1 0)))

(add-hook! post-window-open-hook 
           (lambda ()
             (reset-camera)
             (randomize-genome! current-genome)
             (reset-fode)) 
           #t)

(define-interactive
  (graph-distance-over-time #:optional (genome current-genome))
    (let ((distances '()))
    ;; Set the agent body to a distance of zero.
    (define (step-func fode-state)
      (cons! (cons (- (get-time fode-state)) 
                   (map 
                    (lambda (i)
                      (- (object-x fode-state i) 
                         (object-x fode-state 0)))
                    (range 1 (1- body-count)))) 
             distances)
      
      ;; Return true if we have distances set for all objects.
      (not (every (lambda (i) (< (object-y fode-state i) 0)) (range 1 (1- body-count)))))
        
    (eval-beer-robot genome 
                     #:step-fn step-func)
    (mathematica (format #f "ListPlot[~a, AxesLabel -> {\"delta d\", \"t\"}, PlotRange -> {{-100, 100}, Automatic}, Epilog -> {{Dashed, Line@{{~a, 0}, {~:*~a, -200}}}, {Dashed, Line@{{-~:*~a, 0}, {-~:*~a, -200}}}}];" 
                         (sexp->mathematica (reverse! (map reverse distances))) 
                         successful-distance))))

(define-fitness
  ((minimize "Distance to objects"))
  (beer-selective-attention #:optional (genome current-genome)
                            (initial-condition choose-initial-conditions))
  (let ((d (make-vector body-count #f))
        (fitness #f))
    (define (begin-func fode-state)
      (if initial-condition
       (initial-condition fode-state fode-state)))
    ;; Set the agent body to a distance of zero.
    (define (step-func fode-state)
      (for-each 
       (lambda (i)
         (when (and (not (vector-ref d i)) 
                    (< (object-y fode-state i) 0))
           (vector-set! d i (- (object-x fode-state i) 
                               (object-x fode-state 0)))))
       (range 1 (1- body-count)))
      
      ;; Return true if we have distances set for all objects.
      (not (vector-every identity d)))
    (define (end-func fode-state)
      ;(format #t "d = ~a~%" d)
      (if (vector-every identity d)
          (vector (max-of-vector (vector-map abs d)))
          #;(throw 'invalid-fitness)
          (vector 10000) ;; Big number for terrible fitness.
          ))
    ;; We set the distance for the agent (object 0) to 0.
    (vector-set! d 0 0.)
        
    (set! fitness (eval-beer-robot genome 
                                   #:begin-fn begin-func
                                   #:step-fn step-func 
                                   #:end-fn end-func))
    (message "Fitness ~a." fitness)
    fitness))

;(define initial-conditions (list case-1-IC case-2-IC case-3-IC))

;(define initial-conditions (list choose-initial-conditions))
(define initial-conditions (map make-apply-IC (list (left-IC) (right-IC))))

(define-interactive (add-IC)
  (cons! (make-freeze-random beer-choose-initial-conditions) initial-conditions))

(define-fitness
  ((minimize "the maximum distance to objects for a set of initial conditions."))
  (beer-selective-attention-n 
   #:optional 
   (genome current-genome)
   (my-initial-conditions initial-conditions))
  
  (let* ((initial-conditions (run-if-thunkable my-initial-conditions))
         (trials (length initial-conditions))
         (fitnesses '()))
    
    (do ((i 1 (1+ i)))
        ((> i trials))
      (cons! (vector-ref (beer-selective-attention 
                          genome
                          (list-ref initial-conditions (1- i))) 
                         0) fitnesses))
   ;; is max ok here?
   (let ((total-fitness (vector (apply max fitnesses)
                                #;(mean fitnesses))))
     (message "Aggregate/Max fitness ~a." total-fitness)
     total-fitness
     )))

(define-fitness
  ((minimize "the distance to left object.")
   (minimize "the distance to right object."))
  (left-right-task
   #:optional 
   (genome current-genome)
   (my-initial-conditions initial-conditions)
   (transition-genome #f))
  
  (let* ((initial-conditions (run-if-thunkable my-initial-conditions))
         (trials (length initial-conditions))
         (fitnesses '())
         (last-make-effector-func make-effector-func)
         )
    (when transition-genome
      (set! make-effector-func 
            (make-make-transition-effector-func
             (make-transition-params 2 2 #f)
             transition-genome)))
    (do ((i 1 (1+ i)))
        ((> i trials))
      (cons! (vector-ref (beer-selective-attention 
                          genome
                          (list-ref initial-conditions (1- i))) 
                         0) 
             fitnesses))
    (when transition-genome
      (set! make-effector-func last-make-effector-func))
   (let ((total-fitness (list->vector fitnesses)))
     (message "Fitness ~a." total-fitness)
     total-fitness)))

(define last-fitness-func #f) 
(define last-results #f)

(define successful-distance (+ (/ object-diameter 2)
                                (/ agent-diameter  2)))

(define (individual-succeeded? fitness)
  "Did the individual come in contact with the object."
  (format #t "checking fitness ~a~%" fitness)
  (every (lambda (i)
           (let ((distance (array-ref fitness i)))
             (<= distance successful-distance))) 
         (iota (array-length fitness))))

(define (any-individual-succeeded? generation results)
  "Returns true if we haven't found a successful candidate.  Input is (rank genome objective)."
  (format #t "Continue? ~a~%" generation)
  (any (compose individual-succeeded? caddr) results))

(define (get-results-that-succeeded results)
  "Return the individuals who succeeded at the task."
  (filter (lambda (result) 
            (individual-succeeded? (cdr result))) results))

(define (scaffold-any-individual-succeeded? generation results)
  (if (any-individual-succeeded? generation results)
      (if (< (length initial-conditions) 4)
          (begin
            (add-IC)
            (format #t "Succeeded. Adding next task. ~a~%" initial-conditions)
            #f)
          #t)
      #f))

(define (make-scaffold-any-individual-succeeded? tasks-count add-IC!)
  (let ((added-count 0))
   (lambda (generation results)
     (if (any-individual-succeeded? generation results)
         (if (< added-count tasks-count)
             (begin
               (add-IC! generation)
               (incr! added-count)
               (format #t "Succeeded. Adding task ~a/~a.~%" added-count tasks-count)
               #f ;; Continue searching.
               )
             #t ;; Stop searching.
             )
         #f ;; Continue searching.
         ))))

(define (make-scaffold-any-individual-not-succeeded? tasks-count add-IC!)
  (let ((added-count 0))
   (lambda (generation results)
     (if (any-individual-succeeded? generation results)
         (if (< added-count tasks-count)
             (begin
               (add-IC! generation)
               (incr! added-count)
               (format #t "Succeeded. Adding task ~a/~a.~%" added-count tasks-count)
               'fitness-changed ;; Continue searching.
               )
             #f ;; Stop searching.
             )
         #t ;; Continue searching.
         ))))

(define-interactive (beer-optimize)
  (optimize beer-selective-attention-n 30 '() (compose not scaffold-any-individual-succeeded?)))

;; The option handling is a mess. FIX IT!
(define-interactive
  (optimize 
   #:optional 
   (fitness-fn
    (let*-values
        (((to-string from-string) (object-tracker
                                   (compose symbol->string procedure-name))))
      (from-string (completing-read
                    "Fitness function: " 
                    (map to-string fitness-functions)
                    ;:history* 'fitness-function
                    #:initial-input 
                    (and last-fitness-func (to-string last-fitness-func))))))
   (max-generations 
    (read-from-string (read-from-minibuffer "Generation count: "
                                            ;:history* 'generation-count
                                            )))
   (seed-population (list current-genome))
   (continue-search? (compose not any-individual-succeeded?)))
  
  "Optimizes the given fitness function for a certain number of
generations with a given seed population.  The results are a list
of ((genome . fitness) ...) sorted in ascending order of the first
objective. Genome and fitness are #f64 arrays."

  (message "nsga-ii optimizing ~a" (fitness-desc fitness-fn))
  (if (called-interactively?) 
      ;; Let's the message be displayed before going into the big
      ;; optimization procedure.
      ;;
      ;; (message) should probably do that automatically then, right?
      (block-yield))

  (let* (#;
         (fitness-fn* 
          (lambda (weights)
            (with-fluids ((eval-robot-fluid eval-robot-headless))
              (fitness-fn weights))))
         (eval-count 0)
         (fitness-fn* 
          (lambda (weights)
            (incr! eval-count)
            (fitness-fn weights)))
         (generation-count 0)
         (objective-count (length (objectives fitness-fn)))
         ;; Had to use with-dynamic-state to make fluids work when crossing
         ;; into C code that called Scheme code.
         (results (nsga-ii-search 
                   fitness-fn*
                   #:objective-count objective-count
                   #:gene-count gene-count
                   #:population-count population-count
                   #:generation-count max-generations
                   #:seed-population seed-population
                   #:generation-tick-func (lambda args
                                            (incr! generation-count)
                                            (if continue-search?
                                             (apply continue-search? args)
                                             #t)))))
    ;; Get rid of any duplicate individuals.
    #;(set! results (uniq results))
    (set! last-fitness-func fitness-fn)
    (set! results (sort! results (lambda (a b)
                                   ;; XXX The < or > needs to be used
                                   ;; in reference to whether this is
                                   ;; being minimized or maximized.
                                   (< (: (cdr a) @ 0) (: (cdr b) @ 0)))))
    (set! last-results results)

    (set! current-genome (caar results))
    (genome->ctrnn current-genome ctrnn)
    (reset-fode)
    (message "Feasible fitnesses ~a" (map cdr results))
    (values results generation-count eval-count)
    #;
    (when (called-interactively?) 
      (call-interactively 'set-pareto-front-index 0)
      (call-interactively 'plot-front))
    #;(set! (controller (current-robot)) run-nn-brain)
    ))

(define (run-if-thunkable x)
  (if (thunk? x)
      (x)
      x))

(define* (generation-count-to-do my-initial-conditions 
                                 #:optional 
                                 (max-generations #f) 
                                 (seed-population '()) 
                                 (continue-search? #f))
  "Determine the number of generations required to succeed at the
given tasks."
  (define-fitness
    ((minimize "distance from origin"))
    (fitness-fn genome)
    (beer-selective-attention-n genome my-initial-conditions))
  (let ()
    (receive (results gen-count eval-count) (optimize 
                                             fitness-fn
                                             max-generations
                                             seed-population
                                             continue-search?)
      (list gen-count eval-count))))

(define* (generation-count-to-do2 my-initial-conditions 
                                 #:optional 
                                 (max-generations #f) 
                                 (seed-population '()) 
                                 (continue-search? #f))
  "Determine the number of generations required to succeed at the
given tasks."
  (define-fitness
    ((minimize "distance from origin"))
    (fitness-fn genome)
    (beer-selective-attention-n genome my-initial-conditions))
  (let ()
    ;; results are ((genome . objective-value) ... )
    (receive (results gen-count eval-count) (optimize 
                                             fitness-fn
                                             max-generations
                                             seed-population
                                             continue-search?)
      (list results gen-count eval-count))))

(define* (generation-count-to-do3 my-initial-conditions 
                                 #:optional 
                                 (max-generations #f) 
                                 (seed-population '()) 
                                 (continue-search? #f))
  "Determine the number of generations required to succeed at the
given tasks."
  (define-fitness
    ((minimize "left distance")
     (minimize "right distance"))
    (fitness-fn genome)
    (left-right-task genome my-initial-conditions))
  (let ()
    ;; results are ((genome . objective-value) ... )
    (receive (results gen-count eval-count) (optimize 
                                             fitness-fn
                                             max-generations
                                             seed-population
                                             continue-search?)
      (list results gen-count eval-count))))


(export reset-fode choose-initial-conditions generation-count-to-do2 generation-count-to-do3 any-individual-succeeded? left-right-task get-results-that-succeeded current-genome initial-conditions make-effector-function reset-camera)
