(define-module (experiment-transition)
  #:use-module (beer-parameters)
  #:use-module (oop goops)
  #:use-module (experiment)
  #:use-module (emacsy emacsy)
  #:use-module (optimize-transition)
  #:use-module (beer-initial-conditions)
  #:use-module (experiment-gen-count-vs-select-attn)
  #:use-module (physics)
  #:use-module (stats)
  #:use-module (bullet-physics-car)
  #:use-module (fode-physics)
  #:use-module (guile-user)
  #:use-module (ice-9 match)
  #:use-module (ice-9 getopt-long)
  #:use-module (eval-robot)
  #:use-module (fitness)
  #:use-module (nsga-ii)
  #:use-module (mathematica-plot plot)
  #:use-module (phenotype)
  #:use-module (brain)
  #:use-module (float-equality)
  #:use-module ((vector-math) #:select (vector->string range))
  #:use-module (minimal-cognition ctrnn)
  #:export (
            left-IC 
            right-IC 
            <experiment-transition-trial> 
            exp:ICs 
            exp:mc-genome
            exp:max-gen
            exp:gen-count
            exp:eval-count
            exp:wall-clock-time
            exp:succeeded?

            <experiment-transition-parent> 
            <experiment-fode->bullet-trial>
            <experiment-fode->bullet-trial-no-sandwich>
            exp:transition-params
            run-individual
            install-individual
            to-experiment-fode->bullet-trial
            exp:fitness-time-series
            exp:stop-when-succeeded?
            )
  #:re-export (exp:physics-class)
  )

(define-class <experiment-transition-trial> (<physics-experiment>)
  (ICs #:accessor exp:ICs)
  (max-gen #:accessor exp:max-gen #:init-keyword #:max-gen)
  (gen-count #:accessor exp:gen-count #:init-value 0)
  (eval-count #:accessor exp:eval-count #:init-value 0)
  (wall-clock-time #:accessor exp:wall-clock-time)
  (succeeded? #:accessor exp:succeeded? #:init-value #f)
  (fitness-time-series #:accessor exp:fitness-time-series #:init-value '())
  (stop-when-succeeded? #:accessor exp:stop-when-succeeded? #:init-value #t))

(define-class <experiment-fode->bullet-trial> (<experiment-transition-trial>)
  (mc-genome #:accessor exp:mc-genome #:init-keyword #:mc-genome) ;; minimal cognition genome
  (transition-params #:accessor exp:transition-params #:init-keyword #:transition-params))

;; We do an experiment without sandwiches, we just seed the population with evolved CTRNNs.
(define-class <experiment-fode->bullet-trial-no-sandwich> (<experiment-fode->bullet-trial>))

(define-method (to-experiment-fode->bullet-trial
                (expmt <experiment-transition-trial>))
  (change-class expmt <experiment-fode->bullet-trial>)
  (set! (exp:mc-genome expmt) (caar (exp:results expmt)))
  (set! (exp:results expmt) '())
  (set! (exp:transition-params expmt) (make-transition-params 2 2 #f))
  expmt)

(define-method (to-experiment-fode->bullet-trial
                (expmt <experiment-fode->bullet-trial>))
  expmt)

(define-method (initialize (exp <experiment-transition-trial>) initargs)
  (next-method)
  (set! (exp:save-modules exp) 
        (append (exp:save-modules exp) 
                 '((experiment-transition)
                   (physics)
                   (fode-physics)
                   (bullet-physics-skateboard)
                   (bullet-physics-car)
                   (bullet-physics)))))

(define object-x-distance-from-origin 50.)

(define (left-IC)
  `((position . (#(0 0) #(,(- object-x-distance-from-origin) ,max-height)))
    (velocity . (#(0 0) #(0. -8.)))))

(define (right-IC)
  `((position . (#(0 0) #(,(+ object-x-distance-from-origin) ,max-height)))
    (velocity . (#(0 0) #(0. -8.)))))

(define-interactive (switch-to-left-IC)
  (set! choose-initial-conditions (make-apply-IC (left-IC)))
  (reset-fode))

(define-interactive (switch-to-right-IC)
  (set! choose-initial-conditions (make-apply-IC (right-IC)))
  (reset-fode))

(define-method (generate-parameters! (exp <experiment-transition-trial>))
  (set! (exp:ICs exp) (list (left-IC) (right-IC))))

#;(define-method (run-experiment! (exp <experiment-transition-trial>))
  (define (my-any-individual-succeeded? generation results)
    (let ((result (any-individual-succeeded? generation results)))
      (if result
          (set! (exp:succeeded? exp) #t))
      result))
  (set! (exp:succeeded? exp) #f)
  (if (exp:physics-class exp)
   (set! physics-class (exp:physics-class exp)))
  (let ((start-time (emacsy-time)))
    (match (generation-count-to-do3
            (map make-apply-IC (exp:ICs exp)) 
            (exp:max-gen exp)
            '()
            (compose not my-any-individual-succeeded?))
     ((myresults gen-count eval-count)
      (set! (exp:results exp) (get-results-that-succeeded myresults))
      (set! (exp:gen-count exp) gen-count)
      (set! (exp:eval-count exp) eval-count)
      (set! (exp:wall-clock-time exp) (- (emacsy-time) start-time)))))
  (if (exp:succeeded? exp) 
      (run-individual exp 0)))

(define-method (run-individual (exp <experiment-transition-trial>) index)
  (let ((recalc-fitness (left-right-task (car (list-ref (exp:results exp) index))
                                         (map make-apply-IC (exp:ICs exp))))
        (recorded-fitness (cdr (list-ref (exp:results exp) index))))
    (format #t "Recorded fitness ~a and recalculated fitness ~a.~%" 
            (vector->string recorded-fitness)
            (vector->string recalc-fitness))
    (unless (=? recorded-fitness recalc-fitness)
      (format (current-error-port) "Recorded and recalculated are not the same!~%"))
    recalc-fitness))

(define-method (run-individual (exp <experiment-fode->bullet-trial>) index)
  (let ((ctrnn (make <ctrnn-brain>))
        (transition-genome (car (list-ref (exp:results exp) index))))
    (init-brain-from-genome! ctrnn (exp:mc-genome exp))
    (set! brain-class (list <matrix-sandwich>
                            #:old-brain ctrnn
                            #:transition-params 
                            (eval (exp:transition-params exp) 
                                  (interaction-environment))
                            ;;#:matrix-sandwich transition-genome
                            ))
    (next-method)))

(define-method (install-individual (exp <experiment-transition-trial>) index)
  (set! current-genome (car (list-ref (exp:results exp) index)))
  (set! initial-conditions (map make-apply-IC (exp:ICs exp))))

(define-method (install-individual (exp <experiment-fode->bullet-trial>) index)
  ;(set! current-genome (exp:mc-genome exp))
  ;(format #t "HERE!!!!!~%")
  (let ((ctrnn (make <ctrnn-brain>))
        (transition-genome (car (list-ref (exp:results exp) index))))
    (init-brain-from-genome! ctrnn (exp:mc-genome exp))
    (set! current-genome transition-genome)
    (set! brain-class (list <matrix-sandwich>
                          #:old-brain ctrnn
                          #:transition-params 
                          (eval (exp:transition-params exp) (interaction-environment))
                          #:matrix-sandwich transition-genome)))
  (set! initial-conditions (map make-apply-IC (exp:ICs exp))))

(define-method (get-brain-class (exp <experiment-fode->bullet-trial>))
  (let ((ctrnn (make <ctrnn-brain>)))
       (init-brain-from-genome! ctrnn (exp:mc-genome exp))
       (list <matrix-sandwich>
             #:old-brain ctrnn
             #:transition-params 
             (eval (exp:transition-params exp) 
                   (interaction-environment)))))

(define-method (get-brain-class (exp <experiment-transition-trial>))
  <ctrnn-brain>)

(define-method (get-brain-class (exp <experiment-fode->bullet-trial-no-sandwich>))
  <ctrnn-brain>)

(define-method (get-gene-count (exp <experiment-fode->bullet-trial>))
  (tp:gene-count (exp:transition-params exp)))

(define-method (get-gene-count (exp <experiment-transition-trial>))
  (gene-count-for-n-ctrnn node-count))

(define-method (process-arguments! (exp <experiment-fode->bullet-trial>) args)
  (let* ((leftover-args (next-method))
         (fode-exp (begin
                     (when (null? leftover-args)
                       (format (current-error-port)
                               "error: require a success experiment file for genome.~%")
                       (exit 3))
                     (read-experiment (car leftover-args))))
         (fode-genome (begin
                        (when (null? (exp:results fode-exp))
                          (format (current-error-port)
                                  "error: no successful results available in ~a.~%" (car leftover-args))
                          (exit 1))
                        (caar (exp:results fode-exp)))))
    (set! (exp:mc-genome exp) fode-genome)
    (format #t "mc-genome ~a~%" (array? (exp:mc-genome exp)))
    (set! (exp:transition-params exp) (make-transition-params 2 2 #f))))

(define-method (process-arguments! (exp <experiment-transition-trial>) args)
  (let* ((option-spec
          '((generation-count (single-char #\g) (value #t))
            (stop-when-succeeded? (single-char #\S) (value #t))
            (physics-class (single-char #\p) (value #t))))
         (options (getopt-long args option-spec #:stop-at-first-non-option #t))
         (args (option-ref options '() '()))
         (argc (length args))
         (physics-class (eval (read-from-string 
                               (option-ref options 'physics-class "<fode-physics>"))
                              (interaction-environment)))
         (stop-when-succeeded?
          (read-from-string 
           (option-ref options 'stop-when-succeeded? "#t")))
         (generation-count 
          (string->number (option-ref options 'generation-count "100"))))
    (set! (exp:physics-class exp) physics-class)
    (set! (exp:stop-when-succeeded? exp) stop-when-succeeded?)
    (set! (exp:max-gen exp) generation-count)
    (format #t "physics-class ~a~%" (exp:physics-class exp))
    (format #t "stop-when-succeeded? ~a~%" (exp:stop-when-succeeded? exp))
    (format #t "max-generation ~a~%" (exp:max-gen exp))
    args))

(define-method (exp:seed-population (exp <experiment-transition-trial>))
  '())

(define-method (exp:seed-population (exp <experiment-fode->bullet-trial-no-sandwich>))
  (format #t "Seeding the population with CTRNNs.")
  (map (lambda (i)
         (exp:mc-genome exp)) (range 1 population-count)))

(define-method (run-experiment! (exp <experiment-transition-trial>))
  (define (my-any-individual-succeeded? generation results)
    (let ((result (any-individual-succeeded? generation results)))
      (if result
          (set! (exp:succeeded? exp) #t))
      result))
  (if (exp:physics-class exp)
   (set! physics-class (exp:physics-class exp)))
  (format #t "physics class ~a~%" physics-class)
  (set! brain-class (get-brain-class exp))

  (let ((eval-count 0)
        (generation-count 0)
        (myresults #f)
        (start-time (emacsy-time))
        (fitness-collector (make-fitness-collector))
        (phenotype (make <composite-phenotype> 
                     #:phenotypes (list (make-brain)
                                        (make physics-class
                                          #:object-count body-count)))))
   (define-fitness
     ((minimize "left distance")
      (minimize "right distance"))
     (fitness-fn genome)
     (incr! eval-count)
     (left-right-task genome (map make-apply-IC (exp:ICs exp))))
   
   (set! myresults (nsga-ii-search fitness-fn
                    #:gene-count (gene-count-required phenotype)
                    #:objective-count 2
;                    #:real-mutation-rate 0
;                    #:real-crossover-rate 0
                    #:population-count population-count
                    #:generation-count (exp:max-gen exp)
                    #:seed-population (exp:seed-population exp)
                    #:generation-tick-func 
                    (lambda args
                      (incr! generation-count)
                      (apply fitness-collector args)
                      (if (exp:stop-when-succeeded? exp) 
                          (apply (compose not my-any-individual-succeeded?) args)
                          #t))))
   (set! (exp:results exp) (get-results-that-succeeded myresults))
   (set! (exp:gen-count exp) generation-count)
   (set! (exp:eval-count exp) eval-count)
   (set! (exp:wall-clock-time exp) (- (emacsy-time) start-time))
   (set! (exp:fitness-time-series exp) (fitness-collector)))
  (when (exp:succeeded? exp) 
      (run-individual exp 0)))

(define-method (analyze-data! (exp <experiment-transition-trial>))
  #f)

(define-class <experiment-transition-parent> (<parent-experiment>))

(define-method (analyze-data! (exp <experiment-transition-parent>))
  (let* ((exps (exp:child-experiments exp))
         (gen-counts (map exp:gen-count exps))
         (eval-counts (map exp:eval-count exps))
         (wall-clock-times (map exp:wall-clock-time exps))
         (succeeded (map (lambda (exp) (if (exp:succeeded? exp) 1. 0.)) exps))
         (fitness-time-series (map exp:fitness-time-series exps)))
    
    (define (show-stats lst name)
      (format #t "~a~/ median ~1,2f~/ mean ~1,2f~/ ste ~1,2f~%" name (median lst) (mean lst) (ste lst)))
    (show-stats gen-counts       "gen-counts          ")
    (show-stats eval-counts      "eval-counts         ")
    (show-stats wall-clock-times "wall-clock-times (s)")
    (show-stats succeeded        "success             ")))

(define-method (export-data (exp <experiment-transition-parent>) port)
  (let* ((exps (exp:child-experiments exp))
         (gen-counts (map exp:gen-count exps))
         (eval-counts (map exp:eval-count exps))
         (wall-clock-times (map exp:wall-clock-time exps))
         (succeeded (map (lambda (exp) (if (exp:succeeded? exp) 1. 0.)) exps))
         ;(fitness-time-series (map exp:fitness-time-series exps))
         (fitness (map (compose cdr exp:results) exps)))
    
    (define (show-stats lst name)
      (format port "~a -> ~a~%" name (sexp->mathematica lst)))
    
    (format port "{~%")
    (show-stats gen-counts "genCounts")
    (show-stats eval-counts ", evalCounts")
    (show-stats wall-clock-times ", wallClockTimes")
    (show-stats succeeded ", success")
;    (show-stats fitness-time-series ", fitnessTimeSeries")
;    (show-stats succeeded ", fitness")
    (format port "}~%")))

(define-method (initialize (exp <experiment-transition-parent>) initargs)
  (next-method)
  (set! (exp:save-modules exp) 
        (append (exp:save-modules exp) 
                 '((experiment-transition)
                   (physics)
                   (fode-physics)
                   (bullet-physics-car)
                   (bullet-physics-skateboard)
                   (bullet-physics)))))

