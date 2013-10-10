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
  #:use-module (eval-robot)
  #:use-module (fitness)
  #:use-module (nsga-ii)
  #:use-module (mathematica-plot plot)
  #:use-module (brain)
  #:use-module (float-equality)
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
            exp:transition-params
            run-individual
            install-individual
            )
  #:re-export (exp:physics-class)
  )

(define-class <experiment-transition-trial> (<physics-experiment>)
  (ICs #:accessor exp:ICs)
  (max-gen #:accessor exp:max-gen #:init-keyword #:max-gen)
  (gen-count #:accessor exp:gen-count #:init-value 0)
  (eval-count #:accessor exp:eval-count #:init-value 0)
  (wall-clock-time #:accessor exp:wall-clock-time)
  (succeeded? #:accessor exp:succeeded? #:init-value #f))

(define-class <experiment-fode->bullet-trial> (<experiment-transition-trial>)
  (mc-genome #:accessor exp:mc-genome #:init-keyword #:mc-genome) ;; minimal cognition genome
  (transition-params #:accessor exp:transition-params #:init-keyword #:transition-params)
  )

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

(define-method (run-experiment! (exp <experiment-transition-trial>))
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
            recorded-fitness
            recalc-fitness)
    (unless (=? recorded-fitness recalc-fitness)
      (format (current-error-port) "Recorded and recalculated are not the same!~%")
      #;(throw 'blah))
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

(define-method (run-experiment! (exp <experiment-fode->bullet-trial>))
  (define (my-any-individual-succeeded? generation results)
    (let ((result (any-individual-succeeded? generation results)))
      (if result
          (set! (exp:succeeded? exp) #t))
      result))
  (if (exp:physics-class exp)
   (set! physics-class (exp:physics-class exp)))
  (format #t "physics class ~a~%" physics-class)
  (let ((eval-count 0)
        (generation-count 0)
        (myresults #f)
        (start-time (emacsy-time))
        (ctrnn (make <ctrnn-brain>)))
   (define-fitness
     ((minimize "left distance")
      (minimize "right distance"))
     (fitness-fn genome)
     (incr! eval-count)
     (left-right-task genome (map make-apply-IC (exp:ICs exp))))
   (init-brain-from-genome! ctrnn (exp:mc-genome exp))
   (set! brain-class (list <matrix-sandwich>
                           #:old-brain ctrnn
                           #:transition-params 
                           (eval (exp:transition-params exp) 
                                 (interaction-environment))))
   (set! myresults (nsga-ii-search fitness-fn
                    #:gene-count (tp:gene-count (exp:transition-params exp))
                    #:objective-count 2
                    #:population-count 12
                    #:generation-count (exp:max-gen exp)
                    #:seed-population '()
                    #:generation-tick-func (lambda args
                                             (incr! generation-count)
                                             (apply (compose not my-any-individual-succeeded?) args))))
   (set! (exp:results exp) (get-results-that-succeeded myresults))
   (set! (exp:gen-count exp) generation-count)
   (set! (exp:eval-count exp) eval-count)
   (set! (exp:wall-clock-time exp) (- (emacsy-time) start-time)))
  (if (exp:succeeded? exp) 
      (run-individual exp 0))
  )

(define-method (analyze-data! (exp <experiment-transition-trial>))
  #f)

(define-class <experiment-transition-parent> (<parent-experiment>))

(define-method (analyze-data! (exp <experiment-transition-parent>))
  (let* ((exps (exp:child-experiments exp))
         (gen-counts (map exp:gen-count exps))
         (eval-counts (map exp:eval-count exps))
         (wall-clock-times (map exp:wall-clock-time exps))
         (succeeded (map (lambda (exp) (if (exp:succeeded? exp) 1. 0.)) exps)))
    
    (define (show-stats lst name)
      (format #t "~a mean ~a std ~a~%" name (mean lst) (std lst)))
    (show-stats gen-counts "gen-counts")
    (show-stats eval-counts "eval-counts")
    (show-stats wall-clock-times "wall-clock-times")
    (show-stats succeeded "success")))

(define-method (export-data (exp <experiment-transition-parent>) port)
  (let* ((exps (exp:child-experiments exp))
         (gen-counts (map exp:gen-count exps))
         (eval-counts (map exp:eval-count exps))
         (wall-clock-times (map exp:wall-clock-time exps))
         (succeeded (map (lambda (exp) (if (exp:succeeded? exp) 1. 0.)) exps)))
    
    (define (show-stats lst name)
      (format port "~a -> ~a~%" name (sexp->mathematica lst)))
    (format port "{~%")
    (show-stats gen-counts "genCounts")
    (show-stats eval-counts ", evalCounts")
    (show-stats wall-clock-times ", wallClockTimes")
    (show-stats succeeded ", success")
    (format port "}~%")
    ))

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

