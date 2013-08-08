(define-module (experiment-transition)
  #:use-module (beer-parameters)
  #:use-module (oop goops)
  #:use-module (experiment)
  #:use-module (emacsy emacsy)
  #:use-module (optimize-transition)
  #:use-module (beer-initial-conditions)
  #:use-module (physics)
  #:use-module (stats)
  #:use-module (bullet-physics-car)
  #:use-module (fode-physics)
  #:use-module (guile-user)
  #:use-module (ice-9 match)
  
  #:export (left-IC right-IC <experiment-transition-trial> 
                    ICs 
                    exp:mc-genome exp:max-gen exp:gen-count exp:eval-count exp:wall-clock-time
                    <experiment-transition-parent> 
                    )
 
  )

(define-class <experiment-transition-trial> (<experiment>)
  (ICs #:accessor exp:ICs)
  (max-gen #:accessor exp:max-gen #:init-keyword #:max-gen)
  (gen-count #:accessor exp:gen-count)
  (eval-count #:accessor exp:eval-count)
  (wall-clock-time #:accessor exp:wall-clock-time)
  (physics-class #:accessor exp:physics-class #:init-keyword #:physics-class #:init-value #f)
  (succeeded? #:accessor exp:succeeded?))

(define-class <experiment-fode->bullet-trail> (<experiment-transition-trial>)
  (mc-genome #:getter exp:mc-genome #:init-keyword #:mc-genome) ;; minimal cognition genome
  (transition-params #:accessor exp:transition-params #:init-keyword #:transition-params)
  )

(define-method (initialize (exp <experiment-transition-trial>) initargs)
  (next-method)
  (set! (exp:save-modules exp) 
        (append (exp:save-modules exp) 
                 '((experiment-transition)
                   (physics)
                   (fode-physics)
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
  (define (my-any-individual-succeeded? . args)
    (let ((result (apply any-individual-succeeded? args)))
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
      (set! (exp:results exp) myresults)
      (set! (exp:gen-count exp) gen-count)
      (set! (exp:eval-count exp) eval-count)
      (set! (exp:wall-clock-time exp) (- (emacsy-time) start-time))))))

(define-method (run-experiment! (exp <experiment-fode->bullet-trail>))
  (nsga-ii-search
   #:gene-count (tp:gene-count (exp:transition-params exp))
   #:objective-count 2
   #:population-count 12
   #:generation-count (exp:max-gen exp)
   #:seed-population '()
   )
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

(define-method (initialize (exp <experiment-transition-parent>) initargs)
  (next-method)
  (set! (exp:save-modules exp) 
        (append (exp:save-modules exp) 
                 '((experiment-transition)
                   (physics)
                   (fode-physics)
                   (bullet-physics-car)
                   (bullet-physics)
                   ))))
