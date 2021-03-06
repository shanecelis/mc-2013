;; experiment.scm
;; I need to simplify this experiment stuff.

(define-module (experiment)
  #:use-module (oop goops)
  #:use-module (oop goops save)
  #:export (<experiment>
            exp:parameters
            exp:data
            exp:results
            exp:save-modules
            generate-parameters!
            run-experiment!
            analyze-data!
            export-data
            process-arguments!
            copy-parameters!
            clear-experiment!
            save-experiment
            read-experiment
            export-data
            <parent-experiment>
            exp:child-experiments
            read-experiment-cleanup
            ))

(define-class <experiment> ()
  ;; Get rid of these generic places to put data.  It only encourages
  ;; bad behavior.  Just create whatever members you need in the
  ;; subclasses.
  (parameters #:accessor exp:parameters #:init-value #f)
  (data #:accessor exp:data #:init-value #f)
  (results #:accessor exp:results #:init-value #f)
  
  (save-modules #:accessor exp:save-modules #:init-form '((oop goops)
                                                          (oop goops save)
                                                          (experiment))))
(define-generic process-arguments!)

(define-generic generate-parameters!)
(define-generic run-experiment!)
(define-generic analyze-data!)
(define-generic save-experiment)
(define-generic export-data)

(define-method (process-arguments! (exp <experiment>) args)
  (format (current-error-port) "warning: no argument processing available for ~a~%" exp)
  #f)

(define-method (generate-parameters! (exp <experiment>))
  #f)

(define-method (run-experiment! (exp <experiment>))
  #f)

(define-method (analyze-data! (exp <experiment>))
  #f)

(define-method (save-experiment (exp <experiment>) filename)
 (call-with-output-file filename
   (lambda (port)
     (save-objects (acons 'experiment exp '()) 
                   port 
                   '() 
                   (exp:save-modules exp)))))

(define-method (read-experiment-cleanup (exp <experiment>))
  #f)

(define-method (read-experiment filename)
  (let* ((objects (load-objects filename))
         (exp (assq-ref objects 'experiment)))
    ;(read-experiment-cleanup exp)
    exp))

(define-method (copy-parameters! (dest <experiment>) (src <experiment>))
  (set! (exp:parameters dest) (exp:parameters src)))


(define-method (clear-experiment! (exp <experiment>))
  (set! (exp:parameters exp) #f)
  (set! (exp:data exp) #f)
  (set! (exp:results exp) #f))

;; Let's make it so we can have a hierarchy of experiments.
(define-class <parent-experiment> (<experiment>)
  (child-experiments #:accessor exp:child-experiments #:init-keyword #:child-experiments #:init-form '())
  (aggregate-proc #:accessor exp:aggregate-proc #:init-keyword #:aggregate-proc #:init-value #f)
  )

(define-method (trial-count (exp <parent-experiment>))
  (length (exp:child-experiments exp)))

(define-method (generate-parameters! (exp <parent-experiment>))
  (for-each generate-parameters! (exp:child-experiments exp))
  (next-method))

(define-method (run-experiment! (exp <parent-experiment>))
  (for-each run-experiment! (exp:child-experiments exp))
  (next-method))

(define-method (analyze-data! (exp <parent-experiment>))
  (for-each analyze-data! (exp:child-experiments exp))
  (next-method)
  (if (exp:aggregate-proc exp)
      ((exp:aggregate-proc exp) (exp:child-experiments exp))))

(define-method (clear-experiment! (exp <parent-experiment>))
  (next-method)
  (for-each clear-experiment! (exp:child-experiments exp)))

(define-method (copy-parameters! (dest <parent-experiment>) (src <parent-experiment>))
  (if (= (trial-count dest) (trial-count src))
      (for-each copy-parameters! (exp:child-experiments dest) (exp:child-experiments src))
      (scm-error 'invalid-trial-counts 'copy-parameters! "Cannot copy parameters from one <parent-experiment> because they are different sizes: dest has ~a trials and src has ~a trials." (list (trial-count dest) (trial-count src)) #f)))
