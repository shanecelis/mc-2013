#!/usr/bin/env guile \
--debug -e main -s 
!#
(use-modules (experiment)
             (experiment-gen-count-vs-select-attn)
             (beer-initial-conditions)
             (beer-parameters)
             (emacsy util)
             (oop goops)
             (fode-physics)
             (bullet-physics)
             (oop goops save)
             (system repl error-handling))
;; UGH.

(load "beer-experiment.scm")

(format #t "prog args ~a~%" (program-arguments))

(define (main args)
  (call-with-error-handling
   (lambda ()
     (let ((argc (length (program-arguments)))
           (filename #f)
           (physics-class-arg #f))
       (unless (or (= 2 argc)
                   (= 3 argc))
         (format #t "Usage: run-experiment <filename> [physics-class]~%")
         (format #t "physics-classes: <fode-physics>, <bullet-physics>, <bullet-physics-car>~%")
         (exit 2))
       (set! filename (cadr (program-arguments)))
       (if (= 3 argc)
           (set! physics-class-arg (eval (read-from-string (caddr (program-arguments))) (interaction-environment)))
           (set! physics-class-arg <fode-physics>))
       (run-experiment filename physics-class-arg)))))

(define* (run-experiment filename #:optional physics-class-arg)
  ;;(define exp-class <gen-count-vs-select-attn-trial>)
  (define exp-class <gen-count-vs-select-attn-trial-easier>)
  (define exp #f)
  ;(define exp-class <gen-count-vs-select-attn-dummy>)
  (define trial-count 1) ;;10
  (define (make-trial) 
    (make-instance exp-class 
                   #:task-count 5 
                   #:max-gen 3 ;; 100
                   #:max-speed 0.25
                   #:physics-class 
                   physics-class-arg
                   ))

  (set! exp (make <gen-count-vs-select-attn>
                #:child-experiments
                (map (lambda (i) (make-trial)) (iota trial-count))))


  (generate-parameters! exp)
  (format #t "PARAMETERS: ~a~%" (exp:parameters exp))

  (run-experiment! exp)
  (format #t "DATA: ~a~%" (exp:data exp))

  (analyze-data! exp)
  (format #t "RESULTS: ~a~%" (exp:results exp))


  (call-with-output-file filename
    (lambda (port)
      (save-objects (acons 'experiment exp '()) 
                    port 
                    '() 
                    '((oop goops)
                      (oop goops save)
                      (experiment)
                      (experiment-gen-count-vs-select-attn)
                      (bullet-physics-car)
                      (bullet-physics)
                      (fode-physics)
                      ))))


)
