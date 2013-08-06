#!/usr/bin/env eracs -b -s ./run-experiment.scm
!#
(use-modules (experiment)
             (experiment-gen-count-vs-select-attn)
             (emacsy util)
             (oop goops save))
(format #t "prog args ~a~%" (program-arguments))
(define argc (length (program-arguments)))
(unless (or (= 2 argc)
            (= 3 argc))
  (format #t "Usage: run-experiment <filename> [physics-class]~%")
  (format #t "physics-classes: <fode-physics>, <bullet-physics>, <bullet-physics-car>")
  (exit 2))
(define filename (cadr (program-arguments)))
(define physics-class-arg #f)
(if (= 3 argc)
    (set! physics-class-arg (eval (read-from-string (caddr (program-arguments))) (interaction-environment)))
    (set! physics-class-arg <fode-physics>))
;(define exp-class <gen-count-vs-select-attn-trial>)
(define exp-class <gen-count-vs-select-attn-trial-easier>)
;(define exp-class <gen-count-vs-select-attn-dummy>)
(define trial-count 1) ;;10
(define (make-trial) 
  (make-instance exp-class 
                 #:task-count 5 
                 #:max-gen 10 ;; 100
                 #:max-speed 0.25
                 #:physics-class 
                   physics-class-arg
                 ))

(define exp (make <gen-count-vs-select-attn>
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
(exit 0)


