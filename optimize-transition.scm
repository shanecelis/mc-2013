#!/usr/bin/env guile \
-e (@@\ (optimize-transition)\ main) -s
!#

;; Rename optimize-transition.scm to transition-params, perhaps?
(define-module (optimize-transition)
  #:use-module (srfi srfi-9) ;; record
  #:use-module (srfi srfi-9 gnu) ;; record
  #:use-module (srfi srfi-1) ;; iota
  #:use-module (vector-math)
  #:use-module (emacsy emacsy)
  #:use-module (system foreign)
  
  #:use-module (unified-procedure)
  #:use-module (guile-user) ;; because beer-experiment.scm
  #:use-module (eval-robot)
  #:export (<transition-params> 
            make-transition-params 
            make-transition-params-effector
            make-make-transition-effector-func
            tp:trinary-matrix?
            tp:motor-count-in
            tp:motor-count-out
            tp:gene-count))

(define (main args)
  (format #t "hi~%")

  )

;; This module is to optimize the transition from a minimal cognition
;; agent in FODE to an agent in Bullet.  The trick we're going to try
;; and do is this: Evolve in FODE, then keep that fixed, and evolve a
;; trinary or real matrix between the original agent and the new agent
;; optimized for fitness in Bullet.

(define-record-type <transition-params>
  (make-transition-params* trinary-matrix? motor-count-in motor-count-out gene-count)
  transition-params?
  (trinary-matrix? tp:trinary-matrix?)
  (motor-count-in tp:motor-count-in)
  (motor-count-out tp:motor-count-out)
  (gene-count tp:gene-count)
  )

(set-record-type-printer! <transition-params>
  (lambda (record port)
    (format port "(make-transition-params ~a ~a ~a)" 
            (tp:motor-count-in record)
            (tp:motor-count-out record)
            (tp:trinary-matrix? record))))

(define* (make-transition-params motor-count-in motor-count-out #:optional 
                                 (trinary-matrix? #t))
  "Make a genome for the given number of initial motors and final motors"
  (let ((gene-count (* motor-count-in motor-count-out)))
   (make-transition-params* trinary-matrix?
                            motor-count-in
                            motor-count-out
                            gene-count)))

(define (make-transition-params-effector transition-params effector genome)
  (make-unified-procedure 
   double 
   (lambda (t i effector-context . rest)
     ;; Oh, I can't just do it as matrix multiplication because I get
     ;; the values one at a time, and I send them out one at a time.
     ;; Therefore, I would either need to change the interface or I
     ;; need to collect them before I apply them.  I'll just compute
     ;; the dot product that I need for now without mucking with the
     ;; interface.
     
     #;(let ((original (apply effector t i rest))
     (M (genome->matrix genome)))
     (matrix. M original))
     (let* ((in-count (tp:motor-count-in transition-params))
            (out-count (tp:motor-count-out transition-params))
            (inputs (map 
                     ;; Effector is called multiple times.
                     (lambda (i) (effector t i effector-context)) 
                     (iota in-count 1)))
            ;; We'll treat genome like a matrix in row-major order.
            ;; We want to compute for matrix M = [ r1; r2; r3 ] and
            ;; input x the value r_i . x
            (r_i (map (lambda (j) (array-ref genome j))
                      (iota in-count (* out-count (1- i)))))
            (out (apply + (map * r_i inputs))))
       #;(format #t "CALLED input ~a out ~a~%" inputs out)
       out))
   (list double int '*)))

(define make-effector-func-original #f)

(define (make-make-transition-effector-func transition-params genome)
  (if (not make-effector-func-original)
       (set! make-effector-func-original make-effector-func))
  (lambda (ctrnn-state)
    (make-transition-params-effector
     transition-params
     (make-effector-func-original ctrnn-state)
     genome)))

(define-interactive (swap-out-effector)
  (set! make-effector-func 
        (make-make-transition-effector-func
         (make-transition-params 2 2 #f)
         #f64(1. 0. ;; genome
              0. 1.)))
  (reset-fode))
