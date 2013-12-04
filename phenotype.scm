;; phenotype.scm
;;
;; It's surprising it took this long for this class to make an
;; appearance.

(define-module (phenotype)
  #:use-module (oop goops)
  #:export (<phenotype>
            <composite-phenotype>
            gene-count-required
            init-from-genome!))

(define-class <phenotype> ())

;; gene-count-required :: integer
(define-generic gene-count-required)

;; initializes <phenotype> from a genome.
;; init-from-genome! :: <phenotype>, genome -> ()
(define-generic init-from-genome!)

;; Any phenotypes can be mashed together.
(define-class <composite-phenotype> (<phenotype>)
  (phenotypes #:getter phenotypes #:init-keyword #:phenotypes))

(define-method (gene-count-required (cp <composite-phenotype>))
  (apply + (map gene-count-required (phenotypes cp))))

(define* (make-offset-array array offset #:optional (len #f))
  "Given an array make a subset as a shared-array (no copying)."
  (let ((dim (array-dimensions array)))
    (unless (= 1 (length dim))
      (scm-error 'invalid-array
                 "make-offset-array"
                 "Expected a one dimensional array, got an ~a array instead."
                 (list dim) array))
    (make-shared-array array 
                       (lambda (i) (list (+ i offset)))
                       (or len
                           (- (car dim) offset)))))

(define-method (init-from-genome! (cp <composite-phenotype>) genome)
  (let ((offset 0))
    (for-each 
     (lambda (phenotype)
       (let ((count (gene-count-required phenotype)))
        (init-from-genome! phenotype 
                           (make-offset-array genome offset count))
        (set! offset (+ offset count))))
     (phenotypes cp))))
