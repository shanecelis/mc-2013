;; phenotype.scm
;;
;; It's surprising it took this long for this class to make an
;; appearance.

(define-module (phenotype)
  #:use-module (oop goops)
  #:use-module (ice-9 optargs)
  #:export (<phenotype>
            <composite-phenotype>
            <fixed-phenotype>
            gene-count-required
            init-from-genome!
            random-gene
            make-random-genome
            make-fixed-phenotype-class
            fp:fixed?
            ))

(define-class <phenotype> ())

;; gene-count-required :: integer
(define-generic gene-count-required)

;; initializes <phenotype> from a genome.
;; init-from-genome! :: <phenotype>, genome -> ()
(define-generic init-from-genome!)

;; Any phenotypes can be mashed together.
(define-class <composite-phenotype> (<phenotype>)
  (phenotypes #:getter phenotypes #:init-keyword #:phenotypes))

(define-class <fixed-phenotype> (<phenotype>)
  (fixed-genome #:init-keyword #:fixed-genome)
  (initialized? #:init-value #f)
  (fixed? #:accessor fp:fixed? #:init-value #t)
  #;(phenotype #:getter fp:phenotype #:init-keyword #:phenotype))

(define-method (initialize (fp <fixed-phenotype>) initargs)
  ;(format #t "init args ~a~%" initargs)
  (next-method)
  ;; XXX For some reason these slots are not being initialized.  WTF?
  (slot-set! fp 'initialized? #f)
  (let-keywords initargs #f ((fixed-genome #f))
                (slot-set! fp 'fixed-genome fixed-genome))
  (slot-set! fp 'fixed? #t))

(define-method (gene-count-required (fp <fixed-phenotype>))
  (if (fp:fixed? fp)
      0
      (next-method)))

(define-method (init-from-genome! (fp <fixed-phenotype>) genome)
  (if (not (slot-ref fp 'initialized?))
      (begin
        (slot-set! fp 'initialized? #t)
        (set! (fp:fixed? fp) #f)
        (let ((result (init-from-genome! fp (slot-ref fp 'fixed-genome))))
          (set! (fp:fixed? fp) #t)
          result))
      (if (fp:fixed? fp)
          *unspecified*
          (next-method))))

;; Ha ha, Extended Phenotype.
(define (make-fixed-phenotype-class extended-phenotype-class)
  "Given class that extends <phenotype> return a new class whose
phenotype is fixed."
  (make-class (list <fixed-phenotype> extended-phenotype-class) '() #:name 'fixed-thing))

;; How to dispatch on the other methods?  Don't bother.  We'll just
;; generate new classes at runtime.  Very cool.

(define-method (gene-count-required (cp <composite-phenotype>))
  (apply + (map gene-count-required (phenotypes cp))))

(define* (make-offset-array array offset #:optional (len #f))
  "Given an array make a subset as a shared-array (no copying)."
  (let* ((dim (array-dimensions array))
         (len* (or len (- (car dim) offset))))
    (unless (= 1 (length dim))
      (scm-error 'invalid-array
                 "make-offset-array"
                 "Expected a one dimensional array, got an ~a array instead."
                 (list dim) array))
    (unless (>= (- (car dim) offset len*) 0)
      (scm-error 'count-array
                 "make-offset-array"
                 "Trying to do an offset ~a of length ~a on an array of length ~."
                 (list offset len* (car dim)) array))
    (make-shared-array array 
                       (lambda (i) (list (+ i offset)))
                       len*)))

(define-method (init-from-genome! (cp <composite-phenotype>) genome)
  (let ((offset 0))
    (for-each 
     (lambda (phenotype)
       (let ((count (gene-count-required phenotype)))
         (when (or #t (> count 0))
          (init-from-genome! phenotype 
                             (make-offset-array genome offset count)))
        (set! offset (+ offset count))))
     (phenotypes cp))))


(define (random-gene)
  (- 1. (random 2.)))

(define (make-random-genome n)
  (let ((g (make-typed-array 'f64 0. n)))
    (array-index-map! g (lambda (i)
                          (random-gene)))
    g))
