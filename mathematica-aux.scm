(define-module (mathematica-aux)
  #:use-module (oop goops)
  #:use-module (mathematica)
  #:use-module (emacsy emacsy)
  #:use-module (logging)
  #:export (line-plot sexp->mathematica))

(define* (line-plot points 
                    #:key 
                    (axes-label #f)
                    (plot-label #f)
                    (joined #t)
                    (plot-legends #f)
                    (plot-range #f))
  
  (define (other-options)
    (let ((strings '()))
     (if axes-label
         (cons! (format #f "AxesLabel -> ~a" (sexp->mathematica axes-label)) strings))
     (if plot-label
         (cons! (format #f "PlotLabel -> ~a" (sexp->mathematica plot-label)) strings))
     (if plot-legends
         (cons! (format #f "PlotLegends -> ~a" (sexp->mathematica plot-legends)) strings))
     
     (if plot-range
         (cons! (format #f "PlotRange -> ~a" (sexp->mathematica plot-range)) strings))
     (cons! (format #f "Joined -> ~a" (sexp->mathematica joined)) strings)
     (string-join strings ", ")))
  (let* ((filename (format #f "~a.pdf" (tmpnam)))
         (expr (format #f "Export[\"~a\", ListPlot[~a, ~a]];" 
                        filename
                        (sexp->mathematica points)
                        (other-options))))
    (mylog "mathematica.input" pri-debug "~a" expr)
   (mathematica-eval expr)
   (system* "open" "-a" "Preview" filename)))

(define-method (sexp->mathematica (sexp <vector>))
  (sexp->mathematica (vector->list sexp)))

(define-method (sexp->mathematica (sexp <string>))
  (format #f "\"~a\"" sexp))

(define-method (sexp->mathematica (sexp <integer>))
  (format #f "~d" sexp))

(define-method (sexp->mathematica (sexp <real>))
  (format #f "~f" sexp))

(define-method (sexp->mathematica (sexp <symbol>))
  (format #f "~a" (symbol->string sexp)))

(define-method (sexp->mathematica (sexp <list>))
  (format #f "{~{~a~^,~}}" (map sexp->mathematica sexp)))

(define-method (sexp->mathematica (sexp <boolean>))
  (if sexp
      "True"
      "False"))
