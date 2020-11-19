(in-package :funcl)
(annot:enable-annot-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lparallel:*kernel*
        (lparallel:make-kernel (or (hpc-tools:ncpus) 1))))

@export
(defclass gnuplot-options ()
  ((title :initarg :title :accessor title)
   (debug :initarg :debug :accessor debug-p :initform t)
   (granularity :initarg :granularity :accessor granularity :initform 200)
   (terminals :initarg :terminals :accessor terminals :initform :x11)
   (x-min :initarg :x-min :initform 0)
   (x-max :initarg :x-max :initform 1)
   (x-label :initarg :x-label)
   (y-label :initarg :y-label)
   (wait-for-x11 :initarg :wait-for-x11 :accessor wait-for-x11-p :initform t)))

(defun plot-output-now (&key (extension "tex"))
  (lambda ()
    (format nil "plot~d.~d"
            (substitute #\_ #\:
                        (substitute #\_ #\-
                                    (substitute #\_ #\+
                                                (substitute #\_ #\. 
                                                            (local-time:format-timestring nil (local-time:now)))))) extension)))

@export
(defun simple-gnuplot-options (&key (title "f(x)") 
                                 (terminals (list 
                                             (list '(:cairolatex :pdf :color :standalone :blacktext
                                                     :|size 16cm ,10.5cm|) 
                                                   (plot-output-now))
                                             (list '(:x11) (lambda () nil))))
                                 (wait-for-x11 t)
                                 (x-label "")
                                 (x-min 0)
                                 (x-max 1)
                                 (y-label ""))
  (make-instance 'gnuplot-options :title title :terminals terminals :wait-for-x11 wait-for-x11 :x-label x-label :y-label y-label :x-min x-min :x-max x-max))

@export
(defun gnuplot-options (&key (title "") (x11 nil) (x-label "") (x-min 0) (x-max 1) (y-label "") (terminals nil))
  (make-instance 'gnuplot-options :title title :wait-for-x11 t :x-label x-label :y-label y-label
                 :x-min x-min :x-max x-max
                                  :terminals (if terminals
                                                 terminals
                                                 (if x11 
                                                     (list 
                                                      (list '(:cairolatex :pdf :color :standalone :blacktext
                                                              :|size 16cm ,10.5cm|) 
                                                            (plot-output-now))
                                                      (list '(:x11) (lambda () nil)))
                                                     (list 
                                                      (list '(:cairolatex :pdf :color :standalone :blacktext
                                                              :|size 16cm ,10.5cm|) 
                                                            (plot-output-now)))))))

@export
(defgeneric plot-with-options (function options output-dir))

@export
(defun plot (function)
  (plot-with-options function (simple-gnuplot-options) nil))

(defun function-plot-start (function)
  (with-slots (domain) function
    (if (and (consp domain) (numberp (car domain)))
        (car domain)
        0)))

(defun function-plot-end (function)
  (with-slots (domain) function
    (if (and (consp domain) (numberp (cdr domain)))
        (cdr domain)
        1)))

(defmethod plot-with-options ((function-list list) (options gnuplot-options) output-dir)
  (with-slots (debug range granularity terminals wait-for-x11 x-label y-label x-min x-max) options
    (loop for (terminal output-lambda) in terminals do
         (let ((output (funcall output-lambda)))
           (eazy-gnuplot:with-plots (*standard-output* :debug debug)
             (eazy-gnuplot:gp-setup :terminal terminal :output output :style '(fill pattern 5))
           ;  (break)
             (when (slot-boundp options 'title)
               (format t "~% set title ~s" (title options))
               (format t "~% show title ~%"))
             (format t "~% set xlabel ~s~% show xlabel ~% set ylabel ~s~% show ylabel ~%" x-label y-label)
             (loop for function in function-list
                for index from 1 doing
                  (with-slots (lambda-function domain) function
                    (let ((start x-min)
                          (end x-max))
                      (eazy-gnuplot:plot
                       (lambda ()
                         (let* ((values (alexandria:iota granularity :step (/ (- end start) granularity) :start start))
                                (function-values (lparallel:pmapcar (alexandria:compose #'flatten-scalar lambda-function) values)))
                           (mapcar (lambda (i j) (format t "~&~,6f ~F" i j)) values function-values)))
                       :with '(:lines) :title ;(if (slot-boundp options 'title) (title options) "")
                                        (remove-if (alexandria:curry #'char= #\#)  (name function))
                       ))))
             (when (and (member :x11 terminal)
                        wait-for-x11) (format t "~&pause mouse button1;~%")))
           (when (member :pdf terminal) (cl-user::run-interactively (format nil "pdflatex ~s" output))))))
  (cl-user::run-interactively "mogrify -verbose -density 500 -resize 1600 -format png ./*.pdf")
  (loop for pathname in output-dir do
        (cl-user::run-interactively (format nil "cp plot* ~a" pathname)))
  (cl-user::run-interactively (format nil "mv plot* /rds/general/user/sgs16/home/plots"))
  t)

(defmethod plot-with-options ((function funcl-function) (options gnuplot-options) output-dir)
  (plot-with-options (list function) options output-dir))

(defun plot-slice (function &key (lower #(0 0))
                                 (higher #(1 1))
                                 (axis :z)
                                 slice-value
                                 (resolution 100)
                                 (contours t)
                                 (post-title "")
                                 (title (string-downcase (format nil "~a = ~a" axis slice-value)))
                                 (heatmap t)
                                 (options (simple-gnuplot-options)))
  (labels ((axis-extent (ax)
             (case ax
               (:x (list (aref lower 0) (aref higher 0)))
               (:y (list (aref lower (if (eq axis :z) 1 0))
                         (aref higher (if (eq axis :z) 1 0))))
               (:z (list (aref lower 1) (aref higher 1)))))
           (coordinate-slicer (ax)
             (if (eq axis ax)
                 (make-array nil :initial-element slice-value)
                 (apply #'aops:linspace (append (axis-extent ax) (list resolution)))))
           (dual-axes ()
             (case axis
               (:x '(1 2))
               (:y '(0 2))
               (:z '(0 1)))))
    (let* ((sample-points (aops:flatten
                           (aops:outer #'vector
                                       (coordinate-slicer :x)
                                       (coordinate-slicer :y)
                                       (coordinate-slicer :z))))
           (function-values (lparallel:pmap 'vector (alexandria:compose #'flatten-scalar (lambda-function function)) sample-points)))
      (with-slots (debug range granularity terminals wait-for-x11 x-label y-label x-min x-max) options
        (loop for (terminal output-lambda) in terminals do
              (let ((output (funcall output-lambda)))
                (eazy-gnuplot:with-plots (*standard-output* :debug debug)
                  (eazy-gnuplot:gp-setup :terminal terminal :output output; :style '(fill solid)
                                         )
                  (format t "~%set border 4095 front lt black linewidth 1.000 dashtype solid~%")
 (format t "~%set style increment default~%")
                                        ;(format t "~%set style data lines~%")
                  (format t "~%set style circle radius graph 0.02, first 0.00000, 0.00000~%")
                  (format t "~%set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy~%")
                  (format t "~%set view map scale 1~%")
                  (format t "~%set pm3d map impl")
                  (format t "~%set contour base~%")
                  (format t "~%set title \"~a~a\"~%" title post-title)
                  (format t "~%set lmargin at screen 0.1~%")
                  (format t "~%set rmargin at screen 0.7~%")
                  (format t "~%set cntrlabel  format '%8.3g' font ',4' start 2 interval 20~%")
                  (format t "~%set cntrparam levels auto 10~%")
                  (format t "~%set cntrparam bspline~%");
                  (format t "~%set size ratio ~,6f~%" (/ (- (aref higher (cadr (dual-axes))) (aref lower (cadr (dual-axes))))
                                                         (- (aref higher (car (dual-axes))) (aref lower (car (dual-axes))))))
                  (format t "~%set palette rgbformulae 22,13,10~%");
                  (format t "~%set samples 25, 25~%")
                  (format t "~%set isosamples 26, 26~%")
                  (format t "~%unset grid~%unset key~%")
                  (format t "~%set xlabel \"~a\"~%" (nth (car (dual-axes)) '("x" "y" "z")))
                  (format t "~%set ylabel \"~a\"~%" (nth (cadr (dual-axes)) '("x" "y" "z")))
                  (format t "~%set xtics (\"~s\" 0, \"~s\" ~s)~%" (aref lower (car (dual-axes))) (aref higher (car (dual-axes))) (- resolution 1) )
                  (format t "~%set ytics (\"~s\" 0, \"~s\" ~s)~%" (aref lower (cadr (dual-axes))) (aref higher (cadr (dual-axes))) (- resolution 1))
                 ; (format t "~%set grid xtics lc rgb \"#ffffff\" lw 1 lt 0~%")
                 ; (format t "~%set grid ytics lc rgb \"#ffffff\" lw 1 lt 0~%unset grid~%")
                  (eazy-gnuplot:splot (lambda ()
                                        (format t "~{ ~{~,6f~%~}~%~}" (apply #'mapcar #'list (group (coerce function-values 'list)
                                                                                            resolution)))
                                        )
                                      :using 1 )
                  
                  (when (and (member :x11 terminal)
                             wait-for-x11) (format t "~&pause mouse keypress;~%")))
                (when (member :pdf terminal) (cl-user::run-interactively (format nil "pdflatex ~s" output))))))
    ;  (values splot-input function-values)
      (cl-user::run-interactively "mogrify -verbose -density 500 -resize 1600 -format png ./*.pdf")
      (cl-user::run-interactively (format nil "mv plot* /rds/general/user/sgs16/home/plots"))
      t;(values sample-points (mapcar #'axis-extent '(:x :y :z)))
)))
