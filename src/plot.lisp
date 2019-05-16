(in-package :funcl)
(annot:enable-annot-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lparallel:*kernel*
        (lparallel:make-kernel (or (hpc-tools:ncpus) 1))))

@export
(defclass gnuplot-options ()
  ((title :initarg :title :accessor title)
   (debug :initarg :debug :accessor debug-p :initform nil)
   (granularity :initarg :granularity :accessor granularity :initform 1000)
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
(defun gnuplot-options (&key (title "") (x11 nil) (x-label "") (x-min 0) (x-max 1) (y-label ""))
  (make-instance 'gnuplot-options :title title :wait-for-x11 t :x-label x-label :y-label y-label
                 :x-min x-min :x-max x-max
                 :terminals (if x11 
                                (list 
                                 (list '(:cairolatex :pdf :color :standalone :blacktext
                                         :|size 16cm ,10.5cm|) 
                                       (plot-output-now))
                                 (list '(:x11) (lambda () nil)))
                                (list 
                                 (list '(:cairolatex :pdf :color :standalone :blacktext
                                         :|size 16cm ,10.5cm|) 
                                       (plot-output-now))))))

@export
(defgeneric plot-with-options (function options))

@export
(defun plot (function)
  (plot-with-options function (simple-gnuplot-options)))

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

(defmethod plot-with-options ((function-list list) (options gnuplot-options))
  (with-slots (debug range granularity terminals wait-for-x11 x-label y-label x-min x-max) options
    (loop for (terminal output-lambda) in terminals do
         (let ((output (funcall output-lambda)))
           (eazy-gnuplot:with-plots (*standard-output* :debug debug)
             (eazy-gnuplot:gp-setup :terminal terminal :output output :style '(fill pattern 5))
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
                       :with '(:lines) :title (remove-if (alexandria:curry #'char= #\#)  (name function))))))
             (when (and (member :x11 terminal)
                        wait-for-x11) (format t "~&pause mouse button1;~%")))
           (when (member :pdf terminal) (cl-user::run-interactively (format nil "pdflatex ~s" output))))))
  (cl-user::run-interactively "mogrify -verbose -density 500 -resize 1600 -format png ./*.pdf")
  (cl-user::run-interactively (format nil "mv plot* /rds/general/user/sgs16/home/plots"))
  t)

(defmethod plot-with-options ((function funcl-function) (options gnuplot-options))
  (plot-with-options (list function) options))

(defun plot-slice (function &key (lower #(0 0))
                                 (higher #(1 1))
                                 (axis :z)
                                 slice-value
                                 (resolution 100)
                                 (contours t)
                                 (heatmap t)
                                 (options (simple-gnuplot-options)))
  (labels ((axis-extent (ax)
             (case ax
               (:x (list (aref lower 0) (aref higher 0)))
               (:y (list (aref lower (if (eq axis :z) 0 1))
                         (aref higher (if (eq axis :z) 0 1))))
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
           (function-values (map 'vector (lambda-function function) sample-points))
           (splot-input (map 'vector (lambda (sample-point evaluation)
                                       (vector (aref sample-point (car (dual-axes)))
                                               (aref sample-point (cadr (dual-axes))) evaluation))
                             sample-points function-values)))
      (with-slots (debug range granularity terminals wait-for-x11 x-label y-label x-min x-max) options
        (loop for (terminal output-lambda) in terminals do
              (let ((output (funcall output-lambda)))
                (eazy-gnuplot:with-plots (*standard-output* :debug debug)
                  (eazy-gnuplot:gp-setup :terminal terminal :output output; :style '(fill solid)
                                         )
                  ;(format t "~%set dgrid3d 10,10~%")
 (format t "~%set style increment default~%")
                 (format t "~%set style data lines~%")
                  (format t "~%set view map scale 1~%")
                  (format t "~%set pm3d map impl")
                  (format t "~%set contour~%")
                  (format t "~%set cntrlabel  format '%8.3g' font ',4' start 2 interval 20~%")
                  (format t "~%set cntrparam levels auto 10~%")
                  (format t "~%set cntrparam bspline~%");
                  (format t "~%set palette rgbformulae 22,13,10~%");
                  (format t "~%set samples 25, 25~%")
                  (format t "~%set isosamples 26, 26~%")
                  (format t "~%set nokey~%")
                  (eazy-gnuplot:splot (lambda ()
                                        (format t "~{ ~{~,6f~%~}~%~}" (group (coerce function-values 'list)
                                                                             resolution))
                                        )
                                      :using 1 :with :lines)
                  (eazy-gnuplot:splot (lambda ()
                                        (format t "~{ ~{~,6f~%~}~%~}" (group (coerce function-values 'list)
                                                                             resolution))
                                       )
                                      :using 1 :with :labels)
                  (when (and (member :x11 terminal)
                             wait-for-x11) (format t "~&pause mouse keypress;~%")))
                (when (member :pdf terminal) (cl-user::run-interactively (format nil "pdflatex ~s" output))))))
    ;  (values splot-input function-values)
      (cl-user::run-interactively "mogrify -verbose -density 500 -resize 1600 -format png ./*.pdf")
      (cl-user::run-interactively (format nil "mv plot* /rds/general/user/sgs16/home/plots"))
      t)))
