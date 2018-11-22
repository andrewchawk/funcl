(in-package :funcl)
(annot:enable-annot-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lparallel:*kernel* (lparallel:make-kernel 48)))

@export
(defclass gnuplot-options ()
  ((title :initarg :title :accessor title)
   (debug :initarg :debug :accessor debug-p :initform nil)
   (granularity :initarg :granularity :accessor granularity :initform 1000)
   (terminals :initarg :terminals :accessor terminals :initform :x11)
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
                                 (wait-for-x11 t))
  (make-instance 'gnuplot-options :title title :terminals terminals :wait-for-x11 wait-for-x11))

@export
(defgeneric plot-with-options (function options))

@export
(defun plot (function)
  (plot-with-options function (simple-gnuplot-options)))

(defun function-plot-start (function)
  (with-slots (domain) function
    (if (consp domain)
        (car domain)
        0)))

(defun function-plot-end (function)
  (with-slots (domain) function
    (if (consp domain)
        (cdr domain)
        1)))

(defmethod plot-with-options ((function-list list) (options gnuplot-options))
  (with-slots (debug range granularity terminals wait-for-x11) options
    (loop for (terminal output-lambda) in terminals do
         (let ((output (funcall output-lambda)))
           (eazy-gnuplot:with-plots (*standard-output* :debug debug)
             (eazy-gnuplot:gp-setup :terminal terminal :output output :style '(fill pattern 5))
             (when (slot-boundp options 'title)
               (format t "~% set title ~s" (title options))
               (format t "~% show title ~%"))
             (loop for function in function-list
                for index from 1 doing
                  (with-slots (lambda-function domain) function
                    (let ((start (function-plot-start function))
                          (end (function-plot-end function)))
                      (eazy-gnuplot:plot
                       (lambda ()
                         (let* ((values (alexandria:iota granularity :step (/ (- end start) granularity) :start start))
                                (function-values (lparallel:pmapcar lambda-function values)))
                           (mapcar (lambda (i j) (format t "~&~,6f ~F" i j)) values function-values)))
                       :with '(:lines) :title (name function)))))
             (when (and (member :x11 terminal)
                        wait-for-x11) (format t "~&pause mouse button1;~%")))
           (when (member :pdf terminal) (cl-user::run-interactively (format nil "pdflatex ~s" output))))))
  (bt:make-thread 
   (lambda ()
     (cl-user::run-interactively "mogrify -verbose -density 500 -resize 1600 -format png ./*.pdf")
     (cl-user::run-interactively (format nil "mv plot* /home/sgs16/plots"))))
  t)

(defmethod plot-with-options ((function funcl-function) (options gnuplot-options))
  (plot-with-options (list function) options))
