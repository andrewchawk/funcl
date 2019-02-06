(in-package #:funcl)

(annot:enable-annot-syntax)

@export
(defun lion-hunting (predicate &key (start 0) (end 100) (max-iter 30) (verbose nil))
  (if (funcall predicate end)
      (loop for count from 1 upto max-iter
            with a = start
            with b = end
            do (if (funcall predicate #1=(/ (+ a b) 2))
                   (progn (when verbose (format t "Round ~a / ~a, setting [a, b] = [~,6f, ~,6f].~%" count max-iter a #1#) (setf b #1#)))
                   (progn (when verbose (format t "Round ~a / ~a, setting [a, b] = [~,6f, ~,6f].~%" count max-iter #1# b) (setf a #1#))))
         finally (progn (when verbose (format t "Returning ~,6f.~%" b))  (return (coerce b 'float))))
      (progn (when verbose (format t "Returning ~,6f.~%" end)) end)))

@export
(defun numeric-predicate ( predicate function &key (granularity 100) (start 0) (end 1))
  (apply predicate (lparallel:pmapcar (lambda-function function)
                                      (loop for i from 0 to granularity collecting
                                           (+ start 
                                              (* i (/  (- end start) granularity)))))))
@export
(defun numeric-min (function &rest keys)
  (apply #'numeric-predicate #'min function keys))

@export
(defun numeric-max (function &rest keys)
  (apply #'numeric-predicate #'max function keys))

@export
(defun numeric-within-p (function min max &rest keys)
  (and (> (apply #'numeric-min function keys) min) 
       (< (apply #'numeric-max function keys) max)))

@export
(defun numeric-distance-to (function min max &rest keys)
  (max 0 
       (- min (apply #'numeric-min function keys))
       (- (apply #'numeric-max function keys) max)))

@export
(defun bisect (function a b &key (max-steps 100) (tolerance 0.0001))
  "Bisection method for functions."
  (loop for i from 0 below max-steps
       while (> (- b a) tolerance)
       do (let ((f-a (evaluate function a))
                ;(f-b (evaluate function b))
                (f-c (evaluate function (/ (+ a b) 2))))
            (if (eql (signum f-a) (signum f-c))
                (setf a (/ (+ a b) 2))
                (setf b (/ (+ a b) 2))))
     finally (return (coerce  (/ (+ a b) 2) 'float))))
