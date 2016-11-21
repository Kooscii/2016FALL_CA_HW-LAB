;;;; Find the best rearrangement for least time

(defparameter *seq* '((b . c)
                      (d . e)
                      (d . f)))
(defparameter *r* '((b 1) (c 4 2) (d 1) (e 1 5) (f 5 3)))

(defparameter *w* '((b 4) (c 5) (d) (e 2) (f 3)))

(defun allow-p (arrange)
  (and (= (length arrange) (length (remove-duplicates arrange)))
       (every #'(lambda (x)
		  (< (position (car x) arrange)
		     (position (cdr x) arrange))) *seq*)))

(defun total-time (arrange)
  (apply #'+ 5
         (mapcar #'(lambda (a b)
                     (let ((w (cadr (assoc a *w*))))
                       (if (and w (find w (cdr (assoc b *r*))))
                           3
                         1)))
           arrange
           (cdr arrange))))

(defparameter *paths* nil)

(defconstant failsym :finished)

(defmacro choose-bind (var choices &body body)
  `(cb #'(lambda (,var) ,@body) ,choices))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-choose-let (bindings doing)
    (if bindings
	`(choose-bind ,(caar bindings) ,(cadar bindings)
	   ,(expand-choose-let (cdr bindings) doing))
	doing)))

(defmacro choose-let (bindings &body body)
  (expand-choose-let bindings `(progn ,@body)))

(defun cb (fn choices)
  (declare (function fn))		
  (if choices
     (progn
       (if (cdr choices)
           (push #'(lambda () (cb fn (cdr choices)))
                 *paths*))
       (funcall fn (car choices)))
     (fail)))

(defun fail ()
  (if *paths*
      (funcall (the function (pop *paths*))) 
      failsym))

(defparameter *all* '(b c d e f))

(defun all-result ()
  (let ((*paths* nil)
	(shortest 999))
    (choose-let ((i1 *all*) (i2 *all*) (i3 *all*) (i4 *all*) (i5 *all*))
      (let ((arrange (list i1 i2 i3 i4 i5)))
	(when (allow-p arrange)
	  (let ((time (total-time arrange)))
	    (when (<= time shortest)
	      (setf shortest time)
	      (format t "~a ~a ~a ~a ~a: ~a~%" i1 i2 i3 i4 i5 time))))
	(fail)))))
