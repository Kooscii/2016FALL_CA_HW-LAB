(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '(external-program cl-fad cl-ppcre)))

(defmacro while (test &body body)
  "Repeatedly do body as long as test is true."   ; LMH
  `(do ()
       ((not ,test))
     ,@body))

(defun simulate (i)
  (run i)
  (print (compare i)))

(defun run (i)
  (with-open-file (s #P"config2.txt" :direction :output
		     :if-exists :supersede :if-does-not-exist :create)
    (princ i s))
  (external-program:run "./branchsimulator.out" '("config2.txt" "trace.txt"))
  (fad:copy-file "trace.txt.out" (format nil "trace-~a.txt" i) :overwrite t))

(defun compare (i)
  (with-open-file (origin #P "trace.txt")
    (with-open-file (ith (format nil "trace-~a.txt" i))
      (let ((total 0)
	    (mis 0))
	(while (listen origin)
	  (let ((orig (parse-integer (second (ppcre:split " " (read-line origin)))))
		(pred (parse-integer (read-line ith))))
	    (incf total)
	    (if (/= orig pred)
		(incf mis))))
	(float (/ mis total))))))

(loop for i from 10 to 20 do
     (simulate i))
