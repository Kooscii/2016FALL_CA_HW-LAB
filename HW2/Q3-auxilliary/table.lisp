(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(vecto cl-csv))
  (use-package :vecto))

(defparameter *font-path* "~/temp/Roboto-Regular.ttf")

(defparameter *dist-x* 230)

(defparameter *stages* nil)

(defparameter *rows* nil)

(defparameter *offset* nil)

(defparameter *canvas-height* 600)

(defparameter *canvas-width* 3000)

(defun draw-text (text x y &optional (r 0) (g 0) (b 0))
  (set-rgb-fill r g b)
  (draw-centered-string x y text)
  (set-rgba-fill 0 0 0 0))

(defun draw-box (title subtitle x y)
  (let* ((font (get-font *font-path*))
	 (title-font-size 40)
	 (subtitle-font-size 20)
	 (width 200)
	 (height 100)
	 (round 10) 
	 (center-x (+ x (/ width 2)))
	 (center-y (+ y (/ height 2) -5))
	 (bottom-y (+ y 10)))
    
    (set-rgb-stroke 0 0 0)
    (rounded-rectangle x y width height round round)
    (stroke)

    (set-font font title-font-size)
    (draw-text title center-x center-y)

    (set-font font subtitle-font-size)
    (draw-text subtitle center-x bottom-y 1 0 0)))

(defun draw-row (start-x start-y stage-row row)
  (loop 
     for stage in stage-row
     for i in row
     for x = start-x then (+ x *dist-x*)
     do (draw-box stage i x start-y)))

(defun draw-rows (start-x start-y stages rows offset)
  (loop
     for row in rows
     for stage-row in stages
     for off in offset
     for y = start-y then (- y 150)
     for x = start-x then (+ start-x (* off *dist-x*))
     do (draw-row x y stage-row row)))

(defun draw-time-line (n)
  (let* ((font (get-font *font-path*))
	 (font-size 40) 
	 (start-x 20)
	 (step-x *dist-x*))
    (set-font font font-size)
    (set-rgb-stroke 51/255 102/255 1)
    (set-line-width 3)
    (move-to start-x (- *canvas-height* 50))
    (loop for i below n
       for x = start-x then (+ x step-x)
       do (draw-text (format nil "t~a" i) x (- *canvas-height* 30))
       finally (progn
		 (draw-text "Time" (+ x step-x) (- *canvas-height* 30))
		 (line-to (+ x step-x) (- *canvas-height* 50))))
    (stroke)))

(defun draw-instruction-line (len)
  (let* ((font (get-font *font-path*))
	 (font-size 40) 
	 (start-y (- *canvas-height* 50)))
    (set-font font font-size)
    (set-rgb-stroke 51/255 102/255 1)
    (set-line-width 3)
    (move-to 20 start-y)
    (line-to 20 (- start-y len))
    (draw-text "Instructions" 170 (- start-y len))
    (stroke)))

(defun read-ins-from-csv (csv-file)
  (setf *rows* nil)
  (setf *stages* nil)
  (setf *offset* nil)
  (cl-csv:do-csv (row (pathname csv-file))
    (let (i s (off -1) save-off)
      (dolist (item row)
	(incf off)
	(when (plusp (length item))
	  (unless save-off
	    (setf save-off off))
	  (if (member item '("IF" "ID/RF" "EX" "MEM" "WB") :test 'string=)
	      (push item s)
	      (push item i))))
      (if i
	  (push (nreverse i) *rows*)
	  (push (nreverse s) *stages*))
      (pushnew save-off *offset*)))
  (setf *offset* (nreverse *offset*))
  (setf *rows* (nreverse *rows*))
  (setf *stages* (nreverse *stages*)))

(defun draw-all (input-csv &optional (file :untitled))
  (if (eql file :untitled)
      (let ((input-csv (pathname input-csv)))
	(setf file (make-pathname :type "png" :defaults input-csv))))
  (read-ins-from-csv input-csv)
  (setf *canvas-width* (apply '+ 1000 (mapcar (lambda (i) (* 230 (- (length i) 4))) *rows*))
	*canvas-height* (+ 100 (* 150 (length *rows*))))
  (with-canvas (:width *canvas-width* :height *canvas-height*)
    (draw-instruction-line (- *canvas-height* 100))
    (draw-time-line (apply '+ 4 (mapcar (lambda (i) (- (length i) 4)) *rows*)))
    (draw-rows 50 (- *canvas-height* 180) *stages* *rows* *offset*)

    (save-png file)))

