(defparameter *phb*
  (make-array '(3 4)
	      :element-type '(unsigned-byte 2)
	      :initial-element 0))

(defparameter *large* 100000)

(defparameter *print-large* 10)

(defparameter *bhr* 0)

(defun predict (j)
  "Predict Bj, return T if taken."
  (ecase (aref *phb* (1- j) *bhr*)
    ((2 3) t)
    ((0 1) nil)))

(defparameter *mispredications* (make-array 3 :initial-element 0))

(defun takenp (i &key b)
  (ecase b
    (1 t)
    (2 (= (mod i 4) 0))
    (3 (evenp i))))

(defun incf-mispredication (i &key b)
  (if (not (eql (predict b) (takenp i :b b)))
      (incf (aref *mispredications* (1- b)))))

(defun to-taken (bit)
  (ecase bit
    (#b00 #b01)
    ((#b10 #b01 #b11) #b11)))

(defun to-not-taken (bit)
  (ecase bit
    (#b11 #b10)
    ((#b00 #b01 #b10) #b00)))

(defun update-phb (i &key b)
  (setf (aref *phb* (1- b) *bhr*)
	(if (takenp i :b b)
	    (to-taken (aref *phb* (1- b) *bhr*))
	    (to-not-taken (aref *phb* (1- b) *bhr*)))))

(defun update-bhr (i &key b)
  (shiftf (ldb (byte 1 0) *bhr*)
	  (ldb (byte 1 1) *bhr*)
	  (if (takenp i :b b)
	      1 0)))

(defun simi (i &key b)
  (incf-mispredication i :b b)
  (update-phb i :b b)
  (update-bhr i :b b))

(defmethod sim (i)
  (simi i :b 1)
  (simi i :b 2)
  (simi i :b 3))

(loop for i below *large*
   do (sim i))
