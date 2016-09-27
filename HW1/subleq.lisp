;;;; a subleq interpreter
;;;; an online ecl: http://colabv6.dan.co.jp/lleval.html
(defparameter *mem* (make-array 4))
(eval-when (:load-toplevel :eval-toplevel :execute)
  (defmacro subleq (a b c)
  `(progn
     (decf (aref *mem* ,a) (aref *mem* ,b))
     (print *mem*)
     (if (<= (aref *mem* ,a) 0) (go ,c)))))

(let ((a 0) (b 1) (c 2) (d 3))
  (setf (aref *mem* a) 7
	(aref *mem* b) -9
	(aref *mem* c) 4
	(aref *mem* d) 3)
  (tagbody
   L0 (subleq C C L1)
   L1 (subleq D D L2)
   L2 (subleq A B L6)
   L3 (subleq D B L4)
   L4 (subleq C D L5)
   L5 (subleq D D L9)
   L6 (subleq D A L7)
   L7 (subleq C D L8)
   L8 (subleq D D L9)
   L9 (print *mem*)))
