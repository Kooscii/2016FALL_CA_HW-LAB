(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; need to install graphvitz manually.
  (ql:quickload :donuts)
  (in-package :donuts))

($ (:outfile "seq.png")
   (&& (-> "A" "B")
       (-> "B" "C")
       (-> "A" "D")
       (-> "D" "E")
       (-> "D" "F")))
