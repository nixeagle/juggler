;;; Most of this is simple inlined mathematics formulas

(in-package :juggler)

(declaim (inline inverse))
(defun inverse (number)
  "Divides 1 by NUMBER"
  (declare (number number))
  (/ 1 number))
