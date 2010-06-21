;;; Various utilities related to randomness

(in-package :juggler)

(defun make-random-vector (size &optional (number 1.0))
  "Makes a vector with SIZE random NUMBERs in it."
  (declare (positive-fixnum size))
  (apply #'vector
   (loop for i from 0 to (1- size)
      collect (random number))))