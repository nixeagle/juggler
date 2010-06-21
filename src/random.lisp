;;; Various utilities related to randomness

(in-package :juggler)

(defun make-random-vector (size &optional (number 1.0))
  "Makes a vector with SIZE random NUMBERs in it."
  (declare (positive-fixnum size))
  (apply #'vector
   (loop for i from 0 to (1- size)
      collect (random number))))

(let ((vector (make-random-vector 10000))
      (i 0))
  (let ((vector-length (length vector)))
    (declare ((integer 0 #.(1- most-positive-fixnum)) i vector-length))
    (defun next-random-float ()
      "Grab a float from vector of pregenerated floats."
      (declare (optimize (speed 3) (safety 0) (debug 0)))
      (multiple-value-prog1 (svref vector i)
        (when (= vector-length (the fixnum (incf i)))
          (setf i 0))))))

;;; END
