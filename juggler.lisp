(defpackage #:juggler
  (:use :cl :nutils))

(in-package :juggler)

(defstruct (3d-vector
             (:constructor make-3d-vector (x y z)))
  "Models a vector in 3d.

This is a struct mostly for speed reasons, these act like C arrays but
with much greater type safety built in."
  (x nil :type real)
  (y nil :type real)
  (z nil :type real))

(defun vector-add (vector1 vector2)
  "Adds two vectors."
  (declare (3d-vector vector1 vector2))
  (with-slots ((x1 x) (y1 y) (z1 z)) vector1
    (with-slots ((x2 x) (y2 y) (z2 z)) vector2
      (make-3d-vector (+ x1 x2) (+ y1 y2) (+ z1 z2)))))


(defun magnitude (vector)
  (declare (3d-vector vector))
  (with-slots (x y z) vector
    (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))))

(defun scale (vector scale-factor)
  (declare (3d-vector vector)
           (real scale-factor))
  (with-slots (x y z) vector
    (make-3d-vector (* x scale-factor) (* y scale-factor) (* z scale-factor))))

(defun negate (vector)
  "Invert VECTOR by multiplying by -1."
  (declare (3d-vector vector))
  (scale vector -1))

(defun unit-vector (vector)
  "Compute unit vector of VECTOR."
  (declare (3d-vector vector))
  (scale vector (/ 1 (magnitude vector))))

