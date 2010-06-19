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

(defun add-vector (vector1 vector2)
  "Adds two vectors."
  (declare (3d-vector vector1 vector2))
  (with-slots ((x1 x) (y1 y) (z1 z)) vector1
    (with-slots ((x2 x) (y2 y) (z2 z)) vector2
      (make-3d-vector (+ x1 x2) (+ y1 y2) (+ z1 z2)))))

(defun add-real-vector (vector real)
  "Add a constant to VECTOR"
  (declare (3d-vector vector) (real real))
  (with-slots (x y z) vector
    (make-3d-vector (+ x real)
                    (+ y real)
                    (+ z real))))

(defun magnitude (vector)
  (declare (3d-vector vector))
  (with-slots (x y z) vector
    (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))))

(defun scale (vector scale-factor)
  (declare (3d-vector vector)
           (real scale-factor))
  (with-slots (x y z) vector
    (make-3d-vector (* x scale-factor) (* y scale-factor) (* z scale-factor))))

(defun divide-vector (vector divide-factor)
  (declare (3d-vector vector)
           (real divide-factor))
  (scale vector (/ 1 divide-factor)))

(defun negate (vector)
  "Invert VECTOR by multiplying by -1."
  (declare (3d-vector vector))
  (scale vector -1))

(defun unit-vector (vector)
  "Compute unit vector of VECTOR."
  (declare (3d-vector vector))
  (scale vector (/ 1 (magnitude vector))))

(defun ray (position distance scale-factor)
  (declare (3d-vector position)
	   (3d-vector distance)
	   (real scale-factor))
  (add-vector position (scale distance scale-factor)))


(defun cross-product (vector1 vector2)
  "VECTOR1 cross VECTOR2

Result is the vector that is perpendicular to both VECTOR1 and VECTOR2.

For example #V[1 0 0] #V[0 1 0] results in #V[0 0 1]. Imagine that the
first two vectors are the X and the Y axis on a coordinate chart, the
result of this function defines the Z axis."
  (declare (3d-vector vector1 vector2))
  (with-slots ((x1 x) (y1 y) (z1 z)) vector1
    (with-slots ((x2 x) (y2 y) (z2 z)) vector2
      (make-3d-vector (- (* y1 z2)
                         (* z1 y2))
                      (- (* z1 x2)
                         (* x1 z2))
                      (- (* x1 y2)
                         (* y1 x2))))))

(defun dot-product (vector1 vector2)
"v is one vector, u is the other vector, and abc, and def,
are coordinates for v and u respectfully
v=abc
u=def
uv=ad+be+cf"
  (declare (3d-vector vector1 vector2))
  (with-slots ((x1 x) (y1 y) (z1 z)) vector1
    (with-slots ((x2 x) (y2 y) (z2 z)) vector2
      (+ (* x1 x2) (* y1 y2) (* z1 z2)))))

;;; END
