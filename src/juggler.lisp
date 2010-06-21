(in-package :juggler)

(defstruct (3d-vector
             (:constructor make-3d-vector (x y z)))
  "Models a vector in 3d.

This is a struct mostly for speed reasons, these act like C arrays but
with much greater type safety built in."
  (x nil :type real)
  (y nil :type real)
  (z nil :type real))

;;; Set reader macro so #V(1 2 3) works
(set-dispatch-macro-character #\# #\V
                              #'(lambda (stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (assert (char= #\( (read-char stream)))
                                  (let ((vect (read-delimited-list #\) stream)))
                                    (assert (length= 3 vect))
                                    (make-3d-vector (first vect)
                                                    (second vect)
                                                    (third vect)))))

(defmethod print-object ((object 3d-vector) stream)
  (format stream "#V(~A ~A ~A)" (3d-vector-x object)
          (3d-vector-y object)
          (3d-vector-z object)))

(defmethod make-load-form ((object 3d-vector) &optional environment)
  "Correct form for loading a 3d vector in a fasl."
  (declare (ignore environment))
  (with-slots (x y z) object
   `(make-3d-vector ,x ,y ,z)))

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
  "The magnitude of a vector.

Also known as the 'length' of a vector is computed using the 3-dimensional
version of the Pythagorean Theorem."
  (declare (3d-vector vector))
  (with-slots (x y z) vector
    (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))))

(defun scale (vector scale-factor)
"You can scale a vector (change its length) by
multiplying it by a constant"
  (declare (3d-vector vector)
           (real scale-factor))
  (with-slots (x y z) vector
    (make-3d-vector (* x scale-factor) (* y scale-factor) (* z scale-factor))))

(defun divide-vector (vector divide-factor)
"Basically inverted scale"
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
  "useful when measuring distance over time.

The distance is multiplied by the scale-factor, then added to position.
Think of starting at position, going distance(so many miles) per
scale-factor(hour)"
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

  v=[a b c]
  u=[d e f]
  uv=ad+be+cf"
  (declare (3d-vector vector1 vector2))
  (with-slots ((x1 x) (y1 y) (z1 z)) vector1
    (with-slots ((x2 x) (y2 y) (z2 z)) vector2
      (+ (* x1 x2) (* y1 y2) (* z1 z2)))))

(defun subtract-vector (vector1 vector2)
  "Subtract two vectors"
  (add-vector vector1 (negate vector2)))

(defun subtract-real-vector (vector real)
  "Subtract a constant from VECTOR"
  (add-real-vector vector (* -1 real)))

;;; END