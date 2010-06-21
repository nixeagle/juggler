(in-package :juggler)

#+ () (defstruct (3d-vector
             (:constructor make-3d-vector (x y z)))
  "Models a vector in 3d.

This is a struct mostly for speed reasons, these act like C arrays but
with much greater type safety built in."
  (x nil :type real)
  (y nil :type real)
  (z nil :type real))

(deftype 3d-vector ()
  '(vector real 3))

(deftype real-vector ()
  "A vector containing only real numbers."
  '(vector real))

(defun make-3d-vector (x y z)
  (declare (real x y z))
  (make-array 3 :element-type 'real
              :initial-contents (list x y z)))

;;; Set reader macro so #V(1 2 3) works
(set-dispatch-macro-character #\# #\V
                              #'(lambda (stream char1 char2)
                                  (declare (ignore char1 char2))
                                  (assert (char= #\( (read-char stream)))
                                  (let ((vect (read-delimited-list #\) stream)))
                                    (make-array (length vect)
                                                :element-type 'real
                                                :initial-contents vect))))

(defun add-vector (vector1 vector2)
  "Adds two vectors."
  (declare (3d-vector vector1 vector2))
  (make-array (length vector1) :element-type 'real
              :initial-contents (loop
                                   for i across vector1
                                   for j across vector2
                                   collect (+ i j))))

(defun add-vectors (&rest vectors)
  "Add more then 2 VECTORS at a time."
  (declare ((cons 3d-vector (cons 3d-vector)) vectors))
  (if (length= 2 vectors)
      (add-vector (car vectors) (cadr vectors))
      (apply #'add-vectors (add-vector (car vectors) (cadr vectors))
             (cddr vectors))))

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
  (declare (real-vector vector))
  (sqrt (loop for i across vector
           summing (expt i 2))))

(defun scale (vector scale-factor)
  "You can scale a vector (change its length) by
multiplying it by a constant"
  (declare (real-vector vector)
           (real scale-factor))
  (apply #'vector (loop for i across vector collect (* i scale-factor))))

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

(defun pairwise-multiply-vector (vector1 vector2)
  "Multiply each direction pairwise.

This is not really a valid mathematical operation, but for us it
simplifies writing other functions that need an equivalent to this as part
of its sub operations.

Pairwise here means multiply each 'x', each 'y', each 'z'."
  (declare (3d-vector vector1 vector2))
  (with-slots (x y z) vector1
    (with-slots ((u x) (v y) (w z)) vector2
      (make-3d-vector (* x u) (* y v) (* z w)))))

(defun transform (point origin u v w)
  "Transform POINT with respect to unitvectors U V W."
  (declare (3d-vector point origin u v w))
  (add-vectors origin (pairwise-multiply-vector point u)
               (pairwise-multiply-vector point v)
               (pairwise-multiply-vector point w)))

;;; END
