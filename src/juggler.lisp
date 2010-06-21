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
  (declare (real-vector vector1 vector2))
  (apply #'vector (loop
                     for i across vector1
                     for j across vector2
                     collect (+ i j))))

(defun add-vectors (&rest vectors)
  "Add more then 2 VECTORS at a time."
  (declare ((cons real-vector (cons real-vector)) vectors))
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
  (declare (real-vector vector)
           (real divide-factor))
  (scale vector (/ 1 divide-factor)))

(defun negate (vector)
  "Invert VECTOR by multiplying by -1."
  (declare (real-vector vector))
  (scale vector -1))

(defun unit-vector (vector)
  "Compute unit vector of VECTOR."
  (declare (real-vector vector))
  (scale vector (/ 1 (magnitude vector))))

(defun ray (position distance scale-factor)
  "useful when measuring distance over time.

The distance is multiplied by the scale-factor, then added to position.
Think of starting at position, going distance(so many miles) per
scale-factor(hour)"
  (declare (real-vector position)
	   (real-vector distance)
	   (real scale-factor))
  (add-vector position (scale distance scale-factor)))


(defun cross-product (vector1 vector2)
  "VECTOR1 cross VECTOR2

Result is the vector that is perpendicular to both VECTOR1 and VECTOR2.

For example #V[1 0 0] #V[0 1 0] results in #V[0 0 1]. Imagine that the
first two vectors are the X and the Y axis on a coordinate chart, the
result of this function defines the Z axis."
  (declare (3d-vector vector1 vector2))
  (let ((x1 (svref vector1 0)) (y1 (svref vector1 1))
        (z1 (svref vector1 2)) (x2 (svref vector2 0))
        (y2 (svref vector2 1)) (z2 (svref vector2 2)))
    (vector (- (* y1 z2)
               (* z1 y2))
            (- (* z1 x2)
               (* x1 z2))
            (- (* x1 y2)
               (* y1 x2)))))

(defun dot-product (vector1 vector2)
  "v is one vector, u is the other vector, and abc, and def,
are coordinates for v and u respectfully

  v=[a b c]
  u=[d e f]
  uv=ad+be+cf"
  (declare (real-vector vector1 vector2))
  (loop for i across vector1
       for j across vector2
     summing (* i j)))

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
  (declare (real-vector vector1 vector2))
  (apply #'vector
         (loop
            for i across vector1
            for j across vector2
            collect (* i j))))

(defun transform (point origin u v w)
  "Transform POINT with respect to unitvectors U V W."
  (declare (real-vector point origin u v w))
  (add-vectors origin (pairwise-multiply-vector point u)
               (pairwise-multiply-vector point v)
               (pairwise-multiply-vector point w)))

(defun construct-unit-vector (head tail)
  (declare (real-vector head tail))
  (unit-vector (subtract-vector head tail)))

(defun vector= (&rest vectors)
  (loop for vector in (cdr vectors)
     always (loop
               for a across (car vectors)
               for b across vector
               always (< (abs (- a b)) 0.000000001))))

(defun onb (u v w)
  "Defines a right handed coordinate system."
  (declare (3d-vector u v w)
           (ignore v))
  (cond
    ((vector= w #V(0 1 0)) (values #V(1 0 0)
                                  #V(0 0 -1)))
    ((vector= w #V(0 -1 0) (values #V(1 0 0)
                                  #V(0 0 1))))
    (t (values (vector (svref w 2) 0 (- (svref w 0)))
                       (cross-product w u)))))

(defun onb! (u v w)
  (declare (3d-vector u v w))
  (multiple-value-bind (u-result v-result) (onb u v w)
    (setf u u-result
          v v-result)))

(defun gamma (color gamma)
  "Return the gamma-corrected color. Often used for contrasts. Formula is 255*c^(1/g)"
  (declare (real color gamma))
  (* 255 (expt color (/ 1 gamma))))

(defun gamma-3-colors (colors gamma)
  "Return a vector of 3, with gamma corrected colors. See (gamma) for details"
  (declare (real-vector colors)
	   (real gamma))
  (apply #'vector (loop for i across colors
                     collect (gamma i gamma))))

;;; END
