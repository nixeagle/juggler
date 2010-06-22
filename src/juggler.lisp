(in-package :juggler)

(deftype 3d-vector ()
  '(vector real 3))

(deftype real-vector ()
  "A vector containing only real numbers."
  '(vector real))

(defun make-3d-vector (x y z)
"Deprecated, use make-real-vector instead"
  (declare (real x y z))
  (make-array 3 :element-type 'real
              :initial-contents (list x y z)))

(defun make-real-vector (&rest vector)
  (apply #'vector vector))

;;; This define x y and z, aliases for `aref' on a vector.
(declaim (inline x y z))
(macrolet ((def (name index)
             `(defun ,name (vector)
                ,(format nil "Grabs the ~(~A~) direction out of the vector."
                        name)
                (declare (real-vector vector))
                (svref vector ,index))))
  (def x 0)
  (def y 1)
  (def z 2))

(defun vector= (vector1 vector2)
   (declare (real-vector vector1 vector2))
   (loop for a across vector1
        for b across vector2
        always (= a b)))

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

(defun magnitude-squared (vector)
  (declare (real-vector vector))
  (expt (magnitude vector) 2))

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
  (let ((x1 (x vector1)) (y1 (y vector1)) (z1 (z vector1))
        (x2 (x vector2)) (y2 (y vector2)) (z2 (z vector2)))
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

#+ ()
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

(defun onb (w &optional (u #(0 0 0)) (v #(0 0 0)))
  "Defines a right handed coordinate system."
  (declare (3d-vector u v w)
           (ignore v))
  (cond
    ((vector= w #V(0 1 0)) (values #V(1 0 0) #V(0 0 -1)))
    ((vector= w #V(0 -1 0) (values #V(1 0 0) #V(0 0 1))))
    (t (values (vector (z w) 0 (- (x w)))
               (cross-product w (vector (z w) 0 (- (x w))))))))

(defun onb! (w u v)
  (declare (3d-vector u v w))
  (multiple-value-bind (u-result v-result) (onb w u v)
    (setf u u-result)
    (setf v v-result)))

(defun map-2d-to-3d (2d-point origin u v)
  (declare ((vector real 2) 2d-point)
           (3d-vector origin u v))
  (add-vectors origin (scale u (svref 2d-point 0))
               (scale v (svref 2d-point 1))))

(defun discriminant (a b c)
  "Delta = b^2 - 4ac"
  (- (expt b 2) (* 4 a c)))

(defun quadratic-roots (a b discriminant &aux (b (sqrt b)))
  (let ((bottom (* 2 a)))
    (values (/ (- (- b) discriminant) bottom)
            (/ (- b discriminant) bottom))))

;;; Screen math
(defun translate-2d-coordinate (x y &key (width *width*) (height *height*))
  "Translate the coordinates from upper left based to center based.

assuming a 2x2 grid with #(0 0) at the upper left, this translation
changes the addressing such that the old #(0 0) is now #(-1 1)."
  (declare (real x y width height))
  (the (vector real 2) (vector (- x (/ width 2)) (- (/ height 2) y))))

(defun virtual-screen-height (virtual-width height width)
  "Determine virtual screen height from existing image proportions.

Our ratio is this:

  virtual-width    width
  -------------- = ------
  virtual-height   height"
  (declare (real virtual-width height width))
  (/ (* virtual-width height) width))

(defun transform (point u v w &key (center *center*))
  "Translates POINT to the virtual coordinate system.

U V W need to be at right angles to each other."
  (assert (vector= u (cross-product v w)))
  (add-vectors center
               (scale u (x point))
               (scale v (y point))
               (scale w (z point))))

(defun intersectp (sphere origin direction
                   &key max-time (min-time *epsilon-lower-time-bound*)
                   (center *center*))
  (let ((intersection (subtract-vector origin center)))
    (let ((p (* 2 (dot-product direction intersection)))
          (c (- (magnitude intersection)
                (expt (sphere-radius sphere) 2))))
      (let ((square (discriminant 1 p c)))
        (when (>= square 0)
          (multiple-value-bind (t1 t2) (quadratic-roots 1 p square)
            (cond
              ((<= min-time t1 max-time) (values t t1))
              ((<= min-time t2 max-time) (values t t2)))))))))

(defun gamma (color gamma)
  "Return the gamma-corrected color.

Often used for contrasts. Formula is 255*c^(1/g)"
  (declare (real color gamma))
  (* 255 (expt color (/ 1 gamma))))

(defun gamma-3-colors (colors gamma)
  "Return a vector of 3, with gamma corrected colors.

See `gamma' for details"
  (declare (real-vector colors)
	   (real gamma))
  (apply #'make-real-vector (loop for i across colors
                     collect (gamma i gamma))))

;;; END
