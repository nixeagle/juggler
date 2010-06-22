(in-package :juggler)

(defstruct material
  "Doc me!"
  (ambient-weight nil :type (or fixnum single-float))
  (diffuse-weight nil :type (or fixnum single-float))
  (specular-weight nil :type (or fixnum single-float))
  (reflection-weight nil :type (or fixnum single-float))
  (shininess nil :type (or fixnum single-float))
  (diffuse-color nil :type simple-vector)
  (highlight-color nil :type simple-vector)
  (reflection-color nil :type simple-vector))


(defparameter +red-plastic+
  (make-material :ambient-weight 0.1
                 :diffuse-weight 2.0
                 :specular-weight 1.0
                 :reflection-weight 0.0
                 :shininess 10.0
                 :diffuse-color #(1 0 0)
                 :highlight-color #(1 1 1)
                 :reflection-color #(0 0 0))
  "An object that looks like red plastic, whoo!")


;;; things working with materials, should be moved to better named files
;;; later.

(defstruct object
  "All raytraced objects ought to inherit from this")

(defstruct intersection
  (time nil :type single-float)
  (hit nil :type real-vector)
  (normal nil :type real-vector)
  (material nil :type material))

(defstruct (sphere (:include object))
  (center nil :type real-vector)
  (radius nil :type (or single-float fixnum))
  (material nil :type material))

;;; Make a red sphere like this:
;;; (make-sphere :center #(0 0 0) :radius 50 :material +red-plastic+)

(defmacro intersect-macro (o d primary-ray max-time intersection center material radius)
  `(let* ((a (subtract-vector ,o ,center))
	 (b (* 2.0 (dot-product ,d a)))
	 (c (- (magnitude-squared a) (expt ,radius 2)))
	 (e (- 4 (* (expt b 2) c))))
    (cond ((>= e 0)
	   (let* ((t1 (* 0.5 (- (- b) (sqrt e))))
		 (t2 (* 0.5 (+ (- b) (sqrt e))))
		 (intersected nil))
	     (cond ((and (>= t1 *epsilon-lower-time-bound*)
			 (<= t1 ,max-time))
		    (setf (intersection-time intersection) t1)
		    (setf intersected t))
		   ((and (>= t2 *epsilon-lower-time-bound*)
			 (<= t2 ,max-time))
		    (setf (intersection-time ,intersection) t2)
		    (setf intersected t)))
	     (cond ((and ,primary-ray intersected)
		    (setf (intersection-hit ,intersection)
			  (ray ,o ,d (intersection-time ,intersection)))
		    (setf (intersection-normal ,intersection) (construct-unit-vector
							      (intersection-hit ,intersection)
							      ,center))
		    (setf (intersection-material ,intersection) ,material)))
	     (if intersected 't 'f))))))

(defmethod intersect ((o real-vector) (d real-vector) (primary-ray boolean)
		      (max-time real) (intersection intersection) (object sphere))
  (intersect-macro o d primary-ray max-time intersection (slot-value object 'center) (slot-value object 'material)
		   (slot-value object 'center)))


;;; END
