(in-package :juggler)

;;; preliminary basics required for rendering anything
(defun render ()
  (let* ((w (construct-unit-vector *eye* *look*))
         (c (ray *eye* w (- *screen-distance*))))
    (multiple-value-bind (u v) (onb w)
      (list u v w))))


