(in-package :juggler)


(defun render ()
  (let ((w (construct-unit-vector *eye* *look*)))
    (let ((c (ray *eye* w (- *screen-distance*)))
          (u #(0 0 0))
          (v #(0 0 0)))
      (multiple-value-bind (u v) (onb u v w)
        (list u v w c)))))

(ray *eye* (construct-unit-vector *eye* *look*) (- *screen-distance*))


(render)