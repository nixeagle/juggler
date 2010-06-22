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
  "An object that loks like red plastic, whoo!")