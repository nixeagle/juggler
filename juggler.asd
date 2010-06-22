(asdf:defsystem :juggler
  :depends-on (:nutils :eos :cl-gd)
  :components
  ((:file "packages")
   (:module :src
            :depends-on ("packages")
            :components
            ((:file "readmacro")
             (:file "globals")
             (:file "materials")
             (:file "random")
             (:file "juggler" :depends-on ("globals" "readmacro"))
             (:file "render" :depends-on ("juggler" "random"))))))

;;; END
