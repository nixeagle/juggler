(asdf:defsystem :juggler
  :depends-on (:nutils :eos :cl-gd)
  :components
  ((:file "packages")
   (:module :src
            :depends-on ("packages")
            :components
            ((:file "globals")
             (:file "random")
             (:file "juggler" :depends-on ("globals"))))))

;;; END
