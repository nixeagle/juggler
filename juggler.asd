(asdf:defsystem :juggler
  :depends-on (:nutils :eos
                       ;; Not wholly sure we need opengl
                       ;:cl-opengl
                       )
  :components
  ((:file "juggler")))
