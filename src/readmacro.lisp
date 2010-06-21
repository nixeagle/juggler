(in-package :juggler)

;;; Set reader macro so #V(1 2 3) works
(set-dispatch-macro-character #\# #\V
                               #'(lambda (stream char1 char2)
                                   (declare (ignore char1 char2))
                                   (assert (char= #\( (read-char stream)))
                                   (let ((vect (read-delimited-list #\) stream)))
                                     (make-array (length vect)
                                                 :element-type 'real
                                                 :initial-contents vect))))