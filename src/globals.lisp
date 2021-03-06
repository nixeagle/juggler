(in-package :juggler)

(defvar *width* 640
  "Width of the image in pixels.")
(defvar *height* 360
  "Height of the image in pixels.")

(defvar *gamma* 2.2)
(defvar *second_millis* 1000)
(defvar *image_scale* 3)


;;; Virtual screen stuff.
(declaim (simple-vector *center* *look* *eye*))
(defvar *eye* #(0 0 0)
  "Location where we are looking from.

Right now we will treat this as the origin vector.")

(defvar *center* (vector 0 0 0)
  "This is supposed to be the origin.

This is the center of the virtual screen, our `*eye*' is behind this and
the image we are tracing is in front of it.")


(defvar *look* (vector 2 2 2)
  "Object we are looking at.

This is defaulting to 1 1 1 for now.")

(defvar *screen-distance* 1
  "Distance the `*eye*' is from the screen.")

(defvar *epsilon-lower-time-bound* 0.001
  "Times lower then this are considered too irrelevant to render.")

(defvar +inverse-pi+ (/ pi 1.0)
  "The inverse of pi")

(defvar *width* (/ 1920 *image_scale*)
  "Width Varaible")

(defvar *height* (/ 1080 *image_scale*)
  "Height Variable")

(defvar *sqrt-samples* 16
  "Square Root Samples Variable")

(defvar *samples* (* *sqrt-samples* *sqrt-samples*)
  "Samples Variable")



;;; END
