(in-package :juggler)

(defvar *width* 640
  "Width of the image in pixels.")
(defvar *height* 360
  "Height of the image in pixels.")

(defvar *gamma* 2.2)
(defvar *second_millis* 1000)
(defvar *image_scale* 3)


(defvar *eye* #(0 0 0)
  "Location where we are looking from.

Right now we will treat this as the origin vector.")

(declaim ((vector (member 0)) *center*))
(defvar *center* #(0 0 0)
  "This is supposed to be the origin.")