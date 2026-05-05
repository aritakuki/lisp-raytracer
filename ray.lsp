(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load (merge-pathnames "util.lsp" *load-truename*))
(defparameter *ambient* 0.25)
(defparameter *light* (make-point :x 600 :y 300 :z 200))
;(defparameter *light* (make-point :x 0 :y 150 :z -800))
(defparameter *shadow-mul* 0.75)

(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defparameter *vogel-cache-dxs* nil)
(defparameter *vogel-cache-dzs* nil)
(defparameter *vogel-cache-samples* nil)
(defparameter *vogel-cache-radius* nil)

(defparameter *bvh-root* nil)
(defparameter *bvh-world* nil)
(defparameter *surface-id* nil)

(load (merge-pathnames "ray-color.lsp" *load-truename*))
(load (merge-pathnames "ray-bvh.lsp" *load-truename*))
(load (merge-pathnames "ray-lighting.lsp" *load-truename*))
(load (merge-pathnames "ray-render.lsp" *load-truename*))
