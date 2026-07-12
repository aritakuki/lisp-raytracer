(load "ray.lsp")
(load "util.lsp")
(load "sphere.lsp")
(load "plane.lsp")
(load "ray-render.lsp")

(defparameter *camera*
      (make-camera
       :eye (make-point :x 550 :y -380 :z 650)
       :lookat (make-point :x 0 :y 160 :z -1200)
       :up (make-point :x 0 :y -1 :z 0)
       :fov-deg 28.0d0))

(update-sky-range)
(format t "CPU sky min/max: ~A ~A~%" *sky-yr-min* *sky-yr-max*)
