;;;; Build the callable SBCL core loaded by Monadius in Google Colab.

(defparameter *cpu-init-random-state* (make-random-state nil))

(load (merge-pathnames "gpu-package.lsp" *load-truename*))
(load (merge-pathnames "gpu-output.lsp" *load-truename*))
(load (merge-pathnames "gpu-raytracer.lsp" *load-truename*))
(load (merge-pathnames "gpu-live-background.lsp" *load-truename*))

(let ((core-file (or (sb-ext:posix-getenv "MONADIUS_RAY_CORE")
                     "monadius-ray-background.core")))
  (format t "~&Saving Monadius live background core to ~A.~%" core-file)
  (sb-ext:save-lisp-and-die
   core-file
   :callable-exports
   '(("monadius_lisp_ray_background_run"
      gpu-raytracer::lisp-ray-background-run))
   :compression nil))
