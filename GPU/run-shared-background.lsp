;;;; Standalone entry point spawned by Monadius.

(let ((library (sb-ext:posix-getenv "MONADIUS_RAY_SHARED_LIBRARY")))
  (unless (and library (plusp (length library)))
    (error "MONADIUS_RAY_SHARED_LIBRARY is not set."))
  (sb-alien:load-shared-object library))

(sb-alien:define-alien-routine
    ("monadiusSharedArmParentDeath" %arm-parent-death)
    sb-alien:int)

;; Do this before loading Quicklisp/cl-cuda so a terminated Main cannot leave
;; an expensive, orphaned CUDA producer behind during Lisp startup.
(unless (= 1 (%arm-parent-death))
  (sb-ext:exit :code 1))

(let ((quicklisp (sb-ext:posix-getenv "QUICKLISP_SETUP")))
  (unless (and quicklisp (plusp (length quicklisp)))
    (error "QUICKLISP_SETUP is not set."))
  (load quicklisp))

(defparameter *cpu-init-random-state* (make-random-state nil))

(load (merge-pathnames "gpu-package.lsp" *load-truename*))
(load (merge-pathnames "gpu-output.lsp" *load-truename*))
(load (merge-pathnames "gpu-raytracer.lsp" *load-truename*))
(load (merge-pathnames "gpu-live-background.lsp" *load-truename*))

(handler-case
    (progn
      (gpu-raytracer:run-gpu-live-background-process)
      (sb-ext:exit :code 0))
  (error (condition)
    (format *error-output* "Standalone live background terminated: ~A~%"
            condition)
    (finish-output *error-output*)
    (sb-ext:exit :code 1)))
