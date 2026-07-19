;;;; gpu-package.lsp
;;;; Shared package and cl-cuda imports for every GPU module.

(ql:quickload :cl-cuda)

(defpackage :gpu-raytracer
  (:use :cl)
  (:import-from :cl-cuda
                :defkernel
                :with-cuda
                :with-memory-blocks
                :memory-block-aref
                :sync-memory-block
                ;; CUDA-language built-ins used inside DEFKERNEL bodies.
                :atomic-add
                :pointer
                :void :float* :int* :int)
  (:export :run-gpu-raytracer
           :run-gpu-animation
           :run-gpu-explainer
           :verify-kernel-structure))
