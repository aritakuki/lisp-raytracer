;;;; gpu-main.lsp
;;;; Stable entry point for the GPU implementation.

(load (merge-pathnames "gpu-package.lsp" *load-truename*))
(load (merge-pathnames "gpu-output.lsp" *load-truename*))
(load (merge-pathnames "gpu-raytracer.lsp" *load-truename*))
(load (merge-pathnames "gpu-live-background.lsp" *load-truename*))
(load (merge-pathnames "gpu-explainer.lsp" *load-truename*))
(load (merge-pathnames "gpu-kernel-audit.lsp" *load-truename*))

(gpu-raytracer:verify-kernel-structure)
