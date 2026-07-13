;;;; verify-expanded-kernel.lsp
;;;; Offline lexical-scope verification of the generated cl-cuda kernel.

(defpackage :cl-cuda
  (:use :cl)
  (:export :block-dim-x :block-dim-y :block-idx-x :block-idx-y
           :float :thread-idx-x :thread-idx-y))
(defpackage :gpu-raytracer
  (:use :cl))

(in-package :gpu-raytracer)

(defparameter *kernel-source-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

;; Capture the fully expanded kernel instead of compiling CUDA.
(defmacro defkernel (name return-type arguments &body body)
  `(quote (defkernel ,name ,return-type ,arguments ,@body)))

(defun %load-expanded-kernel ()
  (with-open-file (stream (merge-pathnames "gpu-raytracer.lsp"
                                           *kernel-source-directory*))
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          do (cond
               ;; gpu-package.lsp loads Quicklisp; the offline audit only needs
               ;; the helper-template definitions in gpu-raytracer.lsp.
               ((and (consp form) (eq (car form) 'load)) nil)
               ((and (consp form) (eq (car form) 'in-package)) nil)
               ((and (consp form) (eq (car form) 'eval))
                (return (eval form)))
               (t (eval form))))))

(defparameter *tracked-result-variables*
  '(r0 g0 b0 r1 g1 b1 r2 g2 b2 r3 g3 b3
    accum-r accum-g accum-b))

(defun %tracked-variable-p (symbol)
  (member symbol *tracked-result-variables* :test #'eq))

(defun %verify-scope (form environment)
  (cond
    ((symbolp form)
     (when (and (%tracked-variable-p form)
                (not (member form environment :test #'eq)))
       (error "Expanded GPU kernel references ~S outside its lexical scope." form)))
    ((atom form))
    ((member (car form) '(quote function) :test #'eq))
    ((member (car form) '(let let*) :test #'eq)
     (let ((bindings (cadr form))
           (body (cddr form)))
       (if (eq (car form) 'let*)
           (let ((scope environment))
             (dolist (binding bindings)
               (%verify-scope (cadr binding) scope)
               (push (car binding) scope))
             (dolist (child body)
               (%verify-scope child scope)))
           (progn
             (dolist (binding bindings)
               (%verify-scope (cadr binding) environment))
             (let ((scope (append (mapcar #'car bindings) environment)))
               (dolist (child body)
                 (%verify-scope child scope)))))))
    (t
     (dolist (child form)
       (%verify-scope child environment)))))

(defun verify-expanded-kernel ()
  (let* ((kernel (%load-expanded-kernel))
         (body (cddddr kernel)))
    (unless (and (consp kernel) (eq (car kernel) 'defkernel))
      (error "Failed to expand the GPU DEFKERNEL form."))
    (dolist (form body)
      (%verify-scope form nil))
    (format t "Expanded GPU kernel lexical-scope audit: OK.~%")
    t))

(verify-expanded-kernel)
