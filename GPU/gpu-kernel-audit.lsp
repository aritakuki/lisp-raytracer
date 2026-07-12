;;;; gpu-kernel-audit.lsp
;;;; Structural checks for the generated CUDA kernel.

(in-package :gpu-raytracer)

(defparameter *gpu-kernel-audit-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(defun %symbol-named-p (object name)
  (and (symbolp object) (string= (symbol-name object) name)))

(defun %kernel-template ()
  "Read the DEFKERNEL template without evaluating it."
  (with-open-file (stream (merge-pathnames "gpu-raytracer.lsp"
                                           *gpu-kernel-audit-directory*))
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          when (and (consp form) (%symbol-named-p (car form) "EVAL"))
            do (let ((quoted-form (cadr form)))
                 (when (and (consp quoted-form)
                            (consp (cdr quoted-form)))
                   (return (cadr quoted-form)))))))

(defun %screen-bound-test-p (test axis limit)
  (and (consp test)
       (%symbol-named-p (car test) "<")
       (%symbol-named-p (cadr test) axis)
       (%symbol-named-p (caddr test) limit)))

(defun %output-set-p (form)
  (and (consp form)
       (%symbol-named-p (car form) "SET")
       (consp (cadr form))
       (%symbol-named-p (caadr form) "AREF")
       (member (symbol-name (cadadr form)) '("OUT-R" "OUT-G" "OUT-B")
               :test #'string=)))

(defun verify-kernel-structure ()
  "Reject malformed conditionals and conditional pixel writes before CUDA compilation."
  (let ((template (%kernel-template))
        (invalid-ifs 0)
        (output-writes 0)
        (bad-output-scopes 0))
    (unless template
      (error "Could not find the GPU DEFKERNEL template."))
    (labels ((walk (form if-tests)
               (when (consp form)
                 (let ((next-if-tests
                         (if (%symbol-named-p (car form) "IF")
                             (progn
                               (unless (member (length form) '(3 4))
                                 (incf invalid-ifs))
                               (cons (cadr form) if-tests))
                             if-tests)))
                   (when (%output-set-p form)
                     (incf output-writes)
                     (let ((tests (reverse if-tests)))
                       (unless (and (= (length tests) 2)
                                    (%screen-bound-test-p (first tests) "IX" "WIDTH")
                                    (%screen-bound-test-p (second tests) "IY" "HEIGHT"))
                         (incf bad-output-scopes))))
                   (dolist (child form)
                     (walk child next-if-tests))))))
      (walk template nil))
    (unless (zerop invalid-ifs)
      (error "GPU kernel contains ~D malformed IF form(s)." invalid-ifs))
    (unless (= output-writes 3)
      (error "GPU kernel must write OUT-R, OUT-G, and OUT-B exactly once (found ~D)."
             output-writes))
    (unless (zerop bad-output-scopes)
      (error "GPU pixel output is nested inside a ray-tracing branch."))
    (format t "GPU kernel structural audit: OK.~%")
    t))
