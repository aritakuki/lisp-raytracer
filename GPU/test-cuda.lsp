(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-cuda)
(format t "~A~%" (cl-cuda.lang.compiler:compile-ext '(defkernel test (void ()) (- dx))))
