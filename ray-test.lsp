(load (merge-pathnames "sphere.lsp" *load-truename*))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8 0.02)   ;; 下：反射弱く（安定）
  (defsphere -80 -150 -1200 200 .7 0.2)
  (defsphere 70 -100 -1200 200 .9 0.2)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))
