(load (merge-pathnames "sphere.lsp" *load-truename*))

(defun ray-test (&optional (res 1))
  (setf *world* nil)

(setf *camera*
      (make-camera
       :eye (make-point :x 550 :y -380 :z 650)
       :lookat (make-point :x 0 :y 160 :z -1200)
       :up (make-point :x 0 :y -1 :z 0)
       :fov-deg 28.0d0))

  (defplane 0 500 -1400
            0.0 -1.0 0.0
            2500
            140
            '(0.9 0.9 0.9)
            '(0.2 0.2 0.2)
            0.05)

  ;; 大きい球
  (defsphere 0 -300 -1200 200 '(0.8 0.2 0.2) 0.02)
  (defsphere -80 -150 -1200 200 '(0.2 0.8 0.2) 0.2)
  (defsphere 70 -100 -1200 200 '(0.2 0.2 0.9) 0.2)

  ;; 小さい球（ランダムカラー）
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40
                 (list (random 1.0) (random 1.0) (random 1.0)))))

  (tracer (make-pathname :name "spheres" :type "ppm") res))
