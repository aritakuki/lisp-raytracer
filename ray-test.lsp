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

  ;; 大きい球（金属と透明の中間的な反射）
  (defsphere 0 -300 -1200 200 '(0.8 0.2 0.2) 0.3 1.0)   ;赤
  (defsphere -80 -150 -1200 200 '(0.2 0.8 0.2) 0.4 1.1)  ;緑
  (defsphere 70 -100 -1200 200 '(0.2 0.2 0.9) 0.5 1.0)  ;★ 青に変更

  ;; 小さい球（ランダムカラー）
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      ;; ★ いくつかをガラス球に
      (let* ((is-glass (> (random 1.0) 0.7))  ;70%は通常、30%がガラス
             (ior (if is-glass 1.5 1.0))
             (col (list (random 1.0) (random 1.0) (random 1.0))))
        (defsphere (* x 200) 300 (* z -400) 40 col 0.1 ior))))

  (tracer (make-pathname :name "spheres" :type "ppm") res))
