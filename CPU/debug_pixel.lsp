(load "sphere.lsp")

(defun ray-test-debug ()
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

  (ensure-bvh)
  (update-sky-range)

  ;; Debug pixel (50,50) at res=1 (100x100)
  (let* ((n 100)
         (invn (/ 1.0d0 n))
         (iy 50)
         (ix 50)
         (sy (- 1.0d0 (* 2.0d0 (* (+ iy 0.5d0) invn))))
         (sx (- (* 2.0d0 (* (+ ix 0.5d0) invn)) 1.0d0)))
    (format t "sx=~A sy=~A~%" sx sy)
    (multiple-value-bind (xr yr zr) (camera-ray sx sy)
      (format t "ray dir=~A ~A ~A~%" xr yr zr)
      (multiple-value-bind (s int) (first-hit (camera-eye *camera*) xr yr zr)
        (if s
            (progn
              (format t "Hit surface: ~A~%" (type-of s))
              (format t "Hit point: ~A ~A ~A~%" (x int) (y int) (z int))
              (multiple-value-bind (xn yn zn) (normal s int)
                (format t "Normal: ~A ~A ~A~%" xn yn zn))
              (let* ((sf (shadow-factor s int))
                     (lam (lambert s int))
                     (spec (specular s int xr yr zr))
                     (diff (* sf lam))
                     (spec-val (* 1.5 sf spec))
                     (base (+ *ambient* (* 0.7 diff) spec-val))
                     (col (surface-color-at s int))
                     (refl (surface-reflectivity s)))
                (format t "sf=~A lambert=~A specular=~A~%" sf lam spec)
                (format t "diff=~A spec_val=~A~%" diff spec-val)
                (format t "base=~A~%" base)
                (format t "color=~A reflectivity=~A~%" col refl)
                (format t "base*col = ~A ~A ~A~%"
                        (* (first col) base)
                        (* (second col) base)
                        (* (third col) base))))
            (format t "No hit (sky)~%"))))))

(ray-test-debug)
