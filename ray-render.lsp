(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output :if-exists :supersede)

    ;; ★ PPM (P3)
    (format p "P3~%~A ~A~%255~%" (* res 100) (* res 100))

    (ensure-bvh)

    (let* ((n (* res 100))
           (inc (/ 1.0d0 res)))
      (dotimes (iy n)
        (let ((y (+ -50.0d0 (* iy inc))))
          (dotimes (ix n)
            (let ((x (+ -50.0d0 (* ix inc))))
              
              ;; ★ ここが重要
              (multiple-value-bind (r g b)
                  (color-at x y)
                (format p "~d ~d ~d~%" r g b)))))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- 0 (z eye)))
    (multiple-value-bind (r g b)
        (sendray eye xr yr zr)
      
      ;; ★ デバッグ出力
      (when (and (> x -10) (< x 10) (> y 290) (< y 310))
        (format t "~%DEBUG RGB: ~A ~A ~A~%" r g b))
      
      (values (round (* 255 (clamp01 r)))
              (round (* 255 (clamp01 g)))
              (round (* 255 (clamp01 b)))))))

(defparameter *max-depth* 3)

(defun sendray (pt xr yr zr &optional (depth 0))
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (multiple-value-bind (xn yn zn) (normal s int)

          (let* (
                 ;; =========================
                 ;; ライティング（重要）
                 ;; =========================

                 (sf (shadow-factor s int))

                 ;; 拡散光
                 (diff (* sf (lambert s int)))

                 ;; スペキュラ
                 (spec (* 1.5 sf (specular s int xr yr zr)))

                 ;; ベース光量
                 (base (+ *ambient* (* 0.7 diff) spec))

                 ;; =========================
                 ;; マテリアル色
                 ;; =========================

                 (col (ensure-rgb (surface-color s)))
                 (base-color (scale-color col base))

                 ;; 反射率
                 (refl (if (slot-exists-p s 'reflectivity)
                           (sphere-reflectivity s)
                           0.0))

                 ;; =========================
                 ;; 反射（再帰）
                 ;; =========================

                 (refc
                  (if (and (> refl 0.0) (< depth *max-depth*))
                      (let* ((eps 0.001)
                             (offset (make-point
                                      :x (+ (x int) (* xn eps))
                                      :y (+ (y int) (* yn eps))
                                      :z (+ (z int) (* zn eps))))
                             (rx 0.0) (ry 0.0) (rz 0.0))

                        (multiple-value-setq (rx ry rz)
                          (reflect-dir xr yr zr xn yn zn))

                        ;; 再帰反射
                        (multiple-value-bind (rr rg rb)
                            (sendray offset rx ry rz (1+ depth))

                          ;; 輝度化
                          (let ((lum (* 0.333 (+ rr rg rb))))
                            (list lum lum lum))))

                      '(0.0 0.0 0.0)))

                 ;; =========================
                 ;; 合成
                 ;; =========================

                 (final
                  (add-color base-color
                             (scale-color refc (* refl 1.5)))))

            ;; clampして返す
            (let ((c (clamp-color final)))
              (values (first c)
                      (second c)
                      (third c)))))

        ;; 背景
        (values 0.0d0 0.0d0 0.0d0))))

(defun first-hit (pt xr yr zr)
  (ensure-bvh)

  (multiple-value-bind (surface tmin)
      (if *bvh-root*
          ;; BVHあり
          (bvh-first-hit pt xr yr zr)

          ;; 総当たり
          (let (surface tmin)
            (dolist (s *world*)
              (let ((tt (intersect s pt xr yr zr)))
                (when tt
                  (when (or (null tmin)
                            (< tt tmin))
                    (setf surface s
                          tmin tt)))))
            (values surface tmin)))

    ;; ヒット結果
    (if surface
        (values surface
                (make-point
                 :x (+ (x pt) (* tmin xr))
                 :y (+ (y pt) (* tmin yr))
                 :z (+ (z pt) (* tmin zr))))
        (values nil nil))))

(defun first-hit-t (pt xr yr zr &optional ignore-surface)
  (let (surface tmin)
    (dolist (s *world*)
      (unless (eq s ignore-surface)
	(let ((tt (intersect s pt xr yr zr)))
	  (when tt
	    (when (or (null tmin) (< tt tmin))
	      (setf surface s tmin tt))))))
    (values surface tmin)))

(defun blocked-to-light (pt xr yr zr light-dist ignore-surface)
  (ensure-bvh)
  (if *bvh-root*
      (bvh-blocked-to-light pt xr yr zr light-dist ignore-surface)
      (dolist (s *world* nil)
	(unless (eq s ignore-surface)
	  (let ((tt (intersect s pt xr yr zr)))
	    (when (and tt
		       (> tt 0.05)
		       (< tt light-dist))
	      (return t)))))))
