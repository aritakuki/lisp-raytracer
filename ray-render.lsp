(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output :if-exists :supersede)

    ;; ★ PPM (P3)
    (format p "P3~%~A ~A~%255~%" (* res 100) (* res 100))

    (ensure-bvh)

    (let* ((n (* res 100))
           (invn (/ 1.0d0 n)))
      (dotimes (iy n)
        (let ((sy (- 1.0d0 (* 2.0d0 (* (+ iy 0.5d0) invn)))))
          (dotimes (ix n)
            (let ((sx (- (* 2.0d0 (* (+ ix 0.5d0) invn)) 1.0d0)))
              (multiple-value-bind (r g b)
                  (color-at sx sy)
                (format p "~d ~d ~d~%" r g b)))))))))

(defun v- (ax ay az bx by bz)
  (values (- ax bx) (- ay by) (- az bz)))

(defun v+ (ax ay az bx by bz)
  (values (+ ax bx) (+ ay by) (+ az bz)))

(defun v* (ax ay az s)
  (values (* ax s) (* ay s) (* az s)))

(defun dot3 (ax ay az bx by bz)
  (+ (* ax bx) (* ay by) (* az bz)))

(defun cross3 (ax ay az bx by bz)
  (values (- (* ay bz) (* az by))
          (- (* az bx) (* ax bz))
          (- (* ax by) (* ay bx))))

(defun camera-ray (x y)
  (let* ((cam *camera*)
         (ceye (camera-eye cam))
         (clook (camera-lookat cam))
         (cup (camera-up cam))
         (fov (camera-fov-deg cam)))
    (multiple-value-bind (fx fy fz)
        (unit-vector (- (x clook) (x ceye))
                     (- (y clook) (y ceye))
                     (- (z clook) (z ceye)))
      (multiple-value-bind (rx ry rz)
          (cross3 fx fy fz (x cup) (y cup) (z cup))
        (multiple-value-bind (rx ry rz)
            (unit-vector rx ry rz)
          (multiple-value-bind (ux uy uz)
              (cross3 rx ry rz fx fy fz)
            (let* ((scale (tan (* 0.5d0 (/ (* fov pi) 180.0d0))))
                   (sx (* x scale))
                   (sy (* y scale)))
              (multiple-value-bind (rxx rxy rxz)
                  (v* rx ry rz sx)
                (multiple-value-bind (uxx uxy uxz)
                    (v* ux uy uz sy)
                  (multiple-value-bind (tx ty tz)
                      (v+ rxx rxy rxz uxx uxy uxz)
                    (multiple-value-bind (vx vy vz)
                        (v+ fx fy fz tx ty tz)
                      (unit-vector vx vy vz))))))))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (camera-ray x y)
    (multiple-value-bind (r g b)
        (sendray (camera-eye *camera*) xr yr zr)
      
      ;; ★ デバッグ出力
      (when (and (> x -10) (< x 10) (> y 290) (< y 310))
        (format t "~%DEBUG RGB: ~A ~A ~A~%" r g b))
      
      (values (round (* 255 (clamp01 r)))
              (round (* 255 (clamp01 g)))
              (round (* 255 (clamp01 b)))))))

(defun surface-color-at (s int)
  (typecase s
    (plane (plane-color-at s int))
    (t (surface-color s))))

(defun surface-reflectivity (s)
  (typecase s
    (sphere (sphere-reflectivity s))
    (plane (plane-reflectivity s))
    (t 0.0)))

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

                 (col (ensure-rgb (surface-color-at s int)))
                 (base-color (scale-color col base))

                 ;; 反射率
                 (refl (surface-reflectivity s))

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
