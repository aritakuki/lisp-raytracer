(defun trace-reflection (s int xr yr zr depth)
  (if (>= depth *max-depth*)
      '(0 0 0)
      (multiple-value-bind (xn yn zn) (normal s int)
        (let* ((eps 0.001)
               (offset (make-point
                        :x (+ (x int) (* xn eps))
                        :y (+ (y int) (* yn eps))
                        :z (+ (z int) (* zn eps))))
               (rx 0) (ry 0) (rz 0))
          (multiple-value-setq (rx ry rz)
            (reflect-dir xr yr zr xn yn zn))
          (multiple-value-bind (rr rg rb)
              (sendray offset rx ry rz (1+ depth) s)
            (list rr rg rb))))))

(defun trace-refraction (s int xr yr zr depth)
  (let ((ior (or (ignore-errors (sphere-ior s)) 0.0d0)))
    (if (or (<= ior 1.01d0) (>= depth *max-depth*))
        '(0.0d0 0.0d0 0.0d0)

        (multiple-value-bind (xn yn zn) (normal s int)

          ;; 入射方向と法線の関係
          (let* ((dot (+ (* xr xn) (* yr yn) (* zr zn)))
                 (into (< dot 0.0d0))

                 ;; 屈折率
                 (n1 (if into 1.0d0 ior))
                 (n2 (if into ior 1.0d0))
                 (eta (/ n1 n2))

                 ;; 法線向き調整
                 (nx2 (if into xn (- xn)))
                 (ny2 (if into yn (- yn)))
                 (nz2 (if into zn (- zn))))

            ;; 屈折方向計算
            (multiple-value-bind (tx ty tz)
                (refract-dir xr yr zr nx2 ny2 nz2 eta)
		(if (null tx)
		  ;; ★ 全反射でも反射に回す（黒禁止）
		  (trace-reflection s int xr yr zr depth)
                  ;; 屈折レイ追跡
                  (let* ((eps 0.01d0)

                         ;; ★ 進行方向にオフセット（重要）
                         (offset (make-point
                                  :x (+ (x int) (* tx eps))
                                  :y (+ (y int) (* ty eps))
                                  :z (+ (z int) (* tz eps))))

                         (rr 0.0d0) (rg 0.0d0) (rb 0.0d0))

                    (multiple-value-setq (rr rg rb)
                      (sendray offset tx ty tz (1+ depth) s))

                    ;; =========================
                    ;; ★ Beer-Lambert（色吸収）
                    ;; =========================

                    (let* ((abs (or (ignore-errors (sphere-absorption s))
                                    '(0.2d0 0.05d0 0.15d0)))

                           (dist (* 2.0d0 (sphere-radius s))))

                      (list
                       (* rr (exp (- (* dist (first abs)))))
                       (* rg (exp (- (* dist (second abs)))))
                       (* rb (exp (- (* dist (third abs)))))))))))))))

(defun compose-color (s base-color refc refrc xr yr zr)
  (let* ((ior (or (ignore-errors (sphere-ior s)) 1.0d0))
         (is-glass (> ior 1.01))
         (xn 0) (yn 0) (zn 0))

    ;; 仮法線（既存踏襲）
    (multiple-value-setq (xn yn zn)
      (normal s (make-point :x 0 :y 0 :z 0)))

    (let* ((vdot (max 0.0d0
                      (+ (* (- xr) xn)
                         (* (- yr) yn)
                         (* (- zr) zn))))

           (base-refl (surface-reflectivity s))

           ;; 反射係数だけ残す（ここはOK）
           (refl (+ base-refl
                    (* (- 1.0d0 base-refl)
                       (expt (- 1.0d0 vdot) 5)))))

      (if is-glass
          ;; ★ 重要：単純合成にする
          (add-color
           base-color
           (add-color
            (scale-color refc refl)
            refrc))

          ;; 非ガラス
          (add-color base-color (scale-color refc refl))))))
