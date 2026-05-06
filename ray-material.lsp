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
  (let ((ior (or (ignore-errors (sphere-ior s)) 0.0)))
    (if (or (<= ior 1.01) (>= depth *max-depth*))
        '(0 0 0)
        (multiple-value-bind (xn yn zn) (normal s int)
          (let* ((into (< (+ (* xr xn) (* yr yn) (* zr zn)) 0.0d0))
                 (n1 (if into 1.0d0 ior))
                 (n2 (if into ior 1.0d0))
                 (eta (/ n1 n2))
                 (nx2 (if into xn (- xn)))
                 (ny2 (if into yn (- yn)))
                 (nz2 (if into zn (- zn))))
            (multiple-value-bind (tx ty tz)
                (refract-dir xr yr zr nx2 ny2 nz2 eta)
              (if tx
                  (let* ((eps 0.01)
                         (offset (make-point
                                  :x (+ (x int) (* tx eps))
                                  :y (+ (y int) (* ty eps))
                                  :z (+ (z int) (* tz eps)))))
                    (multiple-value-bind (rr rg rb)
                        (sendray offset tx ty tz (1+ depth) s)
                      (list rr rg rb)))
                  '(0 0 0))))))))

(defun compose-color (s base-color refc refrc xr yr zr)
  (let* ((ior (or (ignore-errors (sphere-ior s)) 0.0))
         (is-glass (> ior 1.01))
         (xn 0) (yn 0) (zn 0))

    (multiple-value-setq (xn yn zn) (normal s (make-point :x 0 :y 0 :z 0))) ;; 仮でOK

    (let* ((vdot (max 0.0d0 (+ (* (- xr) xn)
                               (* (- yr) yn)
                               (* (- zr) zn))))
           (base-refl (surface-reflectivity s))
           (refl (+ base-refl
                    (* (- 1.0d0 base-refl)
                       (expt (- 1.0d0 vdot) 5)))))

      (if is-glass
          (let ((trans 0.3d0))
            (add-color
             (scale-color base-color (- 1.0d0 trans))
             (scale-color
              (add-color
               (scale-color refc refl)
               (scale-color refrc (- 1.0d0 refl)))
              trans)))
          (add-color base-color (scale-color refc refl))))))
