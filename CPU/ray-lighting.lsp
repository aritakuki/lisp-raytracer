(defun shade-local (s int xr yr zr)
  (let* ((sf (shadow-factor s int))
         (diff (* sf (lambert s int)))
         (spec (* 1.5 sf (specular s int xr yr zr)))
         (base (+ *ambient* (* 0.7 diff) spec))
         (col (ensure-rgb (surface-color-at s int))))
    (scale-color col base)))

(defun refract-dir (ix iy iz nx ny nz eta)
  ;; I, N は正規化済み前提

  (let* ((dot (+ (* ix nx) (* iy ny) (* iz nz)))
         (cosi (- dot))
         (sin2t (* eta eta (- 1.0d0 (* cosi cosi)))))

    (if (> sin2t 1.0d0)
        ;; 全反射
        (values nil nil nil)

        ;; 屈折方向（★正規化する）
        (let* ((cost (sqrt (- 1.0d0 sin2t)))
               (a eta)
               (b (- (* eta cosi) cost))
               (tx (+ (* a ix) (* b nx)))
               (ty (+ (* a iy) (* b ny)))
               (tz (+ (* a iz) (* b nz))))
          (unit-vector tx ty tz)))))

(defun lambert (s int)
  (multiple-value-bind (xn yn zn) (normal s int)
    (multiple-value-bind (lx ly lz)
        (unit-vector (- (x *light*) (x int))
                     (- (y *light*) (y int))
                     (- (z *light*) (z int)))
      (max 0 (+ (* lx xn) (* ly yn) (* lz zn))))))

(defun shadowed-to-light (s int light)
  (multiple-value-bind (lx ly lz dist)
      (unit-vector+mag (- (x light) (x int))
                       (- (y light) (y int))
                       (- (z light) (z int)))
    (multiple-value-bind (xn yn zn) (normal s int)
      ;; EPSを距離依存に
      (let* ((eps (* 0.0005 dist))
             (offset (make-point :x (+ (x int) (* xn eps))
                                 :y (+ (y int) (* yn eps))
                                 :z (+ (z int) (* zn eps))))
             (light-dist2 (distance2 offset light))
             (light-dist (sqrt light-dist2)))
        (blocked-to-light offset lx ly lz light-dist s)))))

(defun shadow-factor (s int)
  (let ((samples 64)   ;; 32〜64推奨（48はバランス良い）
        (radius 45)
        (acc 0.0))
    (multiple-value-bind (dxs dzs) (vogel-offsets radius samples)
      (dotimes (i samples)
	(let* ((lp (make-point :x (+ (x *light*) (aref dxs i))
			      :y (y *light*)
			      :z (+ (z *light*) (aref dzs i)))))
	  (incf acc
		(if (shadowed-to-light s int lp)
		    *shadow-mul*
		    1.0)))))
    (/ acc samples)))

(defun random-light-point (center radius)
  ;; XZ平面の円盤サンプリング
  (let* ((theta (* 2 pi (random 1.0)))
         (r (* radius (sqrt (random 1.0))))  ;; √で均一分布
         (dx (* r (cos theta)))
         (dz (* r (sin theta))))
    (make-point :x (+ (x center) dx)
                :y (y center)
                :z (+ (z center) dz))))

(defun vogel-light-point (center radius i n)
  ;; Vogel disk sampling（黄金角）
  (let* ((golden-angle 2.399963229728653) ;; ≈ π(3 - √5)
         (r (* radius (sqrt (/ (+ i 0.5) n))))
         (theta (* i golden-angle))
         (dx (* r (cos theta)))
         (dz (* r (sin theta))))
    (make-point :x (+ (x center) dx)
                :y (y center)
                :z (+ (z center) dz))))

(defun reflect (lx ly lz nx ny nz)
  ;; R = L - 2(L・N)N
  (let ((dot (+ (* lx nx) (* ly ny) (* lz nz))))
    (values (- lx (* 2 dot nx))
            (- ly (* 2 dot ny))
            (- lz (* 2 dot nz)))))

(defun specular (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (multiple-value-bind (lx ly lz)
        (unit-vector (- (x *light*) (x int))
                     (- (y *light*) (y int))
                     (- (z *light*) (z int)))
      (multiple-value-bind (rx ry rz)
          (reflect (- lx) (- ly) (- lz) xn yn zn)
        (let* ((vdot (max 0 (+ (* rx (- xr))
                               (* ry (- yr))
                               (* rz (- zr)))))
               ;; ★ 変更：expt → 手展開（^8）
               (x (* vdot vdot))     ;; v^2
               (x2 (* x x))          ;; v^4
               (x4 (* x2 x2)))       ;; v^8
          x4)))))

(defun reflect-dir (ix iy iz nx ny nz)
  ;; R = I - 2 (I·N) N
  (let ((dot (+ (* ix nx) (* iy ny) (* iz nz))))
    (values (- ix (* 2 dot nx))
            (- iy (* 2 dot ny))
            (- iz (* 2 dot nz)))))

(defun perturb (x scale)
  (+ x (* scale (- (random 1.0) 0.5))))

(defun ensure-rgb (c)
  (cond
    ;; すでにRGBリスト
    ((and (listp c) (= (length c) 3))
     c)

    ;; スカラー → グレー化
    ((numberp c)
     (list c c c))

    ;; それ以外（保険）
    (t
     (list 1.0 0.0 1.0))))  ;; マゼンタ（異常検知用）
