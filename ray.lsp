(load (merge-pathnames "util.lsp" *load-truename*))
(defparameter *ambient* 0.32)
(defparameter *light* (make-point :x 600 :y 300 :z 200))
(defparameter *shadow-mul* 0.75)

(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
	  ((< (- 50 y) inc))
	      (do ((x -50 (+ x inc)))
		  ((< (- 50 x) inc))
		(print (color-at x y) p))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defparameter *max-depth* 3)

(defun sendray (pt xr yr zr &optional (depth 0))
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (multiple-value-bind (xn yn zn) (normal s int)
          ;; --- ベース光（ここにスペキュラを残す） ---
          (let* ((diff (* (shadow-factor s int)
                          (lambert s int)))
                 (spec (* 0.6
                          (shadow-factor s int)
                          (specular s int xr yr zr)))
                 (base (+ *ambient* (* 0.7 diff) spec))

                 ;; --- 反射 ---
                 (refl (or (and (slot-exists-p s 'reflectivity)
                                (sphere-reflectivity s))
                           0.0))

                 ;; 法線方向にオフセット（自己交差防止）
                 (eps 0.001)
                 (offset (make-point :x (+ (x int) (* xn eps))
                                     :y (+ (y int) (* yn eps))
                                     :z (+ (z int) (* zn eps))))

		 (refc
		   (if (and (> refl 0.0) (< depth *max-depth*))
		       (let ((acc 0.0) (blur (* refl 0.7)))
			 (dotimes (i 4)   ;; ← 2 → 4
			   (multiple-value-bind (rx ry rz)
			       (reflect-dir xr yr zr xn yn zn)
			     (multiple-value-bind (rx2 ry2 rz2)
				 (unit-vector
				  (perturb rx blur)
				  (perturb ry blur)
				  (perturb rz blur))
			       (incf acc
				     (sendray offset rx2 ry2 rz2 (1+ depth))))))
			 (/ acc 4))
		       0.0))
                 ;; --- 合成：加算ブレンド ---
                 ;; 反射を「足す」。ただし全体はクランプ
                 (c (min 1.0 (+ base
				(* refl refc)))))
            (* c (surface-color s))))
        0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s hit h dist d))))))
    (values surface hit)))

(defun lambert (s int)
  (multiple-value-bind (xn yn zn) (normal s int)
    (multiple-value-bind (lx ly lz)
        (unit-vector (- (x *light*) (x int))
                     (- (y *light*) (y int))
                     (- (z *light*) (z int)))
      (max 0 (+ (* lx xn) (* ly yn) (* lz zn))))))

(defun shadowed-to-light (s int light)
  (multiple-value-bind (lx ly lz)
      (unit-vector (- (x light) (x int))
                   (- (y light) (y int))
                   (- (z light) (z int)))
    (multiple-value-bind (xn yn zn) (normal s int)
      ;; EPSを距離依存に
      (let* ((eps (* 0.0005 (distance int light)))
             (offset (make-point :x (+ (x int) (* xn eps))
                                 :y (+ (y int) (* yn eps))
                                 :z (+ (z int) (* zn eps))))
             (light-dist (distance offset light)))
        (multiple-value-bind (blocking-surface hit)
            (first-hit offset lx ly lz)
          (and blocking-surface
               (not (eq blocking-surface s))
               hit
               (< (distance offset hit) light-dist)))))))

(defun shadow-factor (s int)
  (let ((samples 64)   ;; 32〜64推奨（48はバランス良い）
        (radius 45)
        (acc 0.0))
    (dotimes (i samples)
      (let ((lp (vogel-light-point *light* radius i samples)))
        (incf acc
              (if (shadowed-to-light s int lp)
                  *shadow-mul*
                  1.0))))
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
               (shininess 8))   ;; ←下げる
          (expt vdot shininess))))))

(defun reflect-dir (ix iy iz nx ny nz)
  ;; R = I - 2 (I·N) N
  (let ((dot (+ (* ix nx) (* iy ny) (* iz nz))))
    (values (- ix (* 2 dot nx))
            (- iy (* 2 dot ny))
            (- iz (* 2 dot nz)))))

(defun perturb (x scale)
  (+ x (* scale (- (random 1.0) 0.5))))
