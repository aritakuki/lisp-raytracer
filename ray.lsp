(load (merge-pathnames "util.lsp" *load-truename*))
(defparameter *ambient* 0.32)
(defparameter *light* (make-point :x 600 :y 300 :z 200))
(defparameter *shadow-mul* 0.75)

(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defparameter *vogel-cache* nil)
(defparameter *vogel-cache-samples* nil)
(defparameter *vogel-cache-radius* nil)

(defun vogel-offsets (radius samples)
  (when (or (null *vogel-cache*)
	    (not (eql *vogel-cache-samples* samples))
	    (not (eql *vogel-cache-radius* radius)))
    (setf *vogel-cache-samples* samples
	  *vogel-cache-radius* radius
	  *vogel-cache*
	  (let ((v (make-array samples)))
	    (dotimes (i samples)
	      (let* ((golden-angle 2.399963229728653)
		     (r (* radius (sqrt (/ (+ i 0.5) samples))))
		     (theta (* i golden-angle))
		     (dx (* r (cos theta)))
		     (dz (* r (sin theta))))
		(setf (aref v i) (cons dx dz))))
	    v)))
  *vogel-cache*)

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let* ((n (* res 100))
           (inc (/ 1.0d0 res)))
      (dotimes (iy n)
	(let ((y (+ -50.0d0 (* iy inc))))
	  (dotimes (ix n)
	    (let ((x (+ -50.0d0 (* ix inc))))
	      (print (color-at x y) p))))))))

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
          (let* ((sf (shadow-factor s int))
		 (diff (* sf
                          (lambert s int)))
                 (spec (* 0.6
			  sf
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
  (let (surface hit tmin)
    (dolist (s *world*)
      (let ((tt (intersect s pt xr yr zr)))
	(when tt
	  (when (or (null tmin) (< tt tmin))
	    (setf surface s tmin tt)))))
    (when surface
      (setf hit (make-point :x (+ (x pt) (* tmin xr))
			    :y (+ (y pt) (* tmin yr))
			    :z (+ (z pt) (* tmin zr)))))
    (values surface hit)))

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
  (dolist (s *world* nil)
    (unless (eq s ignore-surface)
      (let ((tt (intersect s pt xr yr zr)))
	(when (and tt
		   (> tt 0.05)
		   (< tt light-dist))
	  (return t))))))

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
             (light-dist2 (distance2 offset light))
             (light-dist (sqrt light-dist2)))
        (blocked-to-light offset lx ly lz light-dist s)))))

(defun shadow-factor (s int)
  (let ((samples 64)   ;; 32〜64推奨（48はバランス良い）
        (radius 45)
        (acc 0.0))
    (let ((offsets (vogel-offsets radius samples)))
      (dotimes (i samples)
	(let* ((off (aref offsets i))
	       (lp (make-point :x (+ (x *light*) (car off))
			      :y (y *light*)
			      :z (+ (z *light*) (cdr off)))))
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
