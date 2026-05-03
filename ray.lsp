(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load (merge-pathnames "util.lsp" *load-truename*))
(defparameter *ambient* 0.25)
(defparameter *light* (make-point :x 600 :y 300 :z 200))
;(defparameter *light* (make-point :x 0 :y 150 :z -800))
(defparameter *shadow-mul* 0.75)

(defstruct surface color)

(defparameter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defparameter *vogel-cache-dxs* nil)
(defparameter *vogel-cache-dzs* nil)
(defparameter *vogel-cache-samples* nil)
(defparameter *vogel-cache-radius* nil)

(defparameter *bvh-root* nil)
(defparameter *bvh-world* nil)
(defparameter *surface-id* nil)

(defstruct bvh-node
  minx miny minz
  maxx maxy maxz
  left right
  objects)

(defun scale-color (c s)
  (list (* (first c) s)
        (* (second c) s)
        (* (third c) s)))

(defun add-color (a b)
  (list (+ (first a) (first b))
        (+ (second a) (second b))
        (+ (third a) (third b))))

(defun clamp01 (x)
  (cond ((< x 0.0d0) 0.0d0)
        ((> x 1.0d0) 1.0d0)
        (t x)))

(defun clamp-color (c)
  (list (clamp01 (first c))
        (clamp01 (second c))
        (clamp01 (third c))))

(defun sphere-bbox (s)
  (let* ((c (sphere-center s))
	 (r (sphere-radius s)))
    (values (- (x c) r) (- (y c) r) (- (z c) r)
	    (+ (x c) r) (+ (y c) r) (+ (z c) r))))

(defun surface-bbox (s)
  (typecase s
    (sphere (sphere-bbox s))))

(defun bvh-merge-bbox (ax ay az bx by bz cx cy cz dx dy dz)
  (values (min ax bx cx dx)
	  (min ay by cy dy)
	  (min az bz cz dz)
	  (max ax bx cx dx)
	  (max ay by cy dy)
	  (max az bz cz dz)))

(defun bvh-bounds-of-objects (objects)
  (let (minx miny minz maxx maxy maxz)
    (dolist (s objects)
      (multiple-value-bind (a b c d e f) (surface-bbox s)
	(if (null minx)
	    (setf minx a miny b minz c maxx d maxy e maxz f)
	    (setf minx (min minx a)
		  miny (min miny b)
		  minz (min minz c)
		  maxx (max maxx d)
		  maxy (max maxy e)
		  maxz (max maxz f)))))
    (values minx miny minz maxx maxy maxz)))

(defun bvh-surface-center-axis (s axis)
  (let ((c (sphere-center s)))
    (ecase axis
      (0 (x c))
      (1 (y c))
      (2 (z c)))))

(defun build-bvh (objects)
  (multiple-value-bind (minx miny minz maxx maxy maxz)
      (bvh-bounds-of-objects objects)
    (let* ((count (length objects))
	   (ex (- maxx minx))
	   (ey (- maxy miny))
	   (ez (- maxz minz))
	   (axis (cond ((and (>= ex ey) (>= ex ez)) 0)
		       ((>= ey ez) 1)
		       (t 2))))
      (if (<= count 4)
	  (make-bvh-node :minx minx :miny miny :minz minz
			 :maxx maxx :maxy maxy :maxz maxz
			 :objects objects)
	  (let* ((sorted (stable-sort (copy-list objects) #'<
				     :key (lambda (s) (bvh-surface-center-axis s axis))))
		 (mid (ash count -1))
		 (left-objects (subseq sorted 0 mid))
		 (right-objects (subseq sorted mid))
		 (left (build-bvh left-objects))
		 (right (build-bvh right-objects)))
	    (make-bvh-node :minx minx :miny miny :minz minz
			   :maxx maxx :maxy maxy :maxz maxz
			   :left left :right right))))))

(defun rebuild-bvh ()
  (setf *bvh-world* *world*
	*bvh-root* (and *world* (build-bvh *world*))
	*surface-id* (make-hash-table :test #'eq))
  (let ((i 0))
    (dolist (s *world*)
      (setf (gethash s *surface-id*) i)
      (incf i)))
  *bvh-root*)

(defun ensure-bvh ()
  (when (not (eq *bvh-world* *world*))
    (rebuild-bvh))
  *bvh-root*)

(defun ray-aabb-range (pt xr yr zr node)
  (labels ((slab (p d min max)
	     (if (zerop d)
		 (if (and (>= p min) (<= p max))
		     (values most-negative-double-float most-positive-double-float)
		     (values nil nil))
		 (let* ((inv (/ 1.0d0 d))
			(t1 (* (- min p) inv))
			(t2 (* (- max p) inv)))
		   (if (< t1 t2)
		       (values t1 t2)
		       (values t2 t1))))))
    (multiple-value-bind (tx1 tx2) (slab (x pt) xr (bvh-node-minx node) (bvh-node-maxx node))
      (when tx1
      (multiple-value-bind (ty1 ty2) (slab (y pt) yr (bvh-node-miny node) (bvh-node-maxy node))
        (when ty1
        (multiple-value-bind (tz1 tz2) (slab (z pt) zr (bvh-node-minz node) (bvh-node-maxz node))
          (when tz1
            (let ((tmin (max tx1 ty1 tz1))
			  (tmax (min tx2 ty2 tz2)))
		      (when (<= tmin tmax)
			(values tmin tmax)))))))))))

(defun bvh-first-hit (pt xr yr zr)
  (let ((best-s nil)
	(best-t nil)
	(best-id nil)
	(stack (make-array 64 :adjustable t :fill-pointer 0)))
    (when *bvh-root*
      (vector-push-extend *bvh-root* stack))
    (loop while (> (fill-pointer stack) 0) do
      (let ((node (vector-pop stack)))
        (multiple-value-bind (tmin tmax) (ray-aabb-range pt xr yr zr node)
          (declare (ignore tmax))
          (when (and tmin (or (null best-t) (< tmin best-t)))
            (let ((objs (bvh-node-objects node)))
              (if objs
                  (dolist (s objs)
                    (let ((tt (intersect s pt xr yr zr)))
                      (when tt
                        (let ((sid (gethash s *surface-id*)))
                          (when (or (null best-t)
                                    (< tt best-t)
                                    (and best-id (= tt best-t) (< sid best-id)))
                            (setf best-s s
                                  best-t tt
                                  best-id sid))))))
                  (let ((l (bvh-node-left node))
                        (r (bvh-node-right node)))
                    (cond
                      ((and l r)
                       (multiple-value-bind (ltmin ltmax) (ray-aabb-range pt xr yr zr l)
                         (declare (ignore ltmax))
                         (multiple-value-bind (rtmin rtmax) (ray-aabb-range pt xr yr zr r)
                           (declare (ignore rtmax))
                           (cond
                             ((and ltmin rtmin)
                              (if (< ltmin rtmin)
                                  (progn (vector-push-extend r stack) (vector-push-extend l stack))
                                  (progn (vector-push-extend l stack) (vector-push-extend r stack))))
                             (ltmin (vector-push-extend l stack))
                             (rtmin (vector-push-extend r stack))))))
                      (l (vector-push-extend l stack))
                      (r (vector-push-extend r stack))))))))))
    (values best-s best-t)))

(defun bvh-blocked-to-light (pt xr yr zr light-dist ignore-surface)
  (let ((stack (make-array 64 :adjustable t :fill-pointer 0)))
    (when *bvh-root*
      (vector-push-extend *bvh-root* stack))
    (loop
	while (> (fill-pointer stack) 0)
	for node = (vector-pop stack)
	do
	  (multiple-value-bind (tmin tmax) (ray-aabb-range pt xr yr zr node)
	    (when (and tmin (< tmin light-dist) (> tmax 0.05))
	      (let ((objs (bvh-node-objects node)))
		(if objs
		    (dolist (s objs)
		      (unless (eq s ignore-surface)
			(let ((tt (intersect s pt xr yr zr)))
			  (when (and tt (> tt 0.05) (< tt light-dist))
			    (return-from bvh-blocked-to-light t)))))
		    (progn
		      (when (bvh-node-left node) (vector-push-extend (bvh-node-left node) stack))
		      (when (bvh-node-right node) (vector-push-extend (bvh-node-right node) stack))))))))
    nil))

(defun vogel-offsets (radius samples)
  (when (or (null *vogel-cache-dxs*)
	    (null *vogel-cache-dzs*)
	    (not (eql *vogel-cache-samples* samples))
	    (not (eql *vogel-cache-radius* radius)))
    (setf *vogel-cache-samples* samples
	  *vogel-cache-radius* radius
	  *vogel-cache-dxs* (make-array samples)
	  *vogel-cache-dzs* (make-array samples))
    (dotimes (i samples)
      (let* ((golden-angle 2.399963229728653)
	     (r (* radius (sqrt (/ (+ i 0.5) samples))))
	     (theta (* i golden-angle))
	     (dx (* r (cos theta)))
	     (dz (* r (sin theta))))
	(setf (aref *vogel-cache-dxs* i) dx
	      (aref *vogel-cache-dzs* i) dz))))
  (values *vogel-cache-dxs* *vogel-cache-dzs*))

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
