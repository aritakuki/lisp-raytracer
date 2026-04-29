(load (merge-pathnames "ray.lsp" *load-truename*))

(defstruct (sphere (:include surface))
  radius center reflectivity)

(defun defsphere (x y z r c &optional (refl 0.1))
  (let ((s (make-sphere
            :radius r
            :center (make-point :x x :y y :z z)
            :color c
            :reflectivity refl)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
	   s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	(make-point :x (+ (x pt) (* n xr))
		    :y (+ (y pt) (* n yr))
		    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x pt) (x c))
		 (- (y pt) (y c))
		 (- (z pt) (z c)))))
