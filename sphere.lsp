(load (merge-pathnames "ray.lsp" *load-truename*))

(defstruct (sphere (:include surface))
  radius center reflectivity)

(defun defsphere (x y z r c &optional (refl 0.1))
  (let ((col (if (listp c) c (list c c c)))) ;; ★追加
    (let ((s (make-sphere
              :radius r
              :center (make-point :x x :y y :z z)
              :color col
              :reflectivity refl)))
      (push s *world*)
      s)))

(defun intersect (s pt xr yr zr)
  (sphere-intersect s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s)))
    (minroot
     (+ (* xr xr) (* yr yr) (* zr zr))
     (* 2 (+ (* (- (x pt) (x c)) xr)
             (* (- (y pt) (y c)) yr)
             (* (- (z pt) (z c)) zr)))
     (- (+ (* (- (x pt) (x c)) (- (x pt) (x c)))
           (* (- (y pt) (y c)) (- (y pt) (y c)))
           (* (- (z pt) (z c)) (- (z pt) (z c))))
        (* (sphere-radius s) (sphere-radius s))))))

(defun normal (s pt)
  (sphere-normal s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x pt) (x c))
		 (- (y pt) (y c))
		 (- (z pt) (z c)))))
