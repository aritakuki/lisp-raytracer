(defstruct (plane (:include surface))
  point nx ny nz half-size checker-size color1 color2 reflectivity)

(defun defplane (x y z nx ny nz half-size checker-size c1 c2 &optional (refl 0.05))
  (let ((p (make-plane
            :point (make-point :x x :y y :z z)
            :nx nx :ny ny :nz nz
            :half-size half-size
            :checker-size checker-size
            :color1 c1
            :color2 c2
            :reflectivity refl)))
    (push p *world*)
    p))

(defun plane-intersect (pl pt xr yr zr)
  (let* ((nx (plane-nx pl))
         (ny (plane-ny pl))
         (nz (plane-nz pl))
         (den (+ (* xr nx) (* yr ny) (* zr nz))))
    (unless (zerop den)
      (let* ((p0 (plane-point pl))
             (vx (- (x p0) (x pt)))
             (vy (- (y p0) (y pt)))
             (vz (- (z p0) (z pt)))
             (hit-t (/ (+ (* vx nx) (* vy ny) (* vz nz)) den)))
        (when (> hit-t 0.001)
          (let* ((ix (+ (x pt) (* hit-t xr)))
                 (iz (+ (z pt) (* hit-t zr)))
                 (hs (plane-half-size pl))
                 (cx (x p0))
                 (cz (z p0)))
            (when (and (<= (abs (- ix cx)) hs)
                       (<= (abs (- iz cz)) hs))
              hit-t)))))))

(defun plane-normal (pl pt)
  (declare (ignore pt))
  (unit-vector (plane-nx pl) (plane-ny pl) (plane-nz pl)))

(defun plane-color-at (pl pt)
  (let* ((p0 (plane-point pl))
         (sx (plane-checker-size pl))
         (ix (floor (/ (- (x pt) (x p0)) sx)))
         (iz (floor (/ (- (z pt) (z p0)) sx))))
    (if (evenp (+ ix iz))
        (plane-color1 pl)
        (plane-color2 pl))))
