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
