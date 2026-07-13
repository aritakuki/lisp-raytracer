(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

;; ★ 変更：inverse multiply化
(defun unit-vector (x y z)
  (let* ((d (sqrt (+ (* x x) (* y y) (* z z))))
         (inv (/ 1.0d0 d)))
    (values (* x inv) (* y inv) (* z inv))))

(defun unit-vector+mag (x y z)
  (let* ((d (sqrt (+ (* x x) (* y y) (* z z))))
         (inv (/ 1.0d0 d)))
    (values (* x inv) (* y inv) (* z inv) d)))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun distance2 (p1 p2)
  (+ (sq (- (x p1) (x p2)))
     (sq (- (y p1) (y p2)))
     (sq (- (z p1) (z p2)))))

(defun minroot (a b c)
  (let ((disc (- (* b b) (* 4 a c))))
    (when (>= disc 0)
      (let* ((sqrt-disc (sqrt disc))
             (t1 (/ (- (- b) sqrt-disc) (* 2 a)))
             (t2 (/ (- (- b) (- sqrt-disc)) (* 2 a)))
             (eps 0.001)
             (r1 (and (> t1 eps) t1))
             (r2 (and (> t2 eps) t2)))
        (cond ((and r1 r2) (min r1 r2))
              (r1 r1)
              (r2 r2)
              (t nil))))))
