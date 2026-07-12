(defun old-spec (lc-dir-x lc-dir-y lc-dir-z nx ny nz dx dy dz)
  (let* ((dot-l-n (+ (* lc-dir-x nx) (* lc-dir-y ny) (* lc-dir-z nz)))
         (rx-l (- lc-dir-x (* 2.0 dot-l-n nx)))
         (ry-l (- lc-dir-y (* 2.0 dot-l-n ny)))
         (rz-l (- lc-dir-z (* 2.0 dot-l-n nz)))
         (dot-r-v (+ (* rx-l (- dx)) (* ry-l (- dy)) (* rz-l (- dz))))
         (vdot (max 0.0 dot-r-v)))
    (format t "OLD:   vdot=~A~%" vdot)))

(defun my-spec (lc-dir-x lc-dir-y lc-dir-z nx ny nz dx dy dz)
  (let* ((dot-l-n (+ (* (- lc-dir-x) nx) (* (- lc-dir-y) ny) (* (- lc-dir-z) nz)))
         (rx-l (- (- lc-dir-x) (* 2.0 dot-l-n nx)))
         (ry-l (- (- lc-dir-y) (* 2.0 dot-l-n ny)))
         (rz-l (- (- lc-dir-z) (* 2.0 dot-l-n nz)))
         (dot-r-v (+ (* rx-l (- dx)) (* ry-l (- dy)) (* rz-l (- dz))))
         (vdot (max 0.0 dot-r-v)))
    (format t "FIXED: vdot=~A~%" vdot)))

;; Sphere: Center (0, -300, -1200). R = 200.
;; Camera: (550, -380, 650)
;; Light: (600, 300, 200)
;; Point on front of sphere (approx): (0, -330, -1000)
;; lc-dir (from point to light): Light - Point = (600, 630, 1200) -> len = 1485
;; lc-dir = (0.40, 0.42, 0.81)
;; Normal (from center to point): Point - Center = (0, -30, 200) -> len = 202
;; Normal = (0, -0.15, 0.99)
;; dx (from camera to point): Point - Eye = (-550, 50, -1650) -> len = 1740
;; dx = (-0.31, 0.03, -0.95)

(old-spec 0.40 0.42 0.81 0.0 -0.15 0.99 -0.31 0.03 -0.95)
(my-spec 0.40 0.42 0.81 0.0 -0.15 0.99 -0.31 0.03 -0.95)
