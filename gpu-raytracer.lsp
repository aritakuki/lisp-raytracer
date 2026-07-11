;;;; gpu-raytracer.lsp
;;;; Common Lisp GPU Raytracer using cl-cuda

(declaim (optimize (speed 3) (safety 0) (debug 0)))

;; Load cl-cuda
(ql:quickload :cl-cuda)

;; Define a separate package to avoid conflicts with SB-ALIEN:INT and CL:FLOAT
(defpackage :gpu-raytracer
  (:use :cl)
  (:import-from :cl-cuda
                :defkernel
                :with-cuda
                :with-memory-blocks
                :memory-block-aref
                :sync-memory-block
                :void :float* :int* :int)
  (:export :run-gpu-raytracer))

(in-package :gpu-raytracer)

;; Define the GPU Raytracer Kernel without logical operators (AND, OR, NOT) and ABS (unsupported in cl-cuda)
;; Helper functions to generate repetitive raytracing code blocks at compile time.
;; This avoids duplicating code and exceeds maximum output token limits.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-find-first-hit (ox oy oz dx dy dz hit-t hit-type hit-idx)
    `(progn
       (set ,hit-t 1.0f10)
       (set ,hit-type 0)
       (set ,hit-idx -1)
       ;; 1. Check Spheres
       (do ((i 0 (+ i 1)))
           ((>= i num-spheres))
         (let* ((cx (aref sphere-cx i))
                (cy (aref sphere-cy i))
                (cz (aref sphere-cz i))
                (r (aref sphere-r i))
                (vx (- ,ox cx))
                (vy (- ,oy cy))
                (vz (- ,oz cz))
                (b-prime (+ (* vx ,dx) (* vy ,dy) (* vz ,dz)))
                (c-val (- (+ (* vx vx) (* vy vy) (* vz vz)) (* r r)))
                (disc (- (* b-prime b-prime) c-val)))
           (if (>= disc 0.0f0)
               (let* ((sqrt-disc (sqrt disc))
                      (t1 (- (- b-prime) sqrt-disc))
                      (t2 (+ (- b-prime) sqrt-disc))
                      (t-val (if (> t1 0.001f0)
                                 t1
                                 (if (> t2 0.001f0)
                                     t2
                                     1.0f10))))
                 (if (< t-val ,hit-t)
                     (progn
                       (set ,hit-t t-val)
                       (set ,hit-type 1)
                       (set ,hit-idx i)))))))
       ;; 2. Check Checker Plane (Normal (0, -1, 0), Point (0, 500, -1400))
       (let* ((den (- ,dy))
              (abs-den (if (> den 0.0f0) den (- den))))
         (if (> abs-den 1.0f-8)
             (let* ((hit-t-plane (/ (- ,oy 500.0f0) den)))
               (if (> hit-t-plane 0.001f0)
                   (let* ((ix-p (+ ,ox (* hit-t-plane ,dx)))
                          (iz-p (+ ,oz (* hit-t-plane ,dz)))
                          (abs-ix-p (if (> ix-p 0.0f0) ix-p (- ix-p)))
                          (iz-p-diff (- iz-p -1400.0f0))
                          (abs-iz-p-diff (if (> iz-p-diff 0.0f0) iz-p-diff (- iz-p-diff))))
                     (if (<= abs-ix-p 2500.0f0)
                         (if (<= abs-iz-p-diff 2500.0f0)
                             (if (< hit-t-plane ,hit-t)
                                 (progn
                                   (set ,hit-t hit-t-plane)
                                   (set ,hit-type 2)
                                   (set ,hit-idx -1))))))))))))

  (defun make-find-shadow-hit (ox oy oz dx dy dz max-dist shading-hit-type shading-hit-idx blocked)
    `(progn
       (set ,blocked 0)
       ;; Test sphere blockers
       (do ((k 0 (+ k 1)))
           ((>= k num-spheres))
         (if (= ,blocked 0)
             (let ((is-current-sphere 0))
               (if (= ,shading-hit-type 1)
                   (if (= ,shading-hit-idx k)
                       (set is-current-sphere 1)))
               (if (= is-current-sphere 0)
                   (let* ((cx (aref sphere-cx k))
                          (cy (aref sphere-cy k))
                          (cz (aref sphere-cz k))
                          (r (aref sphere-r k))
                          (vx (- ,ox cx))
                          (vy (- ,oy cy))
                          (vz (- ,oz cz))
                          (b-prime (+ (* vx ,dx) (* vy ,dy) (* vz ,dz)))
                          (c-val (- (+ (* vx vx) (* vy vy) (* vz vz)) (* r r)))
                          (disc (- (* b-prime b-prime) c-val)))
                     (if (>= disc 0.0f0)
                         (let* ((sqrt-disc (sqrt disc))
                                (t1 (- (- b-prime) sqrt-disc))
                                (t2 (+ (- b-prime) sqrt-disc))
                                ;; Correct bias logic: find the closest hit > 0.001f0 first, then test if > 0.05f0.
                                (t-min (if (> t1 0.001f0) t1 (if (> t2 0.001f0) t2 1.0f10)))
                                (t-val (if (> t-min 0.05f0) t-min 1.0f10)))
                           (if (< t-val ,max-dist)
                               (set ,blocked 1)))))))))
       ;; Test plane blocker
       (if (= ,blocked 0)
           (if (= ,shading-hit-type 1)
               (let* ((den-sh (- ,dy))
                      (abs-den-sh (if (> den-sh 0.0f0) den-sh (- den-sh))))
                 (if (> abs-den-sh 1.0f-8)
                     (let* ((hit-t-sh (/ (- ,oy 500.0f0) den-sh)))
                       (if (> hit-t-sh 0.05f0)
                           (if (< hit-t-sh ,max-dist)
                               (let* ((ix-sh (+ ,ox (* hit-t-sh ,dx)))
                                      (iz-sh (+ ,oz (* hit-t-sh ,dz)))
                                      (abs-ix-sh (if (> ix-sh 0.0f0) ix-sh (- ix-sh)))
                                      (iz-sh-diff (- iz-sh -1400.0f0))
                                      (abs-iz-sh-diff (if (> iz-sh-diff 0.0f0) iz-sh-diff (- iz-sh-diff))))
                                 (if (<= abs-ix-sh 2500.0f0)
                                     (if (<= abs-iz-sh-diff 2500.0f0)
                                         (set ,blocked 1)))))))))))))

  (defun make-compute-shading (ox oy oz dx dy dz hit-t hit-type hit-idx r-val g-val b-val refl-val)
    `(let* ((hit-x (+ ,ox (* ,hit-t ,dx)))
            (hit-y (+ ,oy (* ,hit-t ,dy)))
            (hit-z (+ ,oz (* ,hit-t ,dz)))
            (nx 0.0f0) (ny 0.0f0) (nz 0.0f0)
            (col-r 0.0f0) (col-g 0.0f0) (col-b 0.0f0)
            (refl-base 0.0f0))
       (if (= ,hit-type 1)
           ;; Sphere normal
           (let* ((cx (aref sphere-cx ,hit-idx))
                  (cy (aref sphere-cy ,hit-idx))
                  (cz (aref sphere-cz ,hit-idx))
                  (r (aref sphere-r ,hit-idx))
                  (inv-r (/ 1.0f0 r)))
             (set nx (* (- hit-x cx) inv-r))
             (set ny (* (- hit-y cy) inv-r))
             (set nz (* (- hit-z cz) inv-r))
             (set col-r (aref sphere-col-r ,hit-idx))
             (set col-g (aref sphere-col-g ,hit-idx))
             (set col-b (aref sphere-col-b ,hit-idx))
             (set refl-base (aref sphere-refl ,hit-idx)))
           ;; Checker plane normal
           (progn
             (set nx 0.0f0)
             (set ny -1.0f0)
             (set nz 0.0f0)
             (let* ((x-div (/ hit-x 140.0f0))
                    (z-div (/ (- hit-z -1400.0f0) 140.0f0))
                    (ix (floor x-div))
                    (iz (floor z-div))
                    (sum (+ ix iz))
                    (div2 (* sum 0.5f0))
                    (is-even (< (- div2 (floor div2)) 0.25f0)))
               (if is-even
                   (progn (set col-r 0.9f0) (set col-g 0.9f0) (set col-b 0.9f0))
                   (progn (set col-r 0.2f0) (set col-g 0.2f0) (set col-b 0.2f0))))
             (set refl-base 0.05f0)))
       
       ;; Soft Shadow (Vogel Sampling 64 points)
       (let ((shadowed-sum 0.0f0))
         (do ((s-idx-f 0.0f0 (+ s-idx-f 1.0f0)))
             ((>= s-idx-f 64.0f0))
           (let* ((golden-angle 2.399963229728653f0)
                  (r-v (* 45.0f0 (sqrt (/ (+ s-idx-f 0.5f0) 64.0f0))))
                  (theta (* s-idx-f golden-angle))
                  (lp-x (+ 600.0f0 (* r-v (cos theta))))
                  (lp-y 300.0f0)
                  (lp-z (+ 200.0f0 (* r-v (sin theta))))
                  (sh-dx (- lp-x hit-x))
                  (sh-dy (- lp-y hit-y))
                  (sh-dz (- lp-z hit-z))
                  (sh-dist (sqrt (+ (* sh-dx sh-dx) (* sh-dy sh-dy) (* sh-dz sh-dz))))
                  (inv-sh-dist (/ 1.0f0 sh-dist))
                  (sh-dir-x (* sh-dx inv-sh-dist))
                  (sh-dir-y (* sh-dy inv-sh-dist))
                  (sh-dir-z (* sh-dz inv-sh-dist))
                  (eps (* 0.0005f0 sh-dist))
                  (off-x (+ hit-x (* nx eps)))
                  (off-y (+ hit-y (* ny eps)))
                  (off-z (+ hit-z (* nz eps)))
                  (dist-offset (sqrt (+ (* (- lp-x off-x) (- lp-x off-x))
                                        (* (- lp-y off-y) (- lp-y off-y))
                                        (* (- lp-z off-z) (- lp-z off-z)))))
                  (blocked 0))
             ,(make-find-shadow-hit 'off-x 'off-y 'off-z 'sh-dir-x 'sh-dir-y 'sh-dir-z 'dist-offset hit-type hit-idx 'blocked)
             (if (= blocked 1)
                 (set shadowed-sum (+ shadowed-sum 0.75f0))
                 (set shadowed-sum (+ shadowed-sum 1.0f0)))))
         
         ;; Base lighting calculations
         (let* ((sf (/ shadowed-sum 64.0f0))
                (lc-x (- 600.0f0 hit-x))
                (lc-y (- 300.0f0 hit-y))
                (lc-z (- 200.0f0 hit-z))
                (lc-dist (sqrt (+ (* lc-x lc-x) (* lc-y lc-y) (* lc-z lc-z))))
                (inv-lc-dist (/ 1.0f0 lc-dist))
                (lc-dir-x (* lc-x inv-lc-dist))
                (lc-dir-y (* lc-y inv-lc-dist))
                (lc-dir-z (* lc-z inv-lc-dist))
                (dot-lc-n (+ (* lc-dir-x nx) (* lc-dir-y ny) (* lc-dir-z nz)))
                (lambert (if (> dot-lc-n 0.0f0) dot-lc-n 0.0f0))
                (diff (* sf lambert))
                
                ;; Specular transcription matching CPU exactly
                (lx lc-dir-x) (ly lc-dir-y) (lz lc-dir-z)
                (ref-lx (- 0.0f0 lx)) (ref-ly (- 0.0f0 ly)) (ref-lz (- 0.0f0 lz))
                (ref-dot (+ (* ref-lx nx) (* ref-ly ny) (* ref-lz nz)))
                (rx-l (- ref-lx (* 2.0f0 ref-dot nx)))
                (ry-l (- ref-ly (* 2.0f0 ref-dot ny)))
                (rz-l (- ref-lz (* 2.0f0 ref-dot nz)))
                (min-dx (- 0.0f0 ,dx)) (min-dy (- 0.0f0 ,dy)) (min-dz (- 0.0f0 ,dz))
                (vdot-val (+ (* rx-l min-dx) (* ry-l min-dy) (* rz-l min-dz)))
                (vdot (if (> vdot-val 0.0f0) vdot-val 0.0f0))
                (v2 (* vdot vdot))
                (v4 (* v2 v2))
                (spec (* 1.5f0 (* sf (* v4 v4))))
                
                (base (+ 0.25f0 (* 0.7f0 diff) spec))
                
                ;; Fresnel reflectivity calculation
                (dot-v-n (+ (* (- ,dx) nx) (* (- ,dy) ny) (* (- ,dz) nz)))
                (vdot-refl (if (> dot-v-n 0.0f0) dot-v-n 0.0f0))
                (fresnel-refl (+ refl-base (* (- 1.0f0 refl-base) (expt (- 1.0f0 vdot-refl) 5.0f0)))))
           
           (set ,r-val (* col-r base))
           (set ,g-val (* col-g base))
           (set ,b-val (* col-b base))
           (set ,refl-val fresnel-refl)))))

  (defun make-compute-sky-color (dy r-val g-val b-val)
    `(let* ((sky-t (if (< ,dy sky-yr-min)
                       0.0f0
                       (if (> ,dy sky-yr-max)
                           1.0f0
                           (/ (- ,dy sky-yr-min) (- sky-yr-max sky-yr-min)))))
            (sky-t-pow (expt sky-t 0.3f0)))
       (set ,r-val (+ (* (- 1.0f0 sky-t-pow) 1.0f0) (* sky-t-pow 0.2f0)))
       (set ,g-val (+ (* (- 1.0f0 sky-t-pow) 1.0f0) (* sky-t-pow 0.5f0)))
       (set ,b-val (+ (* (- 1.0f0 sky-t-pow) 1.0f0) (* sky-t-pow 1.0f0)))))

  (defun make-update-reflection-ray (hit-x hit-y hit-z dx dy dz nx ny nz rx ry rz ox oy oz)
    `(let* ((dot-d-n (+ (* ,dx ,nx) (* ,dy ,ny) (* ,dz ,nz)))
            (rx-dir-n (- ,dx (* 2.0f0 dot-d-n ,nx)))
            (ry-dir-n (- ,dy (* 2.0f0 dot-d-n ,ny)))
            (rz-dir-n (- ,dz (* 2.0f0 dot-d-n ,nz)))
            (norm-rn (sqrt (+ (* rx-dir-n rx-dir-n) (* ry-dir-n ry-dir-n) (* rz-dir-n rz-dir-n))))
            (inv-norm-rn (/ 1.0f0 norm-rn)))
       (set ,rx (* rx-dir-n inv-norm-rn))
       (set ,ry (* ry-dir-n inv-norm-rn))
       (set ,rz (* rz-dir-n inv-norm-rn))
         (set ,ox (+ ,hit-x (* ,nx 0.001f0)))
         (set ,oy (+ ,hit-y (* ,ny 0.001f0)))
         (set ,oz (+ ,hit-z (* ,nz 0.001f0))))))

;; GPU Raytracer Kernel definition utilizing code templates to expand exactly 3 recursion levels.
(eval
  `(defkernel raytrace-kernel-v6 (void ((out-r float*) (out-g float*) (out-b float*)
                                    (width int) (height int)
                                    (width-f cl-cuda:float) (height-f cl-cuda:float)
                                    (sphere-cx float*) (sphere-cy float*) (sphere-cz float*)
                                    (sphere-r float*)
                                    (sphere-col-r float*) (sphere-col-g float*) (sphere-col-b float*)
                                    (sphere-refl float*)
                                    (num-spheres int)
                                    (eye-x cl-cuda:float) (eye-y cl-cuda:float) (eye-z cl-cuda:float)
                                    (f-x cl-cuda:float) (f-y cl-cuda:float) (f-z cl-cuda:float)
                                    (r-x cl-cuda:float) (r-y cl-cuda:float) (r-z cl-cuda:float)
                                    (u-x cl-cuda:float) (u-y cl-cuda:float) (u-z cl-cuda:float)
                                    (scale cl-cuda:float)
                                    (sky-yr-min cl-cuda:float) (sky-yr-max cl-cuda:float)))
     (let* ((ix (+ (* cl-cuda:block-idx-x cl-cuda:block-dim-x) cl-cuda:thread-idx-x))
            (iy (+ (* cl-cuda:block-idx-y cl-cuda:block-dim-y) cl-cuda:thread-idx-y)))
       (if (< ix width)
           (if (< iy height)
               (let* ((pixel-idx (+ (* iy width) ix))
                      (inv-w (/ 1.0f0 width-f))
                      (inv-h (/ 1.0f0 height-f))
                      (sx (- (* 2.0f0 (* (+ (float ix) 0.5f0) inv-w)) 1.0f0))
                      (sy (- 1.0f0 (* 2.0f0 (* (+ (float iy) 0.5f0) inv-h))))
                      
                      ;; Camera Ray Direction
                      (rx-dir (+ f-x (* r-x sx scale) (* u-x sy scale)))
                      (ry-dir (+ f-y (* r-y sx scale) (* u-y sy scale)))
                      (rz-dir (+ f-z (* r-z sx scale) (* u-z sy scale)))
                      (dir-len (sqrt (+ (* rx-dir rx-dir) (* ry-dir ry-dir) (* rz-dir rz-dir))))
                      (inv-dir-len (/ 1.0f0 dir-len))
                      (dx (* rx-dir inv-dir-len))
                      (dy (* ry-dir inv-dir-len))
                      (dz (* rz-dir inv-dir-len))
                      
                      ;; Color Buffers
                      (accum-r 0.0f0)
                      (accum-g 0.0f0)
                      (accum-b 0.0f0))
                 
                 ;; Stage 0 (depth = 0)
                 (let ((t0 1.0f10) (type0 0) (idx0 -1))
                   ,(make-find-first-hit 'eye-x 'eye-y 'eye-z 'dx 'dy 'dz 't0 'type0 'idx0)
                   (if (= type0 0)
                       ;; hit nothing -> sky
                       ,(make-compute-sky-color 'dy 'accum-r 'accum-g 'accum-b)
                       
                       ;; hit object
                       (let* ((hit-x0 (+ eye-x (* t0 dx)))
                              (hit-y0 (+ eye-y (* t0 dy)))
                              (hit-z0 (+ eye-z (* t0 dz)))
                              (nx0 0.0f0) (ny0 0.0f0) (nz0 0.0f0)
                              (col-r0 0.0f0) (col-g0 0.0f0) (col-b0 0.0f0)
                              (refl-base0 0.0f0))
                         
                         ;; normal and material info
                         (if (= type0 1)
                             (let* ((cx (aref sphere-cx idx0))
                                    (cy (aref sphere-cy idx0))
                                    (cz (aref sphere-cz idx0))
                                    (r (aref sphere-r idx0))
                                    (inv-r (/ 1.0f0 r)))
                               (set nx0 (* (- hit-x0 cx) inv-r))
                               (set ny0 (* (- hit-y0 cy) inv-r))
                               (set nz0 (* (- hit-z0 cz) inv-r))
                               (set col-r0 (aref sphere-col-r idx0))
                               (set col-g0 (aref sphere-col-g idx0))
                               (set col-b0 (aref sphere-col-b idx0))
                               (set refl-base0 (aref sphere-refl idx0)))
                             (progn
                               (set nx0 0.0f0)
                               (set ny0 -1.0f0)
                               (set nz0 0.0f0)
                               (let* ((x-div (/ hit-x0 140.0f0))
                                      (z-div (/ (- hit-z0 -1400.0f0) 140.0f0))
                                      (ix-fl (floor x-div))
                                      (iz-fl (floor z-div))
                                      (sum (+ ix-fl iz-fl))
                                      (div2 (* sum 0.5f0))
                                      (is-even (< (- div2 (floor div2)) 0.25f0)))
                                 (if is-even
                                     (progn (set col-r0 0.9f0) (set col-g0 0.9f0) (set col-b0 0.9f0))
                                     (progn (set col-r0 0.2f0) (set col-g0 0.2f0) (set col-b0 0.2f0))))
                               (set refl-base0 0.05f0)))
                         
                         (let ((r0 0.0f0) (g0 0.0f0) (b0 0.0f0) (refl0 0.0f0))
                           ,(make-compute-shading 'eye-x 'eye-y 'eye-z 'dx 'dy 'dz 't0 'type0 'idx0 'r0 'g0 'b0 'refl0)
                           
                           (if (> refl0 0.0f0)
                               ;; Stage 1 (depth = 1)
                               (let ((ox1 0.0f0) (oy1 0.0f0) (oz1 0.0f0)
                                     (dx1 0.0f0) (dy1 0.0f0) (dz1 0.0f0))
                                 ,(make-update-reflection-ray 'hit-x0 'hit-y0 'hit-z0 'dx 'dy 'dz 'nx0 'ny0 'nz0 'dx1 'dy1 'dz1 'ox1 'oy1 'oz1)
                                 
                                 (let ((t1 1.0f10) (type1 0) (idx1 -1)
                                       (r1 0.0f0) (g1 0.0f0) (b1 0.0f0))
                                   ,(make-find-first-hit 'ox1 'oy1 'oz1 'dx1 'dy1 'dz1 't1 'type1 'idx1)
                                   
                                   (if (= type1 0)
                                       ;; sky 1
                                       ,(make-compute-sky-color 'dy1 'r1 'g1 'b1)
                                       
                                       ;; hit object 1
                                       (let* ((hit-x1 (+ ox1 (* t1 dx1)))
                                              (hit-y1 (+ oy1 (* t1 dy1)))
                                              (hit-z1 (+ oz1 (* t1 dz1)))
                                              (nx1 0.0f0) (ny1 0.0f0) (nz1 0.0f0)
                                              (col-r1 0.0f0) (col-g1 0.0f0) (col-b1 0.0f0)
                                              (refl-base1 0.0f0))
                                         (if (= type1 1)
                                             (let* ((cx (aref sphere-cx idx1))
                                                    (cy (aref sphere-cy idx1))
                                                    (cz (aref sphere-cz idx1))
                                                    (r (aref sphere-r idx1))
                                                    (inv-r (/ 1.0f0 r)))
                                               (set nx1 (* (- hit-x1 cx) inv-r))
                                               (set ny1 (* (- hit-y1 cy) inv-r))
                                               (set nz1 (* (- hit-z1 cz) inv-r))
                                               (set col-r1 (aref sphere-col-r idx1))
                                               (set col-g1 (aref sphere-col-g idx1))
                                               (set col-b1 (aref sphere-col-b idx1))
                                               (set refl-base1 (aref sphere-refl idx1)))
                                             (progn
                                               (set nx1 0.0f0)
                                               (set ny1 -1.0f0)
                                               (set nz1 0.0f0)
                                               (let* ((x-div (/ hit-x1 140.0f0))
                                                      (z-div (/ (- hit-z1 -1400.0f0) 140.0f0))
                                                      (ix-fl (floor x-div))
                                                      (iz-fl (floor z-div))
                                                      (sum (+ ix-fl iz-fl))
                                                      (div2 (* sum 0.5f0))
                                                      (is-even (< (- div2 (floor div2)) 0.25f0)))
                                                 (if is-even
                                                     (progn (set col-r1 0.9f0) (set col-g1 0.9f0) (set col-b1 0.9f0))
                                                     (progn (set col-r1 0.2f0) (set col-g1 0.2f0) (set col-b1 0.2f0))))
                                               (set refl-base1 0.05f0)))
                                         
                                         (let ((r1-base 0.0f0) (g1-base 0.0f0) (b1-base 0.0f0) (refl1 0.0f0))
                                           ,(make-compute-shading 'ox1 'oy1 'oz1 'dx1 'dy1 'dz1 't1 'type1 'idx1 'r1-base 'g1-base 'b1-base 'refl1)
                                           
                                           (if (> refl1 0.0f0)
                                               ;; Stage 2 (depth = 2)
                                               (progn
                                                 (let ((ox2 0.0f0) (oy2 0.0f0) (oz2 0.0f0)
                                                       (dx2 0.0f0) (dy2 0.0f0) (dz2 0.0f0))
                                                   ,(make-update-reflection-ray 'hit-x1 'hit-y1 'hit-z1 'dx1 'dy1 'dz1 'nx1 'ny1 'nz1 'dx2 'dy2 'dz2 'ox2 'oy2 'oz2)
                                                   
                                                   (let ((t2 1.0f10) (type2 0) (idx2 -1)
                                                         (r2 0.0f0) (g2 0.0f0) (b2 0.0f0))
                                                     ,(make-find-first-hit 'ox2 'oy2 'oz2 'dx2 'dy2 'dz2 't2 'type2 'idx2)
                                                     
                                                     (if (= type2 0)
                                                         ;; sky 2
                                                         ,(make-compute-sky-color 'dy2 'r2 'g2 'b2)
                                                         
                                                         ;; hit object 2
                                                         (let* ((hit-x2 (+ ox2 (* t2 dx2)))
                                                                (hit-y2 (+ oy2 (* t2 dy2)))
                                                                (hit-z2 (+ oz2 (* t2 dz2)))
                                                                (nx2 0.0f0) (ny2 0.0f0) (nz2 0.0f0)
                                                                (col-r2 0.0f0) (col-g2 0.0f0) (col-b2 0.0f0)
                                                                (refl-base2 0.0f0))
                                                           (if (= type2 1)
                                                               (let* ((cx (aref sphere-cx idx2))
                                                                      (cy (aref sphere-cy idx2))
                                                                      (cz (aref sphere-cz idx2))
                                                                      (r (aref sphere-r idx2))
                                                                      (inv-r (/ 1.0f0 r)))
                                                                 (set nx2 (* (- hit-x2 cx) inv-r))
                                                                 (set ny2 (* (- hit-y2 cy) inv-r))
                                                                 (set nz2 (* (- hit-z2 cz) inv-r))
                                                                 (set col-r2 (aref sphere-col-r idx2))
                                                                 (set col-g2 (aref sphere-col-g idx2))
                                                                 (set col-b2 (aref sphere-col-b idx2))
                                                                 (set refl-base2 (aref sphere-refl idx2)))
                                                               (progn
                                                                 (set nx2 0.0f0)
                                                                 (set ny2 -1.0f0)
                                                                 (set nz2 0.0f0)
                                                                 (let* ((x-div (/ hit-x2 140.0f0))
                                                                        (z-div (/ (- hit-z2 -1400.0f0) 140.0f0))
                                                                        (ix-fl (floor x-div))
                                                                        (iz-fl (floor z-div))
                                                                        (sum (+ ix-fl iz-fl))
                                                                        (div2 (* sum 0.5f0))
                                                                        (is-even (< (- div2 (floor div2)) 0.25f0)))
                                                                   (if is-even
                                                                       (progn (set col-r2 0.9f0) (set col-g2 0.9f0) (set col-b2 0.9f0))
                                                                       (progn (set col-r2 0.2f0) (set col-g2 0.2f0) (set col-b2 0.2f0))))
                                                                 (set refl-base2 0.05f0)))
                                                           
                                                           (let ((r2-base 0.0f0) (g2-base 0.0f0) (b2-base 0.0f0) (refl2 0.0f0))
                                                             ,(make-compute-shading 'ox2 'oy2 'oz2 'dx2 'dy2 'dz2 't2 'type2 'idx2 'r2-base 'g2-base 'b2-base 'refl2)
                                                             
                                                             (if (> refl2 0.0f0)
                                                                 ;; Stage 3 (depth = 3)
                                                                 (progn
                                                                   (let ((ox3 0.0f0) (oy3 0.0f0) (oz3 0.0f0)
                                                                         (dx3 0.0f0) (dy3 0.0f0) (dz3 0.0f0))
                                                                     ,(make-update-reflection-ray 'hit-x2 'hit-y2 'hit-z2 'dx2 'dy2 'dz2 'nx2 'ny2 'nz2 'dx3 'dy3 'dz3 'ox3 'oy3 'oz3)
                                                                     
                                                                     (let ((t3 1.0f10) (type3 0) (idx3 -1)
                                                                           (r3 0.0f0) (g3 0.0f0) (b3 0.0f0))
                                                                       ,(make-find-first-hit 'ox3 'oy3 'oz3 'dx3 'dy3 'dz3 't3 'type3 'idx3)
                                                                       
                                                                       (if (= type3 0)
                                                                           ;; sky 3
                                                                           ,(make-compute-sky-color 'dy3 'r3 'g3 'b3)
                                                                           
                                                                           ;; hit object 3
                                                                           (let* ((hit-x3 (+ ox3 (* t3 dx3)))
                                                                                  (hit-y3 (+ oy3 (* t3 dy3)))
                                                                                  (hit-z3 (+ oz3 (* t3 dz3)))
                                                                                  (nx3 0.0f0) (ny3 0.0f0) (nz3 0.0f0)
                                                                                  (col-r3 0.0f0) (col-g3 0.0f0) (col-b3 0.0f0))
                                                                             (if (= type3 1)
                                                                                 (let* ((cx (aref sphere-cx idx3))
                                                                                        (cy (aref sphere-cy idx3))
                                                                                        (cz (aref sphere-cz idx3))
                                                                                        (r (aref sphere-r idx3))
                                                                                        (inv-r (/ 1.0f0 r)))
                                                                                   (set nx3 (* (- hit-x3 cx) inv-r))
                                                                                   (set ny3 (* (- hit-y3 cy) inv-r))
                                                                                   (set nz3 (* (- hit-z3 cz) inv-r))
                                                                                   (set col-r3 (aref sphere-col-r idx3))
                                                                                   (set col-g3 (aref sphere-col-g idx3))
                                                                                   (set col-b3 (aref sphere-col-b idx3)))
                                                                                 (progn
                                                                                   (set nx3 0.0f0)
                                                                                   (set ny3 -1.0f0)
                                                                                   (set nz3 0.0f0)
                                                                                   (let* ((x-div (/ hit-x3 140.0f0))
                                                                                          (z-div (/ (- hit-z3 -1400.0f0) 140.0f0))
                                                                                          (ix-fl (floor x-div))
                                                                                          (iz-fl (floor z-div))
                                                                                          (sum (+ ix-fl iz-fl))
                                                                                          (div2 (* sum 0.5f0))
                                                                                          (is-even (< (- div2 (floor div2)) 0.25f0)))
                                                                                     (if is-even
                                                                                         (progn (set col-r3 0.9f0) (set col-g3 0.9f0) (set col-b3 0.9f0))
                                                                                         (progn (set col-r3 0.2f0) (set col-g3 0.2f0) (set col-b3 0.2f0))))))
                                                                             
                                                                             (let ((r3-base 0.0f0) (g3-base 0.0f0) (b3-base 0.0f0) (refl3 0.0f0))
                                                                               ,(make-compute-shading 'ox3 'oy3 'oz3 'dx3 'dy3 'dz3 't3 'type3 'idx3 'r3-base 'g3-base 'b3-base 'refl3)
                                                                               (set r3 (if (< r3-base 0.0f0) 0.0f0 (if (> r3-base 1.0f0) 1.0f0 r3-base)))
                                                                               (set g3 (if (< g3-base 0.0f0) 0.0f0 (if (> g3-base 1.0f0) 1.0f0 g3-base)))
                                                                               (set b3 (if (< b3-base 0.0f0) 0.0f0 (if (> b3-base 1.0f0) 1.0f0 b3-base))))))
                                                                       
                                                                       (let* ((lum3 (* 0.333f0 (+ (+ r3 g3) b3)))
                                                                              (final-r (+ r2-base (* refl2 lum3)))
                                                                              (final-g (+ g2-base (* refl2 lum3)))
                                                                              (final-b (+ b2-base (* refl2 lum3))))
                                                                         (set r2 (if (< final-r 0.0f0) 0.0f0 (if (> final-r 1.0f0) 1.0f0 final-r)))
                                                                         (set g2 (if (< final-g 0.0f0) 0.0f0 (if (> final-g 1.0f0) 1.0f0 final-g)))
                                                                         (set b2 (if (< final-b 0.0f0) 0.0f0 (if (> final-b 1.0f0) 1.0f0 final-b)))))))
                                                                   
                                                                   (progn
                                                                     (set r2 (if (< r2-base 0.0f0) 0.0f0 (if (> r2-base 1.0f0) 1.0f0 r2-base)))
                                                                     (set g2 (if (< g2-base 0.0f0) 0.0f0 (if (> g2-base 1.0f0) 1.0f0 g2-base)))
                                                                     (set b2 (if (< b2-base 0.0f0) 0.0f0 (if (> b2-base 1.0f0) 1.0f0 b2-base)))))))
                                                   
                                                   (let* ((lum2 (* 0.333f0 (+ (+ r2 g2) b2)))
                                                          (final-r (+ r1-base (* refl1 lum2)))
                                                          (final-g (+ g1-base (* refl1 lum2)))
                                                          (final-b (+ b1-base (* refl1 lum2))))
                                                     (set r1 (if (< final-r 0.0f0) 0.0f0 (if (> final-r 1.0f0) 1.0f0 final-r)))
                                                     (set g1 (if (< final-g 0.0f0) 0.0f0 (if (> final-g 1.0f0) 1.0f0 final-g)))
                                                     (set b1 (if (< final-b 0.0f0) 0.0f0 (if (> final-b 1.0f0) 1.0f0 final-b))))))
                                               
                                               (progn
                                                 (set r1 (if (< r1-base 0.0f0) 0.0f0 (if (> r1-base 1.0f0) 1.0f0 r1-base)))
                                                 (set g1 (if (< g1-base 0.0f0) 0.0f0 (if (> g1-base 1.0f0) 1.0f0 g1-base)))
                                                 (set b1 (if (< b1-base 0.0f0) 0.0f0 (if (> b1-base 1.0f0) 1.0f0 b1-base))))))
                                       
                                       (let* ((lum1 (* 0.333f0 (+ (+ r1 g1) b1)))
                                              (final-r (+ r0 (* refl0 lum1)))
                                              (final-g (+ g0 (* refl0 lum1)))
                                              (final-b (+ b0 (* refl0 lum1))))
                                         (set accum-r (if (< final-r 0.0f0) 0.0f0 (if (> final-r 1.0f0) 1.0f0 final-r)))
                                         (set accum-g (if (< final-g 0.0f0) 0.0f0 (if (> final-g 1.0f0) 1.0f0 final-g)))
                                         (set accum-b (if (< final-b 0.0f0) 0.0f0 (if (> final-b 1.0f0) 1.0f0 final-b)))))))
                            
                            (progn
                              (set accum-r (if (< r0 0.0f0) 0.0f0 (if (> r0 1.0f0) 1.0f0 r0)))
                              (set accum-g (if (< g0 0.0f0) 0.0f0 (if (> g0 1.0f0) 1.0f0 g0)))
                              (set accum-b (if (< b0 0.0f0) 0.0f0 (if (> b0 1.0f0) 1.0f0 b0)))))))
              
              ;; Output raw floating-point pixel colors
              (set (aref out-r pixel-idx) accum-r)
              (set (aref out-g pixel-idx) accum-g)
              (set (aref out-b pixel-idx) accum-b))))))))))))

;; Host side orchestration code
(defun run-gpu-raytracer (&key (res 8) (output-file "spheres_gpu.ppm"))
  (let* ((width (* res 100))
         (height (* res 100))
         (size (* width height))
         (width-f (float width 1.0f0))
         (height-f (float height 1.0f0))
         ;; Camera parameters
         (eye-x 550.0f0) (eye-y -380.0f0) (eye-z 650.0f0)
         (look-x 0.0f0) (look-y 160.0f0) (look-z -1200.0f0)
         (up-x 0.0f0) (up-y -1.0f0) (up-z 0.0f0)
         (fov 28.0f0)
         
         ;; Forward vector (F)
         (fx-raw (- look-x eye-x))
         (fy-raw (- look-y eye-y))
         (fz-raw (- look-z eye-z))
         (flen (sqrt (+ (* fx-raw fx-raw) (* fy-raw fy-raw) (* fz-raw fz-raw))))
         (fx (/ fx-raw flen))
         (fy (/ fy-raw flen))
         (fz (/ fz-raw flen))
         
         ;; Right vector (R = F x Up)
         (rx-raw (- (* fy up-z) (* fz up-y)))
         (ry-raw (- (* fz up-x) (* fx up-z)))
         (rz-raw (- (* fx up-y) (* fy up-x)))
         (rlen (sqrt (+ (* rx-raw rx-raw) (* ry-raw ry-raw) (* rz-raw rz-raw))))
         (rx (/ rx-raw rlen))
         (ry (/ ry-raw rlen))
         (rz (/ rz-raw rlen))
         
         ;; Up vector (U = R x F)
         (ux (- (* ry fz) (* rz fy)))
         (uy (- (* rz fx) (* rx fz)))
         (uz (- (* rx fy) (* ry fx)))
         (ulen (sqrt (+ (* ux ux) (* uy uy) (* uz uz))))
         (ux (/ ux ulen))
         (uy (/ uy ulen))
         (uz (/ uz ulen))
         
         (scale (float (tan (* 0.5f0 (/ (* fov 3.14159265f0) 180.0f0))) 1.0f0))
         
         ;; Sky range parameters (matching original sky-gradient parameters)
         (ray1-x (+ fx (* ux 1.0f0 scale)))
         (ray1-y (+ fy (* uy 1.0f0 scale)))
         (ray1-z (+ fz (* uz 1.0f0 scale)))
         (ray1-len (sqrt (+ (* ray1-x ray1-x) (* ray1-y ray1-y) (* ray1-z ray1-z))))
         (yr1 (/ ray1-y ray1-len))
         
         (ray2-x (- fx (* ux 1.0f0 scale)))
         (ray2-y (- fy (* uy 1.0f0 scale)))
         (ray2-z (- fz (* uz 1.0f0 scale)))
         (ray2-len (sqrt (+ (* ray2-x ray2-x) (* ray2-y ray2-y) (* ray2-z ray2-z))))
         (yr2 (/ ray2-y ray2-len))
         
         (sky-yr-min (float (min yr1 yr2) 1.0f0))
         (sky-yr-max (float (max yr1 yr2) 1.0f0))
         
         (sphere-data nil))
    
    ;; Restore the startup random state to match CPU SBCL random sequence
    (setf *random-state* (make-random-state cl-user::*cpu-init-random-state*))
    ;; Define Scene Objects (dynamically generating to match CPU SBCL random state)
    (setf sphere-data nil)
    ;; 3 large spheres
    (push (list 0.0f0 -300.0f0 -1200.0f0 200.0f0 0.8f0 0.2f0 0.2f0 0.02f0) sphere-data)
    (push (list -80.0f0 -150.0f0 -1200.0f0 200.0f0 0.2f0 0.8f0 0.2f0 0.2f0) sphere-data)
    (push (list 70.0f0 -100.0f0 -1200.0f0 200.0f0 0.2f0 0.2f0 0.9f0 0.2f0) sphere-data)
    ;; Small spheres (matching CPU order and random generation)
    (do ((x -2 (1+ x)))
        ((> x 2))
      (do ((z 2 (1+ z)))
          ((> z 7))
        (push (list (float (* x 200) 1.0f0)
                    300.0f0
                    (float (* z -400) 1.0f0)
                    40.0f0
                    (float (random 1.0) 1.0f0)
                    (float (random 1.0) 1.0f0)
                    (float (random 1.0) 1.0f0)
                    0.1f0)
              sphere-data)))
    
    #|
    (setf sphere-data
          (list
           (list 400.0f0 300.0f0 -2800.0f0 40.0f0 0.072882414f0 0.3737452f0 0.8174622f0 0.1f0)
           (list 400.0f0 300.0f0 -2400.0f0 40.0f0 0.5310335f0 0.10243285f0 0.59039974f0 0.1f0)
           (list 400.0f0 300.0f0 -2000.0f0 40.0f0 0.25179327f0 0.76311684f0 0.42041814f0 0.1f0)
           (list 400.0f0 300.0f0 -1600.0f0 40.0f0 0.06889212f0 0.32721102f0 0.8774886f0 0.1f0)
           (list 400.0f0 300.0f0 -1200.0f0 40.0f0 0.5274999f0 0.90044403f0 0.98117805f0 0.1f0)
           (list 400.0f0 300.0f0 -800.0f0 40.0f0 0.38965714f0 0.2503655f0 0.6341989f0 0.1f0)
           (list 200.0f0 300.0f0 -2800.0f0 40.0f0 0.9881369f0 0.64691556f0 0.64380646f0 0.1f0)
           (list 200.0f0 300.0f0 -2400.0f0 40.0f0 0.092342734f0 0.2982279f0 0.19426346f0 0.1f0)
           (list 200.0f0 300.0f0 -2000.0f0 40.0f0 0.63310087f0 0.5538459f0 0.74485755f0 0.1f0)
           (list 200.0f0 300.0f0 -1600.0f0 40.0f0 0.4120921f0 0.5945598f0 0.06366563f0 0.1f0)
           (list 200.0f0 300.0f0 -1200.0f0 40.0f0 0.34773028f0 0.34237337f0 0.60382617f0 0.1f0)
           (list 200.0f0 300.0f0 -800.0f0 40.0f0 0.784454f0 0.84417534f0 0.31095576f0 0.1f0)
           (list 0.0f0 300.0f0 -2800.0f0 40.0f0 0.4812944f0 0.48626482f0 0.94951725f0 0.1f0)
           (list 0.0f0 300.0f0 -2400.0f0 40.0f0 0.35747027f0 0.797477f0 0.51548016f0 0.1f0)
           (list 0.0f0 300.0f0 -2000.0f0 40.0f0 0.42384863f0 0.86798644f0 0.36271906f0 0.1f0)
           (list 0.0f0 300.0f0 -1600.0f0 40.0f0 0.07142329f0 0.72258794f0 0.6982585f0 0.1f0)
           (list 0.0f0 300.0f0 -1200.0f0 40.0f0 0.0073252916f0 0.31148136f0 0.59585714f0 0.1f0)
           (list 0.0f0 300.0f0 -800.0f0 40.0f0 0.5844146f0 0.7568612f0 0.9189848f0 0.1f0)
           (list -200.0f0 300.0f0 -2800.0f0 40.0f0 0.22492898f0 0.83147097f0 0.2795267f0 0.1f0)
           (list -200.0f0 300.0f0 -2400.0f0 40.0f0 0.2837726f0 0.009566903f0 0.84352255f0 0.1f0)
           (list -200.0f0 300.0f0 -2000.0f0 40.0f0 0.5962117f0 0.6005609f0 0.5940589f0 0.1f0)
           (list -200.0f0 300.0f0 -1600.0f0 40.0f0 0.9143338f0 0.21972346f0 0.9707513f0 0.1f0)
           (list -200.0f0 300.0f0 -1200.0f0 40.0f0 0.45167792f0 0.9411855f0 0.96221936f0 0.1f0)
           (list -200.0f0 300.0f0 -800.0f0 40.0f0 0.929777f0 0.93538976f0 0.31522608f0 0.1f0)
           (list -400.0f0 300.0f0 -2800.0f0 40.0f0 0.9857626f0 0.91501355f0 0.99292254f0 0.1f0)
           (list -400.0f0 300.0f0 -2400.0f0 40.0f0 0.55699635f0 0.37676394f0 0.093762994f0 0.1f0)
           (list -400.0f0 300.0f0 -2000.0f0 40.0f0 0.6163341f0 0.19508076f0 0.094441175f0 0.1f0)
           (list -400.0f0 300.0f0 -1600.0f0 40.0f0 0.8267517f0 0.44206798f0 0.2647184f0 0.1f0)
           (list -400.0f0 300.0f0 -1200.0f0 40.0f0 0.6700171f0 0.2539736f0 0.93773544f0 0.1f0)
           (list -400.0f0 300.0f0 -800.0f0 40.0f0 0.62944734f0 0.2709539f0 0.81158376f0 0.1f0)
           (list 70.0f0 -100.0f0 -1200.0f0 200.0f0 0.2f0 0.2f0 0.9f0 0.2f0)
           (list -80.0f0 -150.0f0 -1200.0f0 200.0f0 0.2f0 0.8f0 0.2f0 0.2f0)
            (list 0.0f0 -300.0f0 -1200.0f0 200.0f0 0.8f0 0.2f0 0.2f0 0.02f0)))
    |#
     
     (let* ((num-spheres (length sphere-data))
           (block-x 16)
           (block-y 16)
           (grid-x (ceiling (/ width block-x)))
           (grid-y (ceiling (/ height block-y))))
      
      (format t "Initializing CUDA Context...~%")
      (with-cuda (0)
        (format t "Allocating GPU Memory Blocks...~%")
        (with-memory-blocks ((out-r 'cl-cuda:float size)
                             (out-g 'cl-cuda:float size)
                             (out-b 'cl-cuda:float size)
                             (sph-cx 'cl-cuda:float num-spheres)
                             (sph-cy 'cl-cuda:float num-spheres)
                             (sph-cz 'cl-cuda:float num-spheres)
                             (sph-r 'cl-cuda:float num-spheres)
                             (sph-col-r 'cl-cuda:float num-spheres)
                             (sph-col-g 'cl-cuda:float num-spheres)
                             (sph-col-b 'cl-cuda:float num-spheres)
                             (sph-refl 'cl-cuda:float num-spheres))
          
          ;; Copy scene data to host side memory blocks
          (let ((idx 0))
            (dolist (s sphere-data)
              (setf (memory-block-aref sph-cx idx) (nth 0 s)
                    (memory-block-aref sph-cy idx) (nth 1 s)
                    (memory-block-aref sph-cz idx) (nth 2 s)
                    (memory-block-aref sph-r idx) (nth 3 s)
                    (memory-block-aref sph-col-r idx) (nth 4 s)
                    (memory-block-aref sph-col-g idx) (nth 5 s)
                    (memory-block-aref sph-col-b idx) (nth 6 s)
                    (memory-block-aref sph-refl idx) (nth 7 s))
              (incf idx)))
          
          ;; Sync from host memory to GPU device memory
          (sync-memory-block sph-cx :host-to-device)
          (sync-memory-block sph-cy :host-to-device)
          (sync-memory-block sph-cz :host-to-device)
          (sync-memory-block sph-r :host-to-device)
          (sync-memory-block sph-col-r :host-to-device)
          (sync-memory-block sph-col-g :host-to-device)
          (sync-memory-block sph-col-b :host-to-device)
          (sync-memory-block sph-refl :host-to-device)
          
          (dotimes (i 3)
            (format t "DEBUG Sphere ~D: Center (~F, ~F, ~F), Radius ~F, Color (~F, ~F, ~F)~%"
                    i
                    (memory-block-aref sph-cx i)
                    (memory-block-aref sph-cy i)
                    (memory-block-aref sph-cz i)
                    (memory-block-aref sph-r i)
                    (memory-block-aref sph-col-r i)
                    (memory-block-aref sph-col-g i)
                    (memory-block-aref sph-col-b i)))
          
          (format t "Launching CUDA kernel (Grid: ~Ax~A, Block: ~Ax~A)...~%" grid-x grid-y block-x block-y)
          (let ((start-time (get-internal-real-time)))
            (raytrace-kernel-v6 out-r out-g out-b
                             width height
                             width-f height-f
                             sph-cx sph-cy sph-cz
                             sph-r
                             sph-col-r sph-col-g sph-col-b
                             sph-refl
                             num-spheres
                             eye-x eye-y eye-z
                             fx fy fz
                             rx ry rz
                             ux uy uz
                             scale
                             sky-yr-min sky-yr-max
                             :grid-dim (list grid-x grid-y 1)
                             :block-dim (list block-x block-y 1))
            
            ;; Copy rendered pixels from GPU device to host memory
            (sync-memory-block out-r :device-to-host)
            (sync-memory-block out-g :device-to-host)
            (sync-memory-block out-b :device-to-host)
            
            (let* ((end-time (get-internal-real-time))
                   (elapsed (/ (float (- end-time start-time)) internal-time-units-per-second)))
              (format t "GPU Raytracing completed in ~,4F seconds.~%" elapsed)))
          
          ;; Write to PPM Image format
          (format t "Saving PPM file to ~A...~%" output-file)
          (with-open-file (p output-file :direction :output :if-exists :supersede)
            (format p "P3~%~A ~A~%255~%" width height)
            (dotimes (i size)
              (let ((r (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-r i))))))
                    (g (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-g i))))))
                    (b (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-b i)))))))
                (format p "~D ~D ~D~%" r g b))))
          (format t "Rendering Job Successful!~%"))))))

