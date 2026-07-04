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
(defkernel raytrace-kernel-v6 (void ((out-r float*) (out-g float*) (out-b float*)
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
                   (rx-dir (+ f-x (+ (* r-x sx scale) (* u-x sy scale))))
                   (ry-dir (+ f-y (+ (* r-y sx scale) (* u-y sy scale))))
                   (rz-dir (+ f-z (+ (* r-z sx scale) (* u-z sy scale))))
                   (dir-len (sqrt (+ (* rx-dir rx-dir) (+ (* ry-dir ry-dir) (* rz-dir rz-dir)))))
                   (inv-dir-len (/ 1.0f0 dir-len))
                   (dx (* rx-dir inv-dir-len))
                   (dy (* ry-dir inv-dir-len))
                   (dz (* rz-dir inv-dir-len))
                   
                   ;; Ray Origin
                   (ox eye-x)
                   (oy eye-y)
                   (oz eye-z)
                   
                   ;; Color Buffers & Active State
                   (accum-r 0.0f0)
                   (accum-g 0.0f0)
                   (accum-b 0.0f0)
                   (throughput 1.0f0)
                   (active 1))
              
              ;; Ray Tracing Recursion (Iterative loop matching depth <= 2)
              (do ((depth 0 (+ depth 1)))
                  ((>= depth 3))
                (if (= active 1)
                    (let ((hit-t 1.0f10)
                          (hit-type 0) ; 0=none, 1=sphere, 2=plane
                          (hit-idx -1))
                      
                      ;; 1. Check Spheres
                      (do ((i 0 (+ i 1)))
                          ((>= i num-spheres))
                        (let* ((cx (aref sphere-cx i))
                               (cy (aref sphere-cy i))
                               (cz (aref sphere-cz i))
                               (r (aref sphere-r i))
                               (vx (- ox cx))
                               (vy (- oy cy))
                               (vz (- oz cz))
                               (b-prime (+ (* vx dx) (* vy dy) (* vz dz)))
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
                                (if (< t-val hit-t)
                                    (progn
                                      (set hit-t t-val)
                                      (set hit-type 1)
                                      (set hit-idx i)))))))
                      
                      ;; 2. Check Checker Plane (Normal (0, -1, 0), Point (0, 500, -1400))
                      (let* ((den (- dy))
                             (abs-den (if (> den 0.0f0) den (- den))))
                        (if (> abs-den 1.0f-8)
                            (let* ((hit-t-plane (/ (- oy 500.0f0) den)))
                              (if (> hit-t-plane 0.001f0)
                                  (let* ((ix-p (+ ox (* hit-t-plane dx)))
                                         (iz-p (+ oz (* hit-t-plane dz)))
                                         (abs-ix-p (if (> ix-p 0.0f0) ix-p (- ix-p)))
                                         (iz-p-diff (- iz-p -1400.0f0))
                                         (abs-iz-p-diff (if (> iz-p-diff 0.0f0) iz-p-diff (- iz-p-diff))))
                                    (if (<= abs-ix-p 2500.0f0)
                                        (if (<= abs-iz-p-diff 2500.0f0)
                                            (if (< hit-t-plane hit-t)
                                                (progn
                                                  (set hit-t hit-t-plane)
                                                  (set hit-type 2)
                                                  (set hit-idx -1))))))))))
                      
                      ;; Apply Shading/Sky background
                      (if (= hit-type 0)
                          ;; Sky Background Gradient
                          (let* ((sky-t (if (< dy sky-yr-min)
                                            0.0f0
                                            (if (> dy sky-yr-max)
                                                1.0f0
                                                (/ (- dy sky-yr-min) (- sky-yr-max sky-yr-min)))))
                                 (sky-t-pow (expt sky-t 0.3f0))
                                 (sky-r (+ (* (- 1.0f0 sky-t-pow) 1.0f0) (* sky-t-pow 0.2f0)))
                                 (sky-g (+ (* (- 1.0f0 sky-t-pow) 1.0f0) (* sky-t-pow 0.5f0)))
                                 (sky-b (+ (* (- 1.0f0 sky-t-pow) 1.0f0) (* sky-t-pow 1.0f0))))
                            (if (= depth 0)
                                (progn
                                  (set accum-r sky-r)
                                  (set accum-g sky-g)
                                  (set accum-b sky-b))
                                (let ((lum (* 0.333f0 (+ (+ sky-r sky-g) sky-b))))
                                  (set accum-r (+ accum-r (* throughput lum)))
                                  (set accum-g (+ accum-g (* throughput lum)))
                                  (set accum-b (+ accum-b (* throughput lum)))))
                            (set active 0))
                          
                          ;; Hit Object Surface Shading
                          (let* ((hit-x (+ ox (* hit-t dx)))
                                 (hit-y (+ oy (* hit-t dy)))
                                 (hit-z (+ oz (* hit-t dz)))
                                 
                                 (nx 0.0f0)
                                 (ny 0.0f0)
                                 (nz 0.0f0)
                                 
                                 (col-r 0.0f0)
                                 (col-g 0.0f0)
                                 (col-b 0.0f0)
                                 (refl-base 0.0f0))
                            
                            (if (= hit-type 1)
                                ;; Sphere Hit
                                (let* ((cx (aref sphere-cx hit-idx))
                                       (cy (aref sphere-cy hit-idx))
                                       (cz (aref sphere-cz hit-idx))
                                       (r (aref sphere-r hit-idx))
                                       (inv-r (/ 1.0f0 r)))
                                  (set nx (* (- hit-x cx) inv-r))
                                  (set ny (* (- hit-y cy) inv-r))
                                  (set nz (* (- hit-z cz) inv-r))
                                  (set col-r (aref sphere-col-r hit-idx))
                                  (set col-g (aref sphere-col-g hit-idx))
                                  (set col-b (aref sphere-col-b hit-idx))
                                  (set refl-base (aref sphere-refl hit-idx)))
                                
                                ;; Plane Hit
                                (progn
                                  (set nx 0.0f0)
                                  (set ny -1.0f0)
                                  (set nz 0.0f0)
                                  ;; Check checkerboard pattern mathematically using sin(pi * x / W) * sin(pi * z / W) > 0
                                  (let* ((val (* (sin (* (/ 3.14159265f0 140.0f0) hit-x))
                                                 (sin (* (/ 3.14159265f0 140.0f0) (- hit-z -1400.0f0)))))
                                         (is-even (> val 0.0f0)))
                                    (if is-even
                                        (progn (set col-r 0.9f0) (set col-g 0.9f0) (set col-b 0.9f0))
                                        (progn (set col-r 0.2f0) (set col-g 0.2f0) (set col-b 0.2f0))))
                                  (set refl-base 0.05f0)))
                            
                            ;; Soft Shadow Factor (Vogel Sampling 64 points)
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
                                       (sh-dist (sqrt (+ (* sh-dx sh-dx) (+ (* sh-dy sh-dy) (* sh-dz sh-dz)))))
                                       (inv-sh-dist (/ 1.0f0 sh-dist))
                                       (sh-dir-x (* sh-dx inv-sh-dist))
                                       (sh-dir-y (* sh-dy inv-sh-dist))
                                       (sh-dir-z (* sh-dz inv-sh-dist))
                                       
                                       (eps (* 0.0005f0 sh-dist))
                                       (off-x (+ hit-x (* nx eps)))
                                       (off-y (+ hit-y (* ny eps)))
                                       (off-z (+ hit-z (* nz eps)))
                                       (dist-offset (sqrt (+ (* (- lp-x off-x) (- lp-x off-x))
                                                             (+ (* (- lp-y off-y) (- lp-y off-y))
                                                                (* (- lp-z off-z) (- lp-z off-z))))))
                                       (blocked 0))
                                  
                                  ;; Test sphere blockers
                                  (do ((k 0 (+ k 1)))
                                      ((>= k num-spheres))
                                    (if (= blocked 0)
                                        ;; Avoid self-shadowing by checking if the blocker is not the hit sphere
                                        (let ((is-current-sphere 0))
                                          (if (= hit-type 1)
                                              (if (= hit-idx k)
                                                  (set is-current-sphere 1)))
                                          (if (= is-current-sphere 0)
                                              (let* ((cx (aref sphere-cx k))
                                                     (cy (aref sphere-cy k))
                                                     (cz (aref sphere-cz k))
                                                     (r (aref sphere-r k))
                                                     (vx (- off-x cx))
                                                     (vy (- off-y cy))
                                                     (vz (- off-z cz))
                                                     (b-prime (+ (* vx sh-dir-x) (* vy sh-dir-y) (* vz sh-dir-z)))
                                                     (c-val (- (+ (* vx vx) (* vy vy) (* vz vz)) (* r r)))
                                                     (disc (- (* b-prime b-prime) c-val)))
                                                (if (>= disc 0.0f0)
                                                    (let* ((sqrt-disc (sqrt disc))
                                                           (t1 (- (- b-prime) sqrt-disc))
                                                           (t2 (+ (- b-prime) sqrt-disc))
                                                           (t-val (if (> t1 0.05f0)
                                                                      t1
                                                                      (if (> t2 0.05f0)
                                                                          t2
                                                                          1.0f10))))
                                                      (if (< t-val dist-offset)
                                                          (set blocked 1)))))))))
                                  
                                  ;; Test plane blockers
                                  (if (= blocked 0)
                                      (if (= hit-type 1)
                                          (let* ((den-sh (- sh-dir-y))
                                                 (abs-den-sh (if (> den-sh 0.0f0) den-sh (- den-sh))))
                                            (if (> abs-den-sh 1.0f-8)
                                                (let* ((hit-t-sh (/ (- off-y 500.0f0) den-sh)))
                                                  (if (> hit-t-sh 0.001f0)
                                                      (if (< hit-t-sh dist-offset)
                                                          (let* ((ix-sh (+ off-x (* hit-t-sh sh-dir-x)))
                                                                 (iz-sh (+ off-z (* hit-t-sh sh-dir-z)))
                                                                 (abs-ix-sh (if (> ix-sh 0.0f0) ix-sh (- ix-sh)))
                                                                 (iz-sh-diff (- iz-sh -1400.0f0))
                                                                 (abs-iz-sh-diff (if (> iz-sh-diff 0.0f0) iz-sh-diff (- iz-sh-diff))))
                                                            (if (<= abs-ix-sh 2500.0f0)
                                                                (if (<= abs-iz-sh-diff 2500.0f0)
                                                                    (set blocked 1)))))))))))
                                  
                                  (if (= blocked 1)
                                      (set shadowed-sum (+ shadowed-sum 0.75f0))
                                      (set shadowed-sum (+ shadowed-sum 1.0f0))))
                            
                            ;; Base lighting calculations
                            (let* ((sf (/ shadowed-sum 64.0f0))
                                   (lc-x (- 600.0f0 hit-x))
                                   (lc-y (- 300.0f0 hit-y))
                                   (lc-z (- 200.0f0 hit-z))
                                   (lc-dist (sqrt (+ (* lc-x lc-x) (+ (* lc-y lc-y) (* lc-z lc-z)))))
                                   (inv-lc-dist (/ 1.0f0 lc-dist))
                                   (lc-dir-x (* lc-x inv-lc-dist))
                                   (lc-dir-y (* lc-y inv-lc-dist))
                                   (lc-dir-z (* lc-z inv-lc-dist))
                                   
                               (dot-lc-n (+ (* lc-dir-x nx) (+ (* lc-dir-y ny) (+ (* lc-dir-z nz) 0.0f0))))
                                   (parent-lambert dot-lc-n)
                                   (lambert (if (> parent-lambert 0.0f0) parent-lambert 0.0f0))
                                   (diff (* sf lambert))
                                   
                                   ;; CPU specular transcription
                                   (lx lc-dir-x)
                                   (ly lc-dir-y)
                                   (lz lc-dir-z)
                                   (ref-lx (- 0.0f0 lx))
                                   (ref-ly (- 0.0f0 ly))
                                   (ref-lz (- 0.0f0 lz))
                                   (ref-dot (+ (* ref-lx nx) (+ (* ref-ly ny) (* ref-lz nz))))
                                   (rx-l (- ref-lx (* 2.0f0 (* ref-dot nx))))
                                   (ry-l (- ref-ly (* 2.0f0 (* ref-dot ny))))
                                   (rz-l (- ref-lz (* 2.0f0 (* ref-dot nz))))
                                   (min-dx (- 0.0f0 dx))
                                   (min-dy (- 0.0f0 dy))
                                   (min-dz (- 0.0f0 dz))
                                   (vdot-val (+ (* rx-l min-dx) (+ (* ry-l min-dy) (* rz-l min-dz))))
                                   (vdot (if (> vdot-val 0.0f0) vdot-val 0.0f0))
                                   (v2 (* vdot vdot))
                                   (v4 (* v2 v2))
                                   (spec (* 1.5f0 (* sf (* v4 v4))))
                                   
                                   (base (+ 0.25f0 (+ (* 0.7f0 diff) spec)))
                                   (base-r (* col-r base))
                                   (base-g (* col-g base))
                                   (base-b (* col-b base))
                                   
                                   ;; Fresnel reflectivity calculation
                                   (dot-v-n (+ (* (- dx) nx) (+ (* (- dy) ny) (* (- dz) nz))))
                                   (vdot-refl (if (> dot-v-n 0.0f0) dot-v-n 0.0f0))
                                   (refl (+ refl-base (* (- 1.0f0 refl-base) (expt (- 1.0f0 vdot-refl) 5.0f0)))))
                              
                              ;; Update pixel accumulation color
                              (if (= depth 0)
                                  (progn
                                    (set accum-r base-r)
                                    (set accum-g base-g)
                                    (set accum-b base-b)
                                    (set throughput refl))
                                  (let ((lum (* 0.3333333f0 (+ base-r (+ base-g base-b)))))
                                    (set accum-r (+ accum-r (* throughput lum)))
                                    (set accum-g (+ accum-g (* throughput lum)))
                                    (set accum-b (+ accum-b (* throughput lum)))
                                    (set throughput (* throughput refl))))
                              
                              ;; Setup ray reflection direction for next iteration
                              (let* ((dot-d-n (+ (* dx nx) (+ (* dy ny) (* dz nz))))
                                     (rx-dir-n (- dx (* 2.0f0 dot-d-n nx)))
                                     (ry-dir-n (- dy (* 2.0f0 dot-d-n ny)))
                                     (rz-dir-n (- dz (* 2.0f0 dot-d-n nz)))
                                     (norm-rn (sqrt (+ (* rx-dir-n rx-dir-n) (+ (* ry-dir-n ry-dir-n) (* rz-dir-n rz-dir-n)))))
                                     (inv-norm-rn (/ 1.0f0 norm-rn)))
                                (set ox (+ hit-x (* nx 0.001f0)))
                                (set oy (+ hit-y (* ny 0.001f0)))
                                (set oz (+ hit-z (* nz 0.001f0)))
                                (set dx (* rx-dir-n inv-norm-rn))
                                (set dy (* ry-dir-n inv-norm-rn))
                                (set dz (* rz-dir-n inv-norm-rn)))
                              
                              (let ((should-deactivate 0))
                                (if (< throughput 0.0001f0)
                                    (set should-deactivate 1))
                                (if (<= refl 0.0f0)
                                    (set should-deactivate 1))
                                (if (= should-deactivate 1)
                                    (set active 0))))))))))
              
              ;; Output raw floating-point pixel colors
              (set (aref out-r pixel-idx) accum-r)
              (set (aref out-g pixel-idx) accum-g)
              (set (aref out-b pixel-idx) accum-b)))))))
              
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
         (ray1-len (sqrt (+ (* ray1-x ray1-x) (+ (* ray1-y ray1-y) (* ray1-z ray1-z)))))
         (yr1 (/ ray1-y ray1-len))
         
         (ray2-x (- fx (* ux 1.0f0 scale)))
         (ray2-y (- fy (* uy 1.0f0 scale)))
         (ray2-z (- fz (* uz 1.0f0 scale)))
         (ray2-len (sqrt (+ (* ray2-x ray2-x) (+ (* ray2-y ray2-y) (* ray2-z ray2-z)))))
         (yr2 (/ ray2-y ray2-len))
         
         (sky-yr-min (float (min yr1 yr2) 1.0f0))
         (sky-yr-max (float (max yr1 yr2) 1.0f0))
         
         (sphere-data nil))
    
    ;; Define Scene Objects (statically matching the deterministic CPU Lisp output)
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

