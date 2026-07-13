;;;; gpu-raytracer.lsp
;;;; Common Lisp GPU Raytracer using cl-cuda

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(load (merge-pathnames "gpu-package.lsp" *load-truename*))

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
       (do ((sphere-index 0 (+ sphere-index 1)))
           ((>= sphere-index num-spheres))
         (let* ((cx (aref sphere-cx sphere-index))
                (cy (aref sphere-cy sphere-index))
                (cz (aref sphere-cz sphere-index))
                (r (aref sphere-r sphere-index))
                (vx (- ,ox cx))
                (vy (- ,oy cy))
                (vz (- ,oz cz))
                (b-prime (+ (* vx ,dx) (* vy ,dy) (* vz ,dz)))
                (c-val (- (+ (* vx vx) (* vy vy) (* vz vz)) (* r r)))
                (disc (- (* b-prime b-prime) c-val)))
           (if (>= disc 0.0f0)
               (let* ((sqrt-disc (sqrt disc))
                      ;; Do not name these T1/T2: at reflection depths the
                      ;; caller's nearest-hit accumulator is also T1, and
                      ;; cl-cuda emits the local declaration in the same C++
                      ;; scope, shadowing that accumulator.
                      (near-root (- (- b-prime) sqrt-disc))
                      (far-root (+ (- b-prime) sqrt-disc))
                      (t-val (if (> near-root 0.001f0)
                                 near-root
                                 (if (> far-root 0.001f0)
                                     far-root
                                     1.0f10))))
                 (if (< t-val ,hit-t)
                     (progn
                       (set ,hit-t t-val)
                       (set ,hit-type 1)
                       (set ,hit-idx sphere-index)))))))
       ;; 2. Check Checker Plane (Normal (0, -1, 0), Point (0, 500, -1400))
       (let* ((den (- ,dy))
              (abs-den (if (> den 0.0f0) den (- den))))
         (if (> abs-den 1.0f-8)
             (let* ((hit-t-plane (/ (- ,oy 500.0f0) den)))
               (if (> hit-t-plane 0.001f0)
                  (let* ((checker-hit-x (+ ,ox (* hit-t-plane ,dx)))
                          (iz-p (+ ,oz (* hit-t-plane ,dz)))
                          (abs-ix-p (if (> checker-hit-x 0.0f0) checker-hit-x (- checker-hit-x)))
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
                                ;; Keep these distinct from T1/T2/T3 used
                                ;; by the unrolled reflection depths.
                                (near-root (- (- b-prime) sqrt-disc))
                                (far-root (+ (- b-prime) sqrt-disc))
                                ;; Correct bias logic: find the closest hit > 0.001f0 first, then test if > 0.05f0.
                                (t-min (if (> near-root 0.001f0)
                                           near-root
                                           (if (> far-root 0.001f0) far-root 1.0f10)))
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

  ;; CPU surface-color-at, surface-reflectivity, and normal, in one GPU template.
  ;; Every ray depth must use this template rather than its own copy.
  (defun make-load-surface-data (hit-type hit-idx hit-x hit-y hit-z
                                  nx ny nz col-r col-g col-b refl-base)
    `(if (= ,hit-type 1)
         (let* ((cx (aref sphere-cx ,hit-idx))
                (cy (aref sphere-cy ,hit-idx))
                (cz (aref sphere-cz ,hit-idx))
                (r (aref sphere-r ,hit-idx))
                (inv-r (/ 1.0f0 r)))
           (set ,nx (* (- ,hit-x cx) inv-r))
           (set ,ny (* (- ,hit-y cy) inv-r))
           (set ,nz (* (- ,hit-z cz) inv-r))
           (set ,col-r (aref sphere-col-r ,hit-idx))
           (set ,col-g (aref sphere-col-g ,hit-idx))
           (set ,col-b (aref sphere-col-b ,hit-idx))
           (set ,refl-base (aref sphere-refl ,hit-idx)))
         (progn
           (set ,nx 0.0f0)
           (set ,ny -1.0f0)
           (set ,nz 0.0f0)
           (let* ((x-div (/ ,hit-x 140.0f0))
                  (z-div (/ (- ,hit-z -1400.0f0) 140.0f0))
                  (checker-ix (floor x-div))
                  (checker-iz (floor z-div))
                  (checker-sum (+ checker-ix checker-iz))
                  (div2 (* checker-sum 0.5f0))
                  (is-even (< (- div2 (floor div2)) 0.25f0)))
             (if is-even
                 (progn (set ,col-r 0.9f0) (set ,col-g 0.9f0) (set ,col-b 0.9f0))
                 (progn (set ,col-r 0.2f0) (set ,col-g 0.2f0) (set ,col-b 0.2f0))))
           (set ,refl-base 0.05f0))))

  (defun make-compute-shading (ox oy oz dx dy dz hit-t hit-type hit-idx r-val g-val b-val refl-val
                               &optional shadow-factor-out)
    `(let* ((hit-x (+ ,ox (* ,hit-t ,dx)))
            (hit-y (+ ,oy (* ,hit-t ,dy)))
            (hit-z (+ ,oz (* ,hit-t ,dz)))
            (nx 0.0f0) (ny 0.0f0) (nz 0.0f0)
            (col-r 0.0f0) (col-g 0.0f0) (col-b 0.0f0)
            (refl-base 0.0f0))
       ,(make-load-surface-data hit-type hit-idx 'hit-x 'hit-y 'hit-z
                                 'nx 'ny 'nz 'col-r 'col-g 'col-b 'refl-base)
       
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
                (spec-vdot (if (> vdot-val 0.0f0) vdot-val 0.0f0))
                (v2 (* spec-vdot spec-vdot))
                (v4 (* v2 v2))
                (spec (* 1.5f0 (* sf (* v4 v4))))
                
                (base (+ 0.25f0 (* 0.7f0 diff) spec))
                
                ;; Fresnel reflectivity calculation
                (reflect-view-dot (+ (* (- ,dx) nx) (* (- ,dy) ny) (* (- ,dz) nz)))
                (vdot-refl (if (> reflect-view-dot 0.0f0) reflect-view-dot 0.0f0))
                ;; Kept textually equivalent to CPU/sendray.
                (fresnel-refl (+ refl-base (* (- 1.0f0 refl-base) (expt (- 1.0f0 vdot-refl) 5.0f0)))))
           
           (set ,r-val (* col-r base))
           (set ,g-val (* col-g base))
           (set ,b-val (* col-b base))
           (set ,refl-val fresnel-refl)
           ;; Do not insert NIL into the GPU AST when this optional output is
           ;; omitted: cl-cuda accepts statements only, not NIL forms.
           ,@(when shadow-factor-out
               (list `(set ,shadow-factor-out sf)))))))

  (defun make-compute-sky-color (dy r-val g-val b-val)
    ;; Same sky gradient as CPU/sendray.
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
         ;; 0.001 is only a few single-precision ULPs at this scene's
         ;; coordinates (up to roughly 3000), so reflected rays re-hit their
         ;; source sphere.  This bias is still negligible against the
         ;; smallest sphere radius (40) but reliably clears the surface.
         (set ,ox (+ ,hit-x (* ,nx 0.05f0)))
         (set ,oy (+ ,hit-y (* ,ny 0.05f0)))
         (set ,oz (+ ,hit-z (* ,nz 0.05f0))))))

  ;; Trace a ray through a spherical glass surface and out through its far
  ;; surface.  This is the GPU counterpart of CPU trace-refraction: only the
  ;; transmitted branch is followed here, while the caller composes it with
  ;; the ordinary reflected branch using Schlick Fresnel.
  (defun make-trace-glass-transmission
      (hit-x hit-y hit-z dx dy dz nx ny nz ior source-idx out-r out-g out-b)
    `(progn
       (set ,out-r 0.0f0)
       (set ,out-g 0.0f0)
       (set ,out-b 0.0f0)
       ;; Enter the glass (air -> IOR).
       (let* ((dot-in (+ (* ,dx ,nx) (* ,dy ,ny) (* ,dz ,nz)))
              (eta-in (/ 1.0f0 ,ior))
              (cos-in (- 0.0f0 dot-in))
              (k-in (- 1.0f0 (* eta-in eta-in (- 1.0f0 (* cos-in cos-in))))))
         (if (>= k-in 0.0f0)
             (let* ((sqrt-k-in (sqrt k-in))
                    (tx-in (+ (* eta-in ,dx) (* (- (* eta-in cos-in) sqrt-k-in) ,nx)))
                    (ty-in (+ (* eta-in ,dy) (* (- (* eta-in cos-in) sqrt-k-in) ,ny)))
                    (tz-in (+ (* eta-in ,dz) (* (- (* eta-in cos-in) sqrt-k-in) ,nz)))
                    (len-in (sqrt (+ (* tx-in tx-in) (* ty-in ty-in) (* tz-in tz-in))))
                    (inv-len-in (/ 1.0f0 len-in))
                    (dx-in (* tx-in inv-len-in))
                    (dy-in (* ty-in inv-len-in))
                    (dz-in (* tz-in inv-len-in))
                    (ox-in (+ ,hit-x (* dx-in 0.05f0)))
                    (oy-in (+ ,hit-y (* dy-in 0.05f0)))
                    (oz-in (+ ,hit-z (* dz-in 0.05f0)))
                    (t-in 1.0f10) (type-in 0) (idx-in -1))
               ,(make-find-first-hit 'ox-in 'oy-in 'oz-in 'dx-in 'dy-in 'dz-in
                                     't-in 'type-in 'idx-in)
               (if (= type-in 0)
                   ,(make-compute-sky-color 'dy-in out-r out-g out-b)
                   ;; A ray entering a convex glass sphere normally reaches
                   ;; the far side of that same sphere.  Refract it back into
                   ;; air before obtaining the transmitted scene colour.
                   (if (= type-in 1)
                       (if (= idx-in ,source-idx)
                           (let* ((exit-x (+ ox-in (* t-in dx-in)))
                                  (exit-y (+ oy-in (* t-in dy-in)))
                                  (exit-z (+ oz-in (* t-in dz-in)))
                                  (cx-exit (aref sphere-cx idx-in))
                                  (cy-exit (aref sphere-cy idx-in))
                                  (cz-exit (aref sphere-cz idx-in))
                                  (r-exit (aref sphere-r idx-in))
                                  (inv-r-exit (/ 1.0f0 r-exit))
                                  (nx-exit (* (- exit-x cx-exit) inv-r-exit))
                                  (ny-exit (* (- exit-y cy-exit) inv-r-exit))
                                  (nz-exit (* (- exit-z cz-exit) inv-r-exit))
                                  ;; The ray is inside, so the normal is
                                  ;; reversed for Snell's law (IOR -> air).
                                  (onx (- 0.0f0 nx-exit))
                                  (ony (- 0.0f0 ny-exit))
                                  (onz (- 0.0f0 nz-exit))
                                  (cos-out (- 0.0f0 (+ (* dx-in onx) (* dy-in ony) (* dz-in onz))))
                                  (eta-out ,ior)
                                  (k-out (- 1.0f0 (* eta-out eta-out
                                                       (- 1.0f0 (* cos-out cos-out))))))
                             (if (>= k-out 0.0f0)
                                 (let* ((sqrt-k-out (sqrt k-out))
                                        (tx-out (+ (* eta-out dx-in) (* (- (* eta-out cos-out) sqrt-k-out) onx)))
                                        (ty-out (+ (* eta-out dy-in) (* (- (* eta-out cos-out) sqrt-k-out) ony)))
                                        (tz-out (+ (* eta-out dz-in) (* (- (* eta-out cos-out) sqrt-k-out) onz)))
                                        (len-out (sqrt (+ (* tx-out tx-out) (* ty-out ty-out) (* tz-out tz-out))))
                                        (inv-len-out (/ 1.0f0 len-out))
                                        (dx-out (* tx-out inv-len-out))
                                        (dy-out (* ty-out inv-len-out))
                                        (dz-out (* tz-out inv-len-out))
                                        (ox-out (+ exit-x (* dx-out 0.05f0)))
                                        (oy-out (+ exit-y (* dy-out 0.05f0)))
                                        (oz-out (+ exit-z (* dz-out 0.05f0)))
                                        (t-out 1.0f10) (type-out 0) (idx-out -1))
                                   ,(make-find-first-hit 'ox-out 'oy-out 'oz-out 'dx-out 'dy-out 'dz-out
                                                         't-out 'type-out 'idx-out)
                                   (if (= type-out 0)
                                       ,(make-compute-sky-color 'dy-out out-r out-g out-b)
                                       (let ((trans-refl 0.0f0))
                                         ,(make-compute-shading 'ox-out 'oy-out 'oz-out
                                                                'dx-out 'dy-out 'dz-out
                                                                't-out 'type-out 'idx-out
                                                                out-r out-g out-b 'trans-refl))))))
                           ;; An unexpected internal hit still contributes its
                           ;; local colour rather than leaving a black path.
                           (let ((trans-refl 0.0f0))
                             ,(make-compute-shading 'ox-in 'oy-in 'oz-in
                                                    'dx-in 'dy-in 'dz-in
                                                    't-in 'type-in 'idx-in
                                                    out-r out-g out-b 'trans-refl)))
                       ;; A refracted ray can also reach the plane before a
                       ;; sphere boundary in degenerate scene configurations.
                       (let ((trans-refl 0.0f0))
                         ,(make-compute-shading 'ox-in 'oy-in 'oz-in
                                                'dx-in 'dy-in 'dz-in
                                                't-in 'type-in 'idx-in
                                                out-r out-g out-b 'trans-refl)))))))))

  (defun make-compose-glass (base-r base-g base-b refl-r refl-g refl-b
                             trans-r trans-g trans-b ior dx dy dz nx ny nz
                             out-r out-g out-b)
    ;; Matches CPU compose-color's glass branch: 50% local colour and 50%
    ;; Fresnel-weighted reflection/refraction.
    `(let* ((view-dot (+ (* (- ,dx) ,nx) (* (- ,dy) ,ny) (* (- ,dz) ,nz)))
            (vdot (if (> view-dot 0.0f0) view-dot 0.0f0))
            (ior-ratio (/ (- ,ior 1.0f0) (+ ,ior 1.0f0)))
            (f0 (* ior-ratio ior-ratio))
            (fresnel (+ f0 (* (- 1.0f0 f0) (expt (- 1.0f0 vdot) 5.0f0))))
            (trans-weight (- 1.0f0 fresnel)))
       (set ,out-r (+ (* 0.5f0 ,base-r)
                      (* 0.5f0 (+ (* fresnel ,refl-r) (* trans-weight ,trans-r)))))
       (set ,out-g (+ (* 0.5f0 ,base-g)
                      (* 0.5f0 (+ (* fresnel ,refl-g) (* trans-weight ,trans-g)))))
       (set ,out-b (+ (* 0.5f0 ,base-b)
                      (* 0.5f0 (+ (* fresnel ,refl-b) (* trans-weight ,trans-b)))))))

  (defun make-clamp-rgb (source-r source-g source-b out-r out-g out-b)
    `(progn
       (set ,out-r (if (< ,source-r 0.0f0) 0.0f0 (if (> ,source-r 1.0f0) 1.0f0 ,source-r)))
       (set ,out-g (if (< ,source-g 0.0f0) 0.0f0 (if (> ,source-g 1.0f0) 1.0f0 ,source-g)))
       (set ,out-b (if (< ,source-b 0.0f0) 0.0f0 (if (> ,source-b 1.0f0) 1.0f0 ,source-b)))))

  ;; CPU: base-color + reflectivity * grayscale(reflected-color).
  (defun make-compose-reflection (base-r base-g base-b refl child-r child-g child-b out-r out-g out-b)
    ;; Do not introduce temporary local variables here.  cl-cuda's generated
    ;; code lost this contribution in the recursive call site, although the
    ;; identical term evaluated correctly in the diagnostic kernel.
    `(progn
       (set ,out-r
            (if (< (+ ,base-r (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))) 0.0f0)
                0.0f0
                (if (> (+ ,base-r (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))) 1.0f0)
                    1.0f0
                    (+ ,base-r (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))))))
       (set ,out-g
            (if (< (+ ,base-g (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))) 0.0f0)
                0.0f0
                (if (> (+ ,base-g (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))) 1.0f0)
                    1.0f0
                    (+ ,base-g (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))))))
       (set ,out-b
            (if (< (+ ,base-b (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))) 0.0f0)
                0.0f0
                (if (> (+ ,base-b (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))) 1.0f0)
                    1.0f0
                    (+ ,base-b (* ,refl (* 0.333f0 (+ (+ ,child-r ,child-g) ,child-b)))))))))

;; GPU Raytracer Kernel definition utilizing code templates to expand exactly 3 recursion levels.
(eval
  `(defkernel raytrace-kernel-v11 (void ((out-r float*) (out-g float*) (out-b float*)
                                    (out-shadow float*)
                                    ;; Primary shading before any recursive reflection.
                                    (out-direct-r float*) (out-direct-g float*) (out-direct-b float*)
                                    ;; Primary shading plus the first reflected hit only.
                                    (out-one-bounce-r float*) (out-one-bounce-g float*) (out-one-bounce-b float*)
                                    (width int) (height int)
                                    (width-f cl-cuda:float) (height-f cl-cuda:float)
                                    (sphere-cx float*) (sphere-cy float*) (sphere-cz float*)
                                    (sphere-r float*)
                                    (sphere-col-r float*) (sphere-col-g float*) (sphere-col-b float*)
                                    (sphere-refl float*) (sphere-ior float*)
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
                      ;; cl-cuda has no FLOAT cast builtin.  Multiplication by
                      ;; a single-float literal performs the required cast.
                      (sx (- (* 2.0f0 (* (+ (* 1.0f0 ix) 0.5f0) inv-w)) 1.0f0))
                      (sy (- 1.0f0 (* 2.0f0 (* (+ (* 1.0f0 iy) 0.5f0) inv-h))))
                      
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
                      (accum-b 0.0f0)
                      (direct-r 0.0f0)
                      (direct-g 0.0f0)
                      (direct-b 0.0f0)
                      (one-bounce-r 0.0f0)
                      (one-bounce-g 0.0f0)
                      (one-bounce-b 0.0f0)
                      (shadow0 1.0f0))
                 
                 ;; Stage 0 (depth = 0)
                 (let ((t0 1.0f10) (type0 0) (idx0 -1))
                   ,(make-find-first-hit 'eye-x 'eye-y 'eye-z 'dx 'dy 'dz 't0 'type0 'idx0)
                   (if (= type0 0)
                       ;; hit nothing -> sky
                       (progn
                         ,(make-compute-sky-color 'dy 'accum-r 'accum-g 'accum-b)
                         (set direct-r accum-r)
                         (set direct-g accum-g)
                         (set direct-b accum-b)
                         (set one-bounce-r accum-r)
                         (set one-bounce-g accum-g)
                         (set one-bounce-b accum-b))
                       
                       ;; hit object
                       (let* ((hit-x0 (+ eye-x (* t0 dx)))
                              (hit-y0 (+ eye-y (* t0 dy)))
                              (hit-z0 (+ eye-z (* t0 dz)))
                              (nx0 0.0f0) (ny0 0.0f0) (nz0 0.0f0)
                              (col-r0 0.0f0) (col-g0 0.0f0) (col-b0 0.0f0)
                              (refl-base0 0.0f0)
                              ;; Planes are opaque.  Sphere IOR values match
                              ;; the CPU animation material definitions.
                              (ior0 (if (= type0 1) (aref sphere-ior idx0) 1.0f0)))
                         
                         ,(make-load-surface-data 'type0 'idx0 'hit-x0 'hit-y0 'hit-z0
                                                   'nx0 'ny0 'nz0
                                                   'col-r0 'col-g0 'col-b0 'refl-base0)
                         
                         (let ((r0 0.0f0) (g0 0.0f0) (b0 0.0f0) (refl0 0.0f0)
                               (child1-direct-r 0.0f0) (child1-direct-g 0.0f0) (child1-direct-b 0.0f0)
                               (trans-r0 0.0f0) (trans-g0 0.0f0) (trans-b0 0.0f0))
                           ,(make-compute-shading 'eye-x 'eye-y 'eye-z 'dx 'dy 'dz 't0 'type0 'idx0 'r0 'g0 'b0 'refl0 'shadow0)
                           (set direct-r r0)
                           (set direct-g g0)
                           (set direct-b b0)
                           
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
                                       (progn
                                         ,(make-compute-sky-color 'dy1 'r1 'g1 'b1)
                                         (set child1-direct-r r1)
                                         (set child1-direct-g g1)
                                         (set child1-direct-b b1))
                                       
                                       ;; hit object 1
                                       (let* ((hit-x1 (+ ox1 (* t1 dx1)))
                                              (hit-y1 (+ oy1 (* t1 dy1)))
                                              (hit-z1 (+ oz1 (* t1 dz1)))
                                              (nx1 0.0f0) (ny1 0.0f0) (nz1 0.0f0)
                                              (col-r1 0.0f0) (col-g1 0.0f0) (col-b1 0.0f0)
                                              (refl-base1 0.0f0))
                                         ,(make-load-surface-data 'type1 'idx1 'hit-x1 'hit-y1 'hit-z1
                                                                   'nx1 'ny1 'nz1
                                                                   'col-r1 'col-g1 'col-b1 'refl-base1)
                                         
                                         (let ((r1-base 0.0f0) (g1-base 0.0f0) (b1-base 0.0f0) (refl1 0.0f0))
                                           ,(make-compute-shading 'ox1 'oy1 'oz1 'dx1 'dy1 'dz1 't1 'type1 'idx1 'r1-base 'g1-base 'b1-base 'refl1)
                                           (set child1-direct-r r1-base)
                                           (set child1-direct-g g1-base)
                                           (set child1-direct-b b1-base)
                                           
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
                                                           ,(make-load-surface-data 'type2 'idx2 'hit-x2 'hit-y2 'hit-z2
                                                                                     'nx2 'ny2 'nz2
                                                                                     'col-r2 'col-g2 'col-b2 'refl-base2)
                                                           
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
                                                                                  (col-r3 0.0f0) (col-g3 0.0f0) (col-b3 0.0f0)
                                                                                  (refl-base3 0.0f0))
                                                                             ,(make-load-surface-data 'type3 'idx3 'hit-x3 'hit-y3 'hit-z3
                                                                                                       'nx3 'ny3 'nz3
                                                                                                       'col-r3 'col-g3 'col-b3 'refl-base3)
                                                                             
                                                                             (let ((r3-base 0.0f0) (g3-base 0.0f0) (b3-base 0.0f0) (refl3 0.0f0))
                                                                               ,(make-compute-shading 'ox3 'oy3 'oz3 'dx3 'dy3 'dz3 't3 'type3 'idx3 'r3-base 'g3-base 'b3-base 'refl3)
                                                                               ,(make-clamp-rgb 'r3-base 'g3-base 'b3-base 'r3 'g3 'b3))))
                                                                       
                                                                       ,(make-compose-reflection 'r2-base 'g2-base 'b2-base 'refl2
                                                                                                 'r3 'g3 'b3 'r2 'g2 'b2))))
                                                                   
                                                                   ,(make-clamp-rgb 'r2-base 'g2-base 'b2-base 'r2 'g2 'b2)))))
                                                   
                                                   ,(make-compose-reflection 'r1-base 'g1-base 'b1-base 'refl1
                                                                             'r2 'g2 'b2 'r1 'g1 'b1))))
                                               
                                               ,(make-clamp-rgb 'r1-base 'g1-base 'b1-base 'r1 'g1 'b1)))))
                                       
                                       (if (> ior0 1.01f0)
                                           (progn
                                             ,(make-trace-glass-transmission
                                               'hit-x0 'hit-y0 'hit-z0 'dx 'dy 'dz
                                               'nx0 'ny0 'nz0 'ior0 'idx0
                                               'trans-r0 'trans-g0 'trans-b0)
                                             ,(make-compose-glass
                                               'r0 'g0 'b0 'r1 'g1 'b1
                                               'trans-r0 'trans-g0 'trans-b0
                                               'ior0 'dx 'dy 'dz 'nx0 'ny0 'nz0
                                               'accum-r 'accum-g 'accum-b)
                                             ,(make-compose-glass
                                               'r0 'g0 'b0
                                               'child1-direct-r 'child1-direct-g 'child1-direct-b
                                               'trans-r0 'trans-g0 'trans-b0
                                               'ior0 'dx 'dy 'dz 'nx0 'ny0 'nz0
                                               'one-bounce-r 'one-bounce-g 'one-bounce-b))
                                           (progn
                                             ,(make-compose-reflection 'r0 'g0 'b0 'refl0
                                                                       'r1 'g1 'b1 'accum-r 'accum-g 'accum-b)
                                             ,(make-compose-reflection 'r0 'g0 'b0 'refl0
                                                                       'child1-direct-r 'child1-direct-g 'child1-direct-b
                                                                       'one-bounce-r 'one-bounce-g 'one-bounce-b)))
                            
                            ;; ACCUM-* already contains the recursive reflection
                            ;; composition.  Clamp that result; clamping R0/G0/B0
                            ;; here discarded every reflected contribution.
                            ,(make-clamp-rgb 'accum-r 'accum-g 'accum-b
                                             'accum-r 'accum-g 'accum-b)))))))
              
              ;; Output raw floating-point pixel colors
              (set (aref out-r pixel-idx) accum-r)
              (set (aref out-g pixel-idx) accum-g)
              (set (aref out-b pixel-idx) accum-b)
              (set (aref out-shadow pixel-idx) shadow0)
              (set (aref out-direct-r pixel-idx) direct-r)
              (set (aref out-direct-g pixel-idx) direct-g)
              (set (aref out-direct-b pixel-idx) direct-b)
              (set (aref out-one-bounce-r pixel-idx) one-bounce-r)
              (set (aref out-one-bounce-g pixel-idx) one-bounce-g)
              (set (aref out-one-bounce-b pixel-idx) one-bounce-b))))))))

;; This kernel deliberately does not participate in rendering.  It traces only
;; the first reflection ray so that its result can be inspected without
;; changing the production kernel above.
;; Output encoding:
;;   R = primary-surface Fresnel reflectivity
;;   G = first reflected-ray hit type (0.0 sky, 0.5 sphere, 1.0 plane)
;;   B = normalized reflected-ray distance (0.0 for sky)
(eval
  `(defkernel reflection-path-debug-kernel-v2
       (void ((out-r float*) (out-g float*) (out-b float*)
              ;; Shaded colour returned by the first reflection ray.
              (out-child-r float*) (out-child-g float*) (out-child-b float*)
              ;; The depth-0 reflection term before addition to the base.
              (out-contrib-r float*) (out-contrib-g float*) (out-contrib-b float*)
              ;; 1.0 only when the reflection ray immediately re-hits the
              ;; primary sphere; used to detect insufficient float ray bias.
              (out-self-hit float*)
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
                      (sx (- (* 2.0f0 (* (+ (* 1.0f0 ix) 0.5f0) (/ 1.0f0 width-f))) 1.0f0))
                      (sy (- 1.0f0 (* 2.0f0 (* (+ (* 1.0f0 iy) 0.5f0) (/ 1.0f0 height-f)))))
                      (raw-x (+ f-x (* r-x sx scale) (* u-x sy scale)))
                      (raw-y (+ f-y (* r-y sx scale) (* u-y sy scale)))
                      (raw-z (+ f-z (* r-z sx scale) (* u-z sy scale)))
                      (raw-len (sqrt (+ (* raw-x raw-x) (* raw-y raw-y) (* raw-z raw-z))))
                      (dx (* raw-x (/ 1.0f0 raw-len)))
                      (dy (* raw-y (/ 1.0f0 raw-len)))
                      (dz (* raw-z (/ 1.0f0 raw-len)))
                      (debug-r 0.0f0) (debug-g 0.0f0) (debug-b 0.0f0)
                      (child-r 0.0f0) (child-g 0.0f0) (child-b 0.0f0)
                      (contrib-r 0.0f0) (contrib-g 0.0f0) (contrib-b 0.0f0)
                      (self-hit 0.0f0))
                 (let ((t0 1.0f10) (type0 0) (idx0 -1))
                   ,(make-find-first-hit 'eye-x 'eye-y 'eye-z 'dx 'dy 'dz 't0 'type0 'idx0)
                   (if (= type0 0)
                       (progn
                         (set debug-r 0.0f0)
                         (set debug-g 0.0f0)
                         (set debug-b 0.0f0))
                       (let* ((hit-x0 (+ eye-x (* t0 dx)))
                              (hit-y0 (+ eye-y (* t0 dy)))
                              (hit-z0 (+ eye-z (* t0 dz)))
                              (nx0 0.0f0) (ny0 0.0f0) (nz0 0.0f0)
                              (col-r0 0.0f0) (col-g0 0.0f0) (col-b0 0.0f0)
                              (refl-base0 0.0f0))
                         ,(make-load-surface-data 'type0 'idx0 'hit-x0 'hit-y0 'hit-z0
                                                   'nx0 'ny0 'nz0
                                                   'col-r0 'col-g0 'col-b0 'refl-base0)
                         (let* ((debug-view-dot (+ (* (- dx) nx0) (* (- dy) ny0) (* (- dz) nz0)))
                                (debug-vdot (if (> debug-view-dot 0.0f0) debug-view-dot 0.0f0))
                                (refl (+ refl-base0
                                         (* (- 1.0f0 refl-base0)
                                            (expt (- 1.0f0 debug-vdot) 5.0f0))))
                                (ox1 0.0f0) (oy1 0.0f0) (oz1 0.0f0)
                                (dx1 0.0f0) (dy1 0.0f0) (dz1 0.0f0))
                           ,(make-update-reflection-ray 'hit-x0 'hit-y0 'hit-z0
                                                        'dx 'dy 'dz 'nx0 'ny0 'nz0
                                                        'dx1 'dy1 'dz1 'ox1 'oy1 'oz1)
                           (let ((t1 1.0f10) (type1 0) (idx1 -1))
                             ,(make-find-first-hit 'ox1 'oy1 'oz1 'dx1 'dy1 'dz1 't1 'type1 'idx1)
                             (if (= type0 1)
                                 (if (= type1 1)
                                     (if (= idx0 idx1)
                                         (set self-hit 1.0f0))))
                             ;; This is the exact child colour that the main
                             ;; kernel should supply to make-compose-reflection
                             ;; at depth 0.  It intentionally stops before the
                             ;; child's own reflection recursion.
                             (if (= type1 0)
                                 ,(make-compute-sky-color 'dy1 'child-r 'child-g 'child-b)
                                 (let ((child-refl 0.0f0))
                                   ,(make-compute-shading 'ox1 'oy1 'oz1
                                                          'dx1 'dy1 'dz1
                                                          't1 'type1 'idx1
                                                          'child-r 'child-g 'child-b
                                                          'child-refl)))
                             (set debug-r refl)
                             (set debug-g (if (= type1 2) 1.0f0
                                              (if (= type1 1) 0.5f0 0.0f0)))
                             (set debug-b (if (= type1 0) 0.0f0
                                              (/ 1.0f0 (+ 1.0f0 (* 0.001f0 t1)))))
                             (let ((child-lum (* 0.333f0 (+ (+ child-r child-g) child-b))))
                               (set contrib-r (* refl child-lum))
                               (set contrib-g (* refl child-lum))
                               (set contrib-b (* refl child-lum)))))))
                 (set (aref out-r pixel-idx) debug-r)
                 (set (aref out-g pixel-idx) debug-g)
                 (set (aref out-b pixel-idx) debug-b)
                 (set (aref out-child-r pixel-idx) child-r)
                 (set (aref out-child-g pixel-idx) child-g)
                 (set (aref out-child-b pixel-idx) child-b)
                 (set (aref out-contrib-r pixel-idx) contrib-r)
                 (set (aref out-contrib-g pixel-idx) contrib-g)
                 (set (aref out-contrib-b pixel-idx) contrib-b)
                 (set (aref out-self-hit pixel-idx) self-hit))))))))

;; Host side orchestration code
(defun run-gpu-raytracer (&key (res 8) (output-file "spheres_gpu.ppm")
                               frame
                               (write-debug-images t))
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
    ;; 3 large spheres.  FRAME uses the same sinusoidal paths as the CPU
    ;; animation; NIL preserves the still-image scene exactly.
    (let ((frame-f (if frame (float frame 1.0f0) 0.0f0))
          ;; Preserve the established still-image material; the CPU animation
          ;; used a stronger red reflection and a glass green sphere.
          (red-refl (if frame 0.2f0 0.02f0))
          (green-ior (if frame 1.1f0 1.0f0)))
      (push (list (* 150.0f0 (sin (* frame-f 0.1f0)))
                  -300.0f0 -1200.0f0 200.0f0
                  0.8f0 0.2f0 0.2f0 red-refl 1.0f0)
            sphere-data)
      (push (list -80.0f0
                  (+ -150.0f0 (* 100.0f0 (sin (* frame-f 0.15f0))))
                  -1200.0f0 200.0f0
                  0.2f0 0.8f0 0.2f0 0.2f0 green-ior)
            sphere-data)
      (push (list 70.0f0 -100.0f0
                  (+ -1200.0f0 (* 200.0f0 (sin (* frame-f 0.12f0))))
                  200.0f0 0.2f0 0.2f0 0.9f0 0.2f0 1.0f0)
            sphere-data))
    ;; Small spheres (matching CPU order and random generation)
    (let ((small-sphere-index 0))
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
                      0.1f0
                      ;; CPU animation: every third small sphere is glass.
                      (if frame
                          (if (> (mod small-sphere-index 3) 1) 1.5f0 1.0f0)
                          1.0f0))
                sphere-data)
          (incf small-sphere-index))))
    
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
                             (out-shadow 'cl-cuda:float size)
                             (out-direct-r 'cl-cuda:float size)
                             (out-direct-g 'cl-cuda:float size)
                             (out-direct-b 'cl-cuda:float size)
                             (out-one-bounce-r 'cl-cuda:float size)
                             (out-one-bounce-g 'cl-cuda:float size)
                             (out-one-bounce-b 'cl-cuda:float size)
                             ;; Reflection-path diagnostics: geometry, child
                             ;; shading, and the depth-0 reflected term.
                             (out-reflect-path-r 'cl-cuda:float size)
                             (out-reflect-path-g 'cl-cuda:float size)
                             (out-reflect-path-b 'cl-cuda:float size)
                             (out-reflect-child-r 'cl-cuda:float size)
                             (out-reflect-child-g 'cl-cuda:float size)
                             (out-reflect-child-b 'cl-cuda:float size)
                             (out-reflect-contrib-r 'cl-cuda:float size)
                             (out-reflect-contrib-g 'cl-cuda:float size)
                             (out-reflect-contrib-b 'cl-cuda:float size)
                             (out-reflect-self-hit 'cl-cuda:float size)
                             (sph-cx 'cl-cuda:float num-spheres)
                             (sph-cy 'cl-cuda:float num-spheres)
                             (sph-cz 'cl-cuda:float num-spheres)
                             (sph-r 'cl-cuda:float num-spheres)
                             (sph-col-r 'cl-cuda:float num-spheres)
                             (sph-col-g 'cl-cuda:float num-spheres)
                             (sph-col-b 'cl-cuda:float num-spheres)
                             (sph-refl 'cl-cuda:float num-spheres)
                             (sph-ior 'cl-cuda:float num-spheres))
          
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
                    (memory-block-aref sph-refl idx) (nth 7 s)
                    (memory-block-aref sph-ior idx) (nth 8 s))
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
          (sync-memory-block sph-ior :host-to-device)
          
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
          
          ;; Bump the kernel symbol whenever the generated program changes so
          ;; cl-cuda cannot reuse a module compiled for an older definition.
          (format t "Launching CUDA kernel v11 (Grid: ~Ax~A, Block: ~Ax~A)...~%" grid-x grid-y block-x block-y)
          (let ((start-time (get-internal-real-time)))
            (raytrace-kernel-v11 out-r out-g out-b out-shadow
                             out-direct-r out-direct-g out-direct-b
                             out-one-bounce-r out-one-bounce-g out-one-bounce-b
                             width height
                             width-f height-f
                             sph-cx sph-cy sph-cz
                             sph-r
                             sph-col-r sph-col-g sph-col-b
                             sph-refl sph-ior
                             num-spheres
                             eye-x eye-y eye-z
                             fx fy fz
                             rx ry rz
                             ux uy uz
                             scale
                             sky-yr-min sky-yr-max
                             :grid-dim (list grid-x grid-y 1)
                             :block-dim (list block-x block-y 1))

            ;; This diagnostic follows the same first reflection ray but
            ;; exposes its geometry and child shading separately.  It does
            ;; not feed back into the production render, and is skipped for
            ;; animation frames to avoid producing 60 sets of debug images.
            (when write-debug-images
              (reflection-path-debug-kernel-v2
               out-reflect-path-r out-reflect-path-g out-reflect-path-b
               out-reflect-child-r out-reflect-child-g out-reflect-child-b
               out-reflect-contrib-r out-reflect-contrib-g out-reflect-contrib-b
               out-reflect-self-hit
               width height width-f height-f
               sph-cx sph-cy sph-cz sph-r
               sph-col-r sph-col-g sph-col-b sph-refl num-spheres
               eye-x eye-y eye-z fx fy fz rx ry rz ux uy uz scale
               sky-yr-min sky-yr-max
               :grid-dim (list grid-x grid-y 1)
               :block-dim (list block-x block-y 1)))
            
            ;; Copy rendered pixels from GPU device to host memory
            (sync-memory-block out-r :device-to-host)
            (sync-memory-block out-g :device-to-host)
            (sync-memory-block out-b :device-to-host)
            (sync-memory-block out-shadow :device-to-host)
            (sync-memory-block out-direct-r :device-to-host)
            (sync-memory-block out-direct-g :device-to-host)
            (sync-memory-block out-direct-b :device-to-host)
            (sync-memory-block out-one-bounce-r :device-to-host)
            (sync-memory-block out-one-bounce-g :device-to-host)
            (sync-memory-block out-one-bounce-b :device-to-host)
            (when write-debug-images
              (sync-memory-block out-reflect-path-r :device-to-host)
              (sync-memory-block out-reflect-path-g :device-to-host)
              (sync-memory-block out-reflect-path-b :device-to-host)
              (sync-memory-block out-reflect-child-r :device-to-host)
              (sync-memory-block out-reflect-child-g :device-to-host)
              (sync-memory-block out-reflect-child-b :device-to-host)
              (sync-memory-block out-reflect-contrib-r :device-to-host)
              (sync-memory-block out-reflect-contrib-g :device-to-host)
              (sync-memory-block out-reflect-contrib-b :device-to-host)
              (sync-memory-block out-reflect-self-hit :device-to-host))
            (let* ((end-time (get-internal-real-time))
                   (elapsed (/ (float (- end-time start-time)) internal-time-units-per-second)))
              (format t "GPU Raytracing completed in ~,4F seconds.~%" elapsed)))
          
          (write-ppm output-file width height size out-r out-g out-b)
          (when write-debug-images
            (write-ppm "spheres_gpu_shadow-factor-debug.ppm" width height size
                       out-shadow out-shadow out-shadow)
            ;; A/B image for isolating recursive-reflection artifacts.  This is
            ;; the exact primary shading used by the production kernel, before
            ;; its first reflected contribution is added.
            (write-ppm "spheres_gpu_direct-debug.ppm" width height size
                       out-direct-r out-direct-g out-direct-b)
            (write-ppm "spheres_gpu_one-bounce-debug.ppm" width height size
                       out-one-bounce-r out-one-bounce-g out-one-bounce-b)
            (write-ppm "spheres_gpu_reflection-path-debug.ppm" width height size
                       out-reflect-path-r out-reflect-path-g out-reflect-path-b)
            (write-ppm "spheres_gpu_reflection-child-debug.ppm" width height size
                       out-reflect-child-r out-reflect-child-g out-reflect-child-b)
            (write-ppm "spheres_gpu_reflection-contribution-debug.ppm" width height size
                       out-reflect-contrib-r out-reflect-contrib-g out-reflect-contrib-b)
            (write-ppm "spheres_gpu_reflection-self-hit-debug.ppm" width height size
                       out-reflect-self-hit out-reflect-self-hit out-reflect-self-hit))
          (format t "Rendering Job Successful!~%"))))))

(defun run-gpu-animation (&key (frames 60) (res 8)
                                (frame-directory "frames_gpu"))
  "Render FRAMES GPU animation frames into FRAME-DIRECTORY.

The caller creates FRAME-DIRECTORY.  Keeping frame generation in one SBCL
process lets cl-cuda reuse its compiled kernel module; run.sh encodes the
resulting numbered PPM sequence to MP4 with FFmpeg."
  (unless (plusp frames)
    (error "FRAMES must be positive, got ~S." frames))
  (let ((directory (string-right-trim '(#\/) frame-directory)))
    (dotimes (frame frames)
      (format t "~&=== Rendering GPU animation frame ~D/~D ===~%"
              (1+ frame) frames)
      (run-gpu-raytracer
       :res res
       :frame frame
       :write-debug-images nil
       :output-file (format nil "~A/spheres_frame_~3,'0d.ppm"
                            directory frame)))))
