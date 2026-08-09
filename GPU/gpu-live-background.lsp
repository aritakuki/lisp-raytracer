;;;; gpu-live-background.lsp
;;;; Continuous, file-free rendering for Monadius through anonymous shared RAM.

(load (merge-pathnames "gpu-package.lsp" *load-truename*))

(in-package :gpu-raytracer)

(defvar *live-shared-library-loaded* nil)

(cffi:defcfun ("monadiusSharedAttach" %shared-attach) :pointer
  (file-descriptor :int))
(cffi:defcfun ("monadiusSharedWidth" %shared-width) :int
  (context :pointer))
(cffi:defcfun ("monadiusSharedHeight" %shared-height) :int
  (context :pointer))
(cffi:defcfun ("monadiusSharedShouldStop" %shared-should-stop) :int
  (context :pointer))
(cffi:defcfun ("monadiusSharedPublishRgb" %shared-publish-rgb) :int
  (context :pointer)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (width :int)
  (height :int))
(cffi:defcfun ("monadiusSharedFail" %shared-fail) :void
  (context :pointer)
  (error-code :int))
(cffi:defcfun ("monadiusSharedClose" %shared-close) :void
  (context :pointer))

(defun %load-live-shared-library ()
  (unless *live-shared-library-loaded*
    (let ((library (sb-ext:posix-getenv "MONADIUS_RAY_SHARED_LIBRARY")))
      (unless (and library (plusp (length library)))
        (error "MONADIUS_RAY_SHARED_LIBRARY is not set."))
      (cffi:load-foreign-library library)
      (setf *live-shared-library-loaded* t))))

(defun %configure-live-cuda-cache ()
  (let ((raw-directory (sb-ext:posix-getenv "MONADIUS_RAY_CUDA_CACHE")))
    (when (and raw-directory (plusp (length raw-directory)))
      (let ((directory
              (if (char= (char raw-directory (1- (length raw-directory))) #\/)
                  raw-directory
                  (concatenate 'string raw-directory "/"))))
        (ensure-directories-exist (concatenate 'string directory "placeholder"))
        (setf cl-cuda.api.nvcc:*tmp-path* directory)))))

(defun %make-live-sphere-data (frame)
  "Create the same animated scene used by RUN-GPU-ANIMATION for FRAME."
  (unless (boundp 'cl-user::*cpu-init-random-state*)
    (setf cl-user::*cpu-init-random-state* (make-random-state nil)))
  (setf *random-state*
        (make-random-state cl-user::*cpu-init-random-state*))
  (let ((sphere-data nil))
    (let* ((frame-f (float frame 1.0f0))
           (animation-time (* frame-f (/ 1.0f0 60.0f0)))
           (red-x (+ (* 170.0f0 (sin (* animation-time 0.85f0)))
                     (* 45.0f0
                        (sin (+ (* animation-time 1.90f0) 0.40f0)))))
           (red-y (+ -300.0f0
                     (* 55.0f0 (cos (* animation-time 0.55f0)))))
           (red-z (+ -1200.0f0
                     (* 60.0f0 (sin (* animation-time 0.40f0)))))
           (green-x (+ -80.0f0
                       (* 80.0f0
                          (cos (+ (* animation-time 0.38f0) 0.20f0)))))
           (green-y (+ -150.0f0
                       (* 120.0f0
                          (sin (+ (* animation-time 0.72f0) 0.30f0)))))
           (green-z (+ -1200.0f0
                       (* 110.0f0
                          (sin (+ (* animation-time 0.47f0) 0.90f0)))))
           (blue-x (+ 70.0f0
                      (* 130.0f0 (sin (* animation-time 0.51f0)))))
           (blue-y (+ -100.0f0
                      (* 70.0f0
                         (sin (+ (* animation-time 1.13f0) 0.80f0)))))
           (blue-z (+ -1200.0f0
                      (* 170.0f0 (cos (* animation-time 0.31f0))))))
      (push (list red-x red-y red-z
                  200.0f0 0.8f0 0.2f0 0.2f0 0.2f0 1.0f0)
            sphere-data)
      (push (list green-x green-y green-z
                  200.0f0 0.2f0 0.8f0 0.2f0 0.2f0 1.1f0)
            sphere-data)
      (push (list blue-x blue-y blue-z
                  200.0f0 0.2f0 0.2f0 0.9f0 0.2f0 1.0f0)
            sphere-data))
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
                      (if (> (mod small-sphere-index 3) 1)
                          1.5f0
                          1.0f0))
                sphere-data)
          (incf small-sphere-index))))
    sphere-data))

(defun run-gpu-live-background (&key shared-context width height)
  "Continuously publish complete animated frames through SHARED-CONTEXT.

CUDA context, kernel module, and memory blocks live for the whole loop. The
Haskell process keeps drawing its previous texture while this function is
calculating the next frame."
  (unless (and shared-context (not (cffi:null-pointer-p shared-context)))
    (error "A mapped Monadius shared-memory context is required."))
  (unless (and (plusp width) (plusp height))
    (error "Live background dimensions must be positive, got ~Dx~D."
           width height))
  (%configure-live-cuda-cache)
  (let* ((size (* width height))
         (width-f (float width 1.0f0))
         (height-f (float height 1.0f0))
         (eye-x 550.0f0) (eye-y -380.0f0) (eye-z 650.0f0)
         (look-x 0.0f0) (look-y 160.0f0) (look-z -1200.0f0)
         (up-x 0.0f0) (up-y -1.0f0) (up-z 0.0f0)
         (fov 28.0f0)
         (fx-raw (- look-x eye-x))
         (fy-raw (- look-y eye-y))
         (fz-raw (- look-z eye-z))
         (flen (sqrt (+ (* fx-raw fx-raw)
                        (* fy-raw fy-raw)
                        (* fz-raw fz-raw))))
         (fx (/ fx-raw flen))
         (fy (/ fy-raw flen))
         (fz (/ fz-raw flen))
         (rx-raw (- (* fy up-z) (* fz up-y)))
         (ry-raw (- (* fz up-x) (* fx up-z)))
         (rz-raw (- (* fx up-y) (* fy up-x)))
         (rlen (sqrt (+ (* rx-raw rx-raw)
                        (* ry-raw ry-raw)
                        (* rz-raw rz-raw))))
         (aspect (/ width-f height-f))
         (rx (* (/ rx-raw rlen) aspect))
         (ry (* (/ ry-raw rlen) aspect))
         (rz (* (/ rz-raw rlen) aspect))
         (unit-rx (/ rx aspect))
         (unit-ry (/ ry aspect))
         (unit-rz (/ rz aspect))
         (ux (- (* unit-ry fz) (* unit-rz fy)))
         (uy (- (* unit-rz fx) (* unit-rx fz)))
         (uz (- (* unit-rx fy) (* unit-ry fx)))
         (ulen (sqrt (+ (* ux ux) (* uy uy) (* uz uz))))
         (ux (/ ux ulen))
         (uy (/ uy ulen))
         (uz (/ uz ulen))
         (scale (float
                 (tan (* 0.5f0 (/ (* fov 3.14159265f0) 180.0f0)))
                 1.0f0))
         (ray1-x (+ fx (* ux scale)))
         (ray1-y (+ fy (* uy scale)))
         (ray1-z (+ fz (* uz scale)))
         (ray1-len (sqrt (+ (* ray1-x ray1-x)
                            (* ray1-y ray1-y)
                            (* ray1-z ray1-z))))
         (yr1 (/ ray1-y ray1-len))
         (ray2-x (- fx (* ux scale)))
         (ray2-y (- fy (* uy scale)))
         (ray2-z (- fz (* uz scale)))
         (ray2-len (sqrt (+ (* ray2-x ray2-x)
                            (* ray2-y ray2-y)
                            (* ray2-z ray2-z))))
         (yr2 (/ ray2-y ray2-len))
         (sky-yr-min (float (min yr1 yr2) 1.0f0))
         (sky-yr-max (float (max yr1 yr2) 1.0f0))
         (initial-scene (%make-live-sphere-data 0))
         (num-spheres (length initial-scene))
         (block-x 16)
         (block-y 16)
         (grid-x (ceiling width block-x))
         (grid-y (ceiling height block-y)))
    (format t "~&Starting live CUDA background at ~Dx~D.~%" width height)
    (finish-output)
    (with-cuda (0)
      (with-memory-blocks
          ((out-r 'cl-cuda:float size)
           (out-g 'cl-cuda:float size)
           (out-b 'cl-cuda:float size)
           (out-shadow 'cl-cuda:float size)
           (out-direct-r 'cl-cuda:float size)
           (out-direct-g 'cl-cuda:float size)
           (out-direct-b 'cl-cuda:float size)
           (out-one-bounce-r 'cl-cuda:float size)
           (out-one-bounce-g 'cl-cuda:float size)
           (out-one-bounce-b 'cl-cuda:float size)
           (out-completion-rank 'cl-cuda:int size)
           (completion-counter 'cl-cuda:int 1)
           (sph-cx 'cl-cuda:float num-spheres)
           (sph-cy 'cl-cuda:float num-spheres)
           (sph-cz 'cl-cuda:float num-spheres)
           (sph-r 'cl-cuda:float num-spheres)
           (sph-col-r 'cl-cuda:float num-spheres)
           (sph-col-g 'cl-cuda:float num-spheres)
           (sph-col-b 'cl-cuda:float num-spheres)
           (sph-refl 'cl-cuda:float num-spheres)
           (sph-ior 'cl-cuda:float num-spheres))
        (labels ((upload-scene (frame)
                   (let ((index 0))
                     (dolist (sphere (%make-live-sphere-data frame))
                       (setf (memory-block-aref sph-cx index) (nth 0 sphere)
                             (memory-block-aref sph-cy index) (nth 1 sphere)
                             (memory-block-aref sph-cz index) (nth 2 sphere)
                             (memory-block-aref sph-r index) (nth 3 sphere)
                             (memory-block-aref sph-col-r index) (nth 4 sphere)
                             (memory-block-aref sph-col-g index) (nth 5 sphere)
                             (memory-block-aref sph-col-b index) (nth 6 sphere)
                             (memory-block-aref sph-refl index) (nth 7 sphere)
                             (memory-block-aref sph-ior index) (nth 8 sphere))
                       (incf index))
                     (unless (= index num-spheres)
                       (error "Live scene sphere count changed from ~D to ~D."
                              num-spheres index)))
                   (sync-memory-block sph-cx :host-to-device)
                   (sync-memory-block sph-cy :host-to-device)
                   (sync-memory-block sph-cz :host-to-device)
                   (sync-memory-block sph-r :host-to-device)
                   (sync-memory-block sph-col-r :host-to-device)
                   (sync-memory-block sph-col-g :host-to-device)
                   (sync-memory-block sph-col-b :host-to-device)
                   (sync-memory-block sph-refl :host-to-device)
                   (sync-memory-block sph-ior :host-to-device)))
          (setf (memory-block-aref completion-counter 0) 0)
          (sync-memory-block completion-counter :host-to-device)
          (loop for frame from 0
                until (plusp (%shared-should-stop shared-context))
                do (upload-scene frame)
                   (raytrace-kernel-v12
                    out-r out-g out-b out-shadow
                    out-direct-r out-direct-g out-direct-b
                    out-one-bounce-r out-one-bounce-g out-one-bounce-b
                    out-completion-rank completion-counter
                    0
                    width height 0 0
                    width-f height-f
                    sph-cx sph-cy sph-cz sph-r
                    sph-col-r sph-col-g sph-col-b sph-refl sph-ior
                    num-spheres
                    eye-x eye-y eye-z
                    fx fy fz
                    rx ry rz
                    ux uy uz
                    scale
                    sky-yr-min sky-yr-max
                    :grid-dim (list grid-x grid-y 1)
                    :block-dim (list block-x block-y 1))
                   (sync-memory-block out-r :device-to-host)
                   (sync-memory-block out-g :device-to-host)
                   (sync-memory-block out-b :device-to-host)
                   (unless (plusp
                            (%shared-publish-rgb
                             shared-context
                             (memory-block-host-ptr out-r)
                             (memory-block-host-ptr out-g)
                             (memory-block-host-ptr out-b)
                             width height))
                     (error "The shared-memory bridge rejected a completed frame."))
                   (when (or (zerop frame)
                             (zerop (mod (1+ frame) 30)))
                     (format t "Live CUDA background published frame ~D.~%"
                             (1+ frame))
                     (finish-output))))))
    (format t "Live CUDA background stopped between frames.~%")))

(defun run-gpu-live-background-process ()
  "Attach the inherited memfd and run the standalone Lisp producer process."
  (%load-live-shared-library)
  (let* ((descriptor-text
           (sb-ext:posix-getenv "MONADIUS_RAY_SHARED_FD"))
         (descriptor
           (and descriptor-text
                (ignore-errors (parse-integer descriptor-text :junk-allowed nil))))
         (context
           (and descriptor (%shared-attach descriptor))))
    (unless (and context (not (cffi:null-pointer-p context)))
      (error "Could not attach the inherited Monadius shared memory."))
    (unwind-protect
         (handler-case
             (run-gpu-live-background
              :shared-context context
              :width (%shared-width context)
              :height (%shared-height context))
           (error (condition)
             (format *error-output* "Live CUDA background failed: ~A~%"
                     condition)
             (finish-output *error-output*)
             (%shared-fail context 1)
             (error condition)))
      (%shared-close context))))
