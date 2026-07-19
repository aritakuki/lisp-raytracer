;;;; gpu-output.lsp
;;;; Host-side conversion of GPU color buffers to a PPM image.

(in-package :gpu-raytracer)

(defun write-ppm (output-file width height size out-r out-g out-b)
  (format t "Saving PPM file to ~A...~%" output-file)
  (with-open-file (p output-file :direction :output :if-exists :supersede)
    (format p "P3~%~A ~A~%255~%" width height)
    (dotimes (i size)
      (let ((r (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-r i))))))
            (g (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-g i))))))
            (b (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-b i)))))))
        (format p "~D ~D ~D~%" r g b)))))

(defun write-ppm-with-pixel-marker (output-file width height size out-r out-g out-b
                                     marker-x marker-y)
  "Write a PPM image and draw a small yellow box over MARKER-X, MARKER-Y.

The marker identifies the representative pixel used by the explainer assets.
It is added only while saving the explanatory copy; the GPU color buffers stay
unchanged."
  (format t "Saving annotated PPM file to ~A...~%" output-file)
  (with-open-file (p output-file :direction :output :if-exists :supersede)
    (format p "P3~%~A ~A~%255~%" width height)
    (dotimes (i size)
      (let* ((x (mod i width))
             (y (floor i width))
             ;; A 25-pixel, four-pixel-thick outline stays visible after the
             ;; 800-pixel render is reduced for the video.
             (dx (abs (- x marker-x)))
             (dy (abs (- y marker-y)))
             (marked (and (<= dx 12) (<= dy 12)
                          (or (>= dx 9) (>= dy 9))))
             (r (if marked 255
                    (round (* 255.0f0 (max 0.0f0 (min 1.0f0
                                                        (memory-block-aref out-r i)))))))
             (g (if marked 220
                    (round (* 255.0f0 (max 0.0f0 (min 1.0f0
                                                        (memory-block-aref out-g i)))))))
             (b (if marked 0
                    (round (* 255.0f0 (max 0.0f0 (min 1.0f0
                                                        (memory-block-aref out-b i))))))))
        (format p "~D ~D ~D~%" r g b)))))

(defun write-ppm-completion-replay (output-file width height size out-r out-g out-b
                                     completion-rank completed-count)
  "Replay an instrumented kernel's actual per-pixel completion order.

COMPLETION-RANK is written by the GPU immediately after a thread stores its
final RGB value.  Pixels with a later rank remain black in this frame."
  (format t "Saving completion-order PPM file to ~A...~%" output-file)
  (with-open-file (p output-file :direction :output :if-exists :supersede)
    (format p "P3~%~A ~A~%255~%" width height)
    (dotimes (i size)
      (if (< (memory-block-aref completion-rank i) completed-count)
          (format p "~D ~D ~D~%"
                  (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-r i)))))
                  (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-g i)))))
                  (round (* 255.0f0 (max 0.0f0 (min 1.0f0 (memory-block-aref out-b i))))))
          (format p "10 18 28~%")))))

(defun verify-completion-log (completion-rank completion-counter size)
  "Reject a progress replay unless every instrumented GPU thread was recorded.

The end marker is an atomic counter increment made after the source-level
final RGB stores.  A valid run must report exactly SIZE markers and a
permutation of ranks from 0 through SIZE-1; otherwise no explanatory progress
image is trustworthy."
  (let ((reported-count (memory-block-aref completion-counter 0))
        (seen (make-array size :element-type 'bit :initial-element 0)))
    (unless (= reported-count size)
      (error "Completion log is incomplete: GPU reported ~D of ~D pixels."
             reported-count size))
    (dotimes (pixel-index size)
      (let ((rank (memory-block-aref completion-rank pixel-index)))
        (unless (and (integerp rank) (<= 0 rank) (< rank size))
          (error "Invalid completion rank ~S at pixel ~D." rank pixel-index))
        (when (= (sbit seen rank) 1)
          (error "Duplicate completion rank ~D in GPU measurement." rank))
        (setf (sbit seen rank) 1)))
    (format t "GPU completion log verified: ~D unique per-pixel end markers.~%" size)
    t))

(defun write-completion-log-report (directory size percentages)
  "Write provenance beside the replay frames so they cannot be mistaken for tiles."
  (with-open-file (stream (merge-pathnames "12-progress-measurement.txt"
                                           (pathname (concatenate 'string
                                                                  (string-right-trim '(#\/) directory)
                                                                  "/")))
                          :direction :output :if-exists :supersede)
    (format stream "source: one instrumented CUDA kernel launch~%")
    (format stream "event: atomic per-pixel end marker after source-level final RGB stores~%")
    (format stream "validated markers: ~D unique ranks (0 through ~D)~%" size (1- size))
    (format stream "replay percentages: ~{~D~^, ~}~%" percentages)))
