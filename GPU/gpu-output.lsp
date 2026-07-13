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
