;;;; gpu-explainer.lsp
;;;; Educational assets built from the diagnostic buffers of the GPU renderer.

(in-package :gpu-raytracer)

;; Defined by modules loaded before/after this one from GPU/gpu-main.lsp.
(declaim (ftype function write-ppm write-ppm-with-pixel-marker
                          run-gpu-raytracer))

(defparameter *explainer-stages*
  '(("00-title.ppm" "Common Lisp と GPUで学ぶ 趣味のレイトレーシング")
    ("01-intro.ppm" "レイトレーサーとは")
    ("02-scene.ppm" "まず、Lisp（CPU側）がシミュレーション空間にカメラ・光源・空・床・球を配置します。
位置・半径・色・材質の値を配列にしてGPUへ送ります。")
    ("03-ray-direction.ppm" "光源は実際には全方向へ光を出します。
橙は L→H→C の代表経路。GPUはこれをカメラ側から逆向きに追跡します。")
    ("04-pixel.ppm" "これはGPUが出力する完成画像です。黄色い小枠の1マスが、これから追う1画素です。
この1画素の色を求めるために、カメラからレイを出します。")
    ("05-primary-ray.ppm" "選んだ1画素の色を調べるため、カメラから最初に出すレイ（主レイ）です。
最初に当たった表面Hを見つけ、そこで色の計算を始めます。")
    ("06-shadow-ray.ppm" "表面Hから光源へ、光が届くかを調べるレイ（影レイ）です。
途中に物体があれば、その光は表面Hには届かず、影になります。")
    ("07-local-shading.ppm" "直接照明は、光源から表面Hへ届く明るさです。
その明るさと材質の色・面の向き・影レイの結果が、この画素の色を決めます。")
    ("08-reflection-ray.ppm" "反射する物体では、表面Hから反射した先を調べるレイ（反射レイ）を出します。
そこで見つけた色も足して、この画素の最終RGBを決めます。")
    ("09-grid.ppm" "このプログラムでは、16×16画素を一組にし、その組をブロックと呼びます。
画面全体を一度に渡すのではなく、ブロック単位に分けてGPUへ仕事を渡します。")
    ("10-thread-flow.ppm" "ブロック内の各画素は、スレッドという計算担当に渡されます。
1スレッドが、1画素の色を最初のレイから最終RGBまで計算します。")
    ("11-scheduling.ppm" "800×800画素を、16×16画素の2500ブロックに分けます。
SM（Streaming Multiprocessor）は、ブロックのスレッドを実行する装置です。")
    ("12-progress-01.ppm" "ここから、Lispが配置したカメラ・空・球・床・光源をGPUが計算します。
開始直後は、最終色を出し終えた画素がまだごく一部です。")
    ("12-progress-02.ppm" "先に終えた画素では、Lispが指定した空・球・床の色が現れます。
影レイで光が遮られた場所は、同じ材質でも暗くなります。")
    ("12-progress-03.ppm" "各画素では、主レイ（カメラから最初に出すレイ）で表面を見つけ、影レイで光の遮りを調べます。
反射する球では反射レイの結果も加え、表面に周囲の色や明るさを映します。")
    ("12-progress-04.ppm" "完了した画素が増えるにつれ、空・球・床・影の形が見えてきます。
色の位置は、Lispが配置した物体と光源から計算された結果です。")
    ("12-progress-05.ppm" "全画素の計算が終わり、Lispが置いたシーン全体が画像になりました。
この完成したRGB配列をGPUからCPUへコピーして保存します。")
    ("14-transfer.ppm" "PPM（Portable Pixmap）は、画像の各画素の色をR・G・Bの数値として保存する
シンプルな画像ファイルです。GPUが計算した最終RGB配列をCPUへコピーして保存します。")
    ("15-final.ppm" "全画素の最終RGB値がそろった完成画像です。
1回のGPUカーネル実行で計算した結果を、画像ファイルに保存しました。")))

(defun %explainer-directory (directory)
  (make-pathname :name nil :type nil
                 :defaults (pathname (concatenate 'string
                                                   (string-right-trim '(#\/) directory)
                                                   "/"))))

(defun %explainer-path (directory filename)
  (merge-pathnames filename (%explainer-directory directory)))

(defun %ray-sphere-hit (ox oy oz dx dy dz sphere)
  (let* ((cx (first sphere)) (cy (second sphere)) (cz (third sphere))
         (radius (fourth sphere))
         (vx (- ox cx)) (vy (- oy cy)) (vz (- oz cz))
         (b (+ (* vx dx) (* vy dy) (* vz dz)))
         (c (- (+ (* vx vx) (* vy vy) (* vz vz)) (* radius radius)))
         (disc (- (* b b) c)))
    (when (>= disc 0.0f0)
      (let* ((root (sqrt disc))
             (near (- (- b) root))
             (far (+ (- b) root))
             (hit-t (cond ((> near 0.001f0) near)
                          ((> far 0.001f0) far))))
        hit-t))))

(defun %ray-plane-hit (ox oy oz dx dy dz)
  (when (> (abs dy) 1.0f-8)
    (let ((hit-t (/ (- 500.0f0 oy) dy)))
      (when (> hit-t 0.001f0)
        (let ((hit-x (+ ox (* hit-t dx)))
              (hit-z (+ oz (* hit-t dz))))
          (when (and (<= (abs hit-x) 2500.0f0)
                     (<= (abs (- hit-z -1400.0f0)) 2500.0f0))
            hit-t))))))

(defun %first-explainer-hit (ox oy oz dx dy dz sphere-data)
  (let ((best-t most-positive-single-float)
        (best-type nil)
        (best-index -1))
    (loop for sphere in sphere-data
          for index from 0
          for hit-t = (%ray-sphere-hit ox oy oz dx dy dz sphere)
          when (and hit-t (< hit-t best-t))
            do (setf best-t hit-t best-type :sphere best-index index))
    (let ((plane-t (%ray-plane-hit ox oy oz dx dy dz)))
      (when (and plane-t (< plane-t best-t))
        (setf best-t plane-t best-type :plane best-index -1)))
    (when best-type
      (list best-type best-index best-t))))

(defun %hit-point (ox oy oz dx dy dz hit-t)
  (list (+ ox (* hit-t dx)) (+ oy (* hit-t dy)) (+ oz (* hit-t dz))))

(defun %hit-normal (hit sphere-data point)
  (if (eq (first hit) :plane)
      (list 0.0f0 -1.0f0 0.0f0)
      (let* ((sphere (nth (second hit) sphere-data))
             (radius (fourth sphere)))
        (list (/ (- (first point) (first sphere)) radius)
              (/ (- (second point) (second sphere)) radius)
              (/ (- (third point) (third sphere)) radius)))))

(defun %reflected-direction (dx dy dz normal)
  (let* ((dot (+ (* dx (first normal)) (* dy (second normal)) (* dz (third normal))))
         (rx (- dx (* 2.0f0 dot (first normal))))
         (ry (- dy (* 2.0f0 dot (second normal))))
         (rz (- dz (* 2.0f0 dot (third normal))))
         (length (sqrt (+ (* rx rx) (* ry ry) (* rz rz)))))
    (list (/ rx length) (/ ry length) (/ rz length))))

(defun %svg-x (x) (+ 70.0f0 (* (+ x 2500.0f0) 0.15f0)))
(defun %svg-z (z) (+ 40.0f0 (* (- 650.0f0 z) 0.12f0)))

;; Coordinates for the full-scene top-down overview.  The lower part of every
;; SVG is intentionally left empty for the subtitle band added by FFmpeg.
(defun %overview-x (x) (+ 120.0f0 (* (/ (+ x 2500.0f0) 5000.0f0) 1040.0f0)))
(defun %overview-z (z) (+ 100.0f0 (* (/ (- 1100.0f0 z) 5000.0f0) 330.0f0)))

(defun %svg-rgb (r g b)
  (flet ((channel (value)
           (round (* 255 (max 0.0 (min 1.0 value))))))
    (format nil "#~2,'0X~2,'0X~2,'0X" (channel r) (channel g) (channel b))))

(defun write-explainer-ray-diagram (directory marker-x marker-y width height
                                     eye-x eye-y eye-z fx fy fz rx ry rz ux uy uz
                                     scale sphere-data)
  "Record and draw the primary and first reflected ray of one representative pixel.

The diagram is a top-down X/Z view.  It deliberately uses the same scene data
and camera basis supplied to the CUDA kernel, so it explains the work rather
than becoming a separate illustrative scene."
  (ensure-directories-exist (%explainer-path directory "placeholder"))
  (let* ((sx (- (* 2.0f0 (/ (+ marker-x 0.5f0) width)) 1.0f0))
         (sy (- 1.0f0 (* 2.0f0 (/ (+ marker-y 0.5f0) height))))
         (raw-x (+ fx (* rx sx scale) (* ux sy scale)))
         (raw-y (+ fy (* ry sx scale) (* uy sy scale)))
         (raw-z (+ fz (* rz sx scale) (* uz sy scale)))
         (raw-length (sqrt (+ (* raw-x raw-x) (* raw-y raw-y) (* raw-z raw-z))))
         (dx (/ raw-x raw-length)) (dy (/ raw-y raw-length)) (dz (/ raw-z raw-length))
         (primary (%first-explainer-hit eye-x eye-y eye-z dx dy dz sphere-data))
         (primary-point (and primary (%hit-point eye-x eye-y eye-z dx dy dz (third primary))))
         (normal (and primary (%hit-normal primary sphere-data primary-point)))
         (reflect-dir (and normal (%reflected-direction dx dy dz normal)))
         ;; Offset the secondary ray exactly as the production kernel does.
         (reflect-origin (and primary-point normal
                              (list (+ (first primary-point) (* (first normal) 0.05f0))
                                    (+ (second primary-point) (* (second normal) 0.05f0))
                                    (+ (third primary-point) (* (third normal) 0.05f0)))))
         (secondary (and reflect-origin reflect-dir
                         (%first-explainer-hit (first reflect-origin) (second reflect-origin) (third reflect-origin)
                                               (first reflect-dir) (second reflect-dir) (third reflect-dir)
                                               sphere-data)))
         (secondary-point (and secondary reflect-origin reflect-dir
                               (%hit-point (first reflect-origin) (second reflect-origin) (third reflect-origin)
                                           (first reflect-dir) (second reflect-dir) (third reflect-dir)
                                           (third secondary))))
         ;; Draw a long enough segment to make the direction intelligible.
         ;; The diagnostic text above still records the actual first hit.
         (reflection-end-x (if reflect-origin
                               (+ (first reflect-origin) (* (first reflect-dir) 700.0f0))
                               eye-x))
         (reflection-end-z (if reflect-origin
                               (+ (third reflect-origin) (* (third reflect-dir) 700.0f0))
                               eye-z))
         (sphere-color (if (and primary (eq (first primary) :sphere))
                           (let ((sphere (nth (second primary) sphere-data)))
                             (%svg-rgb (fifth sphere) (sixth sphere) (seventh sphere)))
                           "#8a96a3")))
    (with-open-file (stream (%explainer-path directory "representative-ray.txt")
                            :direction :output :if-exists :supersede)
      (format stream "Representative pixel: (~D, ~D) of ~Dx~D~%" marker-x marker-y width height)
      (format stream "Primary ray origin: ~,3F ~,3F ~,3F~%" eye-x eye-y eye-z)
      (format stream "Primary ray direction: ~,6F ~,6F ~,6F~%" dx dy dz)
      (if primary
          (format stream "Primary hit: ~A #~D at t=~,3F, point ~{~,3F~^ ~}~%"
                  (first primary) (second primary) (third primary) primary-point)
          (format stream "Primary hit: sky~%"))
      (if secondary
          (format stream "First reflected hit: ~A #~D at t=~,3F, point ~{~,3F~^ ~}~%"
                  (first secondary) (second secondary) (third secondary) secondary-point)
          (when primary (format stream "First reflected hit: sky~%"))))
    (let* ((hit-x (if primary-point (first primary-point) eye-x))
           (hit-z (if primary-point (third primary-point) eye-z))
           ;; The right-hand drawing is a true zoom of the same X/Z plane,
           ;; with one shared scale for X and Z.  It is not a rearranged
           ;; explanatory scene.
           (zoom-min-x (- (min eye-x 600.0f0 hit-x) 350.0f0))
           (zoom-max-x (+ (max eye-x 600.0f0 hit-x) 350.0f0))
           (zoom-min-z (- (min eye-z 200.0f0 hit-z) 350.0f0))
           (zoom-max-z (+ (max eye-z 200.0f0 hit-z) 350.0f0))
           (zoom-scale (min (/ 640.0f0 (- zoom-max-x zoom-min-x))
                            (/ 270.0f0 (- zoom-max-z zoom-min-z))))
           (zoom-width (* (- zoom-max-x zoom-min-x) zoom-scale))
           (zoom-origin-x (+ 535.0f0 (/ (- 680.0f0 zoom-width) 2.0f0)))
           (hit-sphere (and primary (eq (first primary) :sphere)
                            (nth (second primary) sphere-data))))
      (labels ((svg-header (stream title)
               (declare (ignore title))
               (format stream "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1280\" height=\"720\" viewBox=\"0 0 1280 720\">~%")
               (format stream "<rect width=\"1280\" height=\"720\" fill=\"#101820\"/><defs><marker id=\"arrow-orange\" markerWidth=\"7\" markerHeight=\"7\" refX=\"6\" refY=\"2.1\" orient=\"auto\"><path d=\"M0,0 L0,4.2 L6,2.1 z\" fill=\"#ff9f1c\"/></marker><marker id=\"arrow-blue\" markerWidth=\"7\" markerHeight=\"7\" refX=\"6\" refY=\"2.1\" orient=\"auto\"><path d=\"M0,0 L0,4.2 L6,2.1 z\" fill=\"#52b7ff\"/></marker><marker id=\"arrow-green\" markerWidth=\"7\" markerHeight=\"7\" refX=\"6\" refY=\"2.1\" orient=\"auto\"><path d=\"M0,0 L0,4.2 L6,2.1 z\" fill=\"#7ee787\"/></marker><marker id=\"arrow-purple\" markerWidth=\"7\" markerHeight=\"7\" refX=\"6\" refY=\"2.1\" orient=\"auto\"><path d=\"M0,0 L0,4.2 L6,2.1 z\" fill=\"#c084fc\"/></marker></defs>~%")
               ;; Keep a small, deliberate gap below the top subtitles.
               (format stream "<g transform=\"translate(0 115)\" font-family=\"Noto Sans CJK JP, sans-serif\">~%"))
             (write-intro (filename)
               (with-open-file (stream (%explainer-path directory filename) :direction :output :if-exists :supersede)
                 (svg-header stream "レイトレーサーとは")
                 (format stream "<g fill=\"#ffffff\" font-size=\"30\"><text x=\"110\" y=\"100\">レイトレーサーは、カメラに入る光の経路を逆向きに追跡し、</text><text x=\"110\" y=\"145\">物体・光・反射を計算して3D画像を作ります。</text></g><text x=\"110\" y=\"210\" fill=\"#ffcf70\" font-size=\"32\">レイ（光線）をトレース（追跡）する者。だから、レイトレーサーです。</text><text x=\"110\" y=\"270\" fill=\"#a7f3b1\" font-size=\"27\">今回は、Common Lispが設定した情報をもとに、GPUが画素を並列に計算します。</text><g fill=\"#1b2c3d\" stroke=\"#52b7ff\" stroke-width=\"4\"><rect x=\"110\" y=\"320\" width=\"180\" height=\"105\" rx=\"18\"/><rect x=\"490\" y=\"320\" width=\"260\" height=\"105\" rx=\"18\"/><rect x=\"950\" y=\"320\" width=\"180\" height=\"105\" rx=\"18\"/></g><circle cx=\"165\" cy=\"372\" r=\"23\" fill=\"#e8eef6\"/><text x=\"205\" y=\"382\" fill=\"white\" font-size=\"26\">カメラ</text><path d=\"M305,372 H470\" stroke=\"#52b7ff\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-blue)\"/><text x=\"350\" y=\"350\" fill=\"#8ed0ff\" font-size=\"24\">レイ</text><circle cx=\"565\" cy=\"372\" r=\"35\" fill=\"#32cd32\"/><path d=\"M600,410 H720\" stroke=\"#8a96a3\" stroke-width=\"10\"/><text x=\"610\" y=\"350\" fill=\"white\" font-size=\"24\">空・球・床</text><path d=\"M765,372 H930\" stroke=\"#ff9f1c\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><text x=\"990\" y=\"382\" fill=\"#a7f3b1\" font-size=\"30\">RGB</text><g><rect x=\"980\" y=\"395\" width=\"38\" height=\"24\" fill=\"#f55\"/><rect x=\"1023\" y=\"395\" width=\"38\" height=\"24\" fill=\"#5f5\"/><rect x=\"1066\" y=\"395\" width=\"38\" height=\"24\" fill=\"#55f\"/></g></g></svg>~%")))
             (zoom-x (x) (+ zoom-origin-x (* (- x zoom-min-x) zoom-scale)))
             (zoom-y (z) (+ 145.0f0 (* (- zoom-max-z z) zoom-scale)))
             (write-world (filename title mode)
               (with-open-file (stream (%explainer-path directory filename) :direction :output :if-exists :supersede)
                 (svg-header stream title)
                 ;; Every diagram below uses the same GPU scene coordinates.
                 ;; Keep the lower third empty: FFmpeg places subtitles there.
                 (format stream "<rect x=\"120\" y=\"100\" width=\"1040\" height=\"330\" fill=\"#555b66\"/><text x=\"1060\" y=\"410\" fill=\"white\" font-size=\"25\">空・床</text>~%")
                 (loop for sphere in sphere-data
                       do (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"~,1F\" fill=\"~A\" stroke=\"white\" stroke-width=\"1\"/>~%"
                                  (%overview-x (first sphere)) (%overview-z (third sphere))
                                  (* (fourth sphere) 0.208f0)
                                  (%svg-rgb (fifth sphere) (sixth sphere) (seventh sphere))))
                 (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"12\" fill=\"white\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"white\" font-size=\"24\">カメラ</text>~%"
                         (%overview-x eye-x) (%overview-z eye-z) (+ 18 (%overview-x eye-x)) (%overview-z eye-z))
                 (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"14\" fill=\"#ff9f1c\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#ffcf70\" font-size=\"24\">光源</text>~%"
                         (%overview-x 600.0f0) (%overview-z 200.0f0) (+ 20 (%overview-x 600.0f0)) (%overview-z 200.0f0))
                 (when (eq mode :physical)
                   (let ((lx (%overview-x 600.0f0)) (lz (%overview-z 200.0f0)))
                     (format stream "<g stroke=\"#ff9f1c\" stroke-width=\"4\" opacity=\"0.8\" fill=\"none\" marker-end=\"url(#arrow-orange)\"><path d=\"M~,1F,~,1F L~,1F,~,1F\"/><path d=\"M~,1F,~,1F L~,1F,~,1F\"/><path d=\"M~,1F,~,1F L~,1F,~,1F\"/><path d=\"M~,1F,~,1F L~,1F,~,1F\"/></g><text x=\"~,1F\" y=\"~,1F\" fill=\"#ffcf70\" font-size=\"25\">光は全方向へ広がる</text>~%"
                             lx lz (- lx 145) (- lz 70) lx lz (+ lx 145) (- lz 70)
                             lx lz (- lx 145) (+ lz 90) lx lz (+ lx 145) (+ lz 90)
                             (- lx 170) (- lz 90))))
                 (when (eq mode :primary)
                   (when primary-point
                     (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#52b7ff\" stroke-width=\"7\" fill=\"none\" marker-end=\"url(#arrow-blue)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#8ed0ff\" font-size=\"22\">カメラから最初に出すレイ（主レイ）</text>~%"
                             (%overview-x eye-x) (%overview-z eye-z)
                             (%overview-x (first primary-point)) (%overview-z (third primary-point))
                             (/ (+ (%overview-x eye-x) (%overview-x (first primary-point))) 2)
                             (- (/ (+ (%overview-z eye-z) (%overview-z (third primary-point))) 2) 15))))
                 (when (member mode '(:shadow :local))
                   (when primary-point
                     (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#7ee787\" stroke-width=\"6\" fill=\"none\" stroke-dasharray=\"12 8\" marker-end=\"url(#arrow-green)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#a7f3b1\" font-size=\"22\">光が届くか調べるレイ（影レイ）</text>~%"
                             (%overview-x (first primary-point)) (%overview-z (third primary-point))
                             (%overview-x 600.0f0) (%overview-z 200.0f0)
                             (/ (+ (%overview-x (first primary-point)) (%overview-x 600.0f0)) 2)
                             (- (/ (+ (%overview-z (third primary-point)) (%overview-z 200.0f0)) 2) 18))))
                 (when (eq mode :local)
                   (format stream "<text x=\"150\" y=\"90\" fill=\"#ffffff\" font-size=\"25\">直接照明: 光源から直接届く明るさ</text>~%"))
                 (when (eq mode :reflection)
                   (when primary-point
                     (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#c084fc\" stroke-width=\"6\" fill=\"none\" stroke-dasharray=\"12 8\" marker-end=\"url(#arrow-purple)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#d8b4fe\" font-size=\"22\">反射した先を調べるレイ（反射レイ）</text>~%"
                             (%overview-x hit-x) (%overview-z hit-z)
                             (%overview-x reflection-end-x) (%overview-z reflection-end-z)
                             (/ (+ (%overview-x hit-x) (%overview-x reflection-end-x)) 2)
                             (- (/ (+ (%overview-z hit-z) (%overview-z reflection-end-z)) 2) 16))))
                 (format stream "</g></svg>~%")))
             (write-explanation-panel (filename title mode)
               (with-open-file (stream (%explainer-path directory filename) :direction :output :if-exists :supersede)
                 (svg-header stream title)
                 ;; Left panel: actual X/Z coordinates, always kept visible.
                 (format stream "<rect x=\"45\" y=\"95\" width=\"410\" height=\"345\" fill=\"#555b66\"/><text x=\"60\" y=\"125\" fill=\"white\" font-size=\"21\">実際の配置（上から）</text>~%")
                 (loop for sphere in sphere-data
                       do (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"~,1F\" fill=\"~A\" stroke=\"white\" stroke-width=\"1\"/>~%"
                                  (+ 55 (* (- (%overview-x (first sphere)) 120) 0.36))
                                  (+ 105 (* (- (%overview-z (third sphere)) 100) 0.80))
                                  (max 3.0f0 (* (fourth sphere) 0.075f0))
                                  (%svg-rgb (fifth sphere) (sixth sphere) (seventh sphere))))
                 (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"10\" fill=\"white\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"white\" font-size=\"21\">C: カメラ</text>~%"
                         (+ 55 (* (- (%overview-x eye-x) 120) 0.36)) (+ 105 (* (- (%overview-z eye-z) 100) 0.80))
                         (+ 68 (* (- (%overview-x eye-x) 120) 0.36)) (+ 105 (* (- (%overview-z eye-z) 100) 0.80)))
                 (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"11\" fill=\"#ff9f1c\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#ffcf70\" font-size=\"21\">L: 光源</text>~%"
                         (+ 55 (* (- (%overview-x 600.0f0) 120) 0.36)) (+ 105 (* (- (%overview-z 200.0f0) 100) 0.80))
                         (+ 70 (* (- (%overview-x 600.0f0) 120) 0.36)) (+ 105 (* (- (%overview-z 200.0f0) 100) 0.80)))
                 (when primary-point
                   (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"11\" fill=\"none\" stroke=\"#52b7ff\" stroke-width=\"4\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#8ed0ff\" font-size=\"21\">H: 当たり点</text>~%"
                           (+ 55 (* (- (%overview-x (first primary-point)) 120) 0.36)) (+ 105 (* (- (%overview-z (third primary-point)) 100) 0.80))
                           (+ 70 (* (- (%overview-x (first primary-point)) 120) 0.36)) (+ 105 (* (- (%overview-z (third primary-point)) 100) 0.80))))
                 ;; Right panel: a true X/Z zoom.  X and Z share one scale.
                 (format stream "<rect x=\"500\" y=\"95\" width=\"735\" height=\"345\" rx=\"18\" fill=\"#1b2c3d\"/><text x=\"530\" y=\"125\" fill=\"white\" font-size=\"23\">同じ実座標を拡大（上から）</text>~%")
                 (when hit-sphere
                   (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"~,1F\" fill=\"~A\" stroke=\"white\" stroke-width=\"2\"/>~%"
                           (zoom-x (first hit-sphere)) (zoom-y (third hit-sphere))
                           (* (fourth hit-sphere) zoom-scale) sphere-color))
                 (when (eq mode :physical)
                   (loop for degrees from 0 below 360 by 30
                         for angle = (* degrees (/ pi 180.0f0))
                         do (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#ff9f1c\" stroke-width=\"3\" opacity=\"0.55\"/>~%"
                                    (zoom-x 600.0f0) (zoom-y 200.0f0)
                                    (+ (zoom-x 600.0f0) (* 92.0f0 (cos angle)))
                                    (+ (zoom-y 200.0f0) (* 92.0f0 (sin angle))))))
                 (format stream "<circle cx=\"~,1F\" cy=\"~,1F\" r=\"11\" fill=\"white\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"white\" font-size=\"21\">C: カメラ</text><circle cx=\"~,1F\" cy=\"~,1F\" r=\"13\" fill=\"#ff9f1c\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#ffcf70\" font-size=\"21\">L: 光源</text><circle cx=\"~,1F\" cy=\"~,1F\" r=\"10\" fill=\"none\" stroke=\"#52b7ff\" stroke-width=\"4\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#8ed0ff\" font-size=\"21\">H: 当たり点</text>~%"
                         (zoom-x eye-x) (zoom-y eye-z) (+ 14 (zoom-x eye-x)) (- (zoom-y eye-z) 10)
                         (zoom-x 600.0f0) (zoom-y 200.0f0) (+ 16 (zoom-x 600.0f0)) (- (zoom-y 200.0f0) 12)
                         (zoom-x hit-x) (zoom-y hit-z) (+ 14 (zoom-x hit-x)) (+ 26 (zoom-y hit-z)))
                 (cond
                   ((eq mode :physical)
                    (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F L~,1F,~,1F\" stroke=\"#ff9f1c\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#ffcf70\" font-size=\"22\">L → H → C</text>~%"
                            (zoom-x 600.0f0) (zoom-y 200.0f0) (zoom-x hit-x) (zoom-y hit-z)
                            (zoom-x eye-x) (zoom-y eye-z)
                            (zoom-x hit-x) (- (zoom-y hit-z) 20)))
                   ((eq mode :primary)
                    (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#52b7ff\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-blue)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#8ed0ff\" font-size=\"21\">最初に出すレイ（主レイ）</text>~%"
                            (zoom-x eye-x) (zoom-y eye-z) (zoom-x hit-x) (zoom-y hit-z)
                            (- (/ (+ (zoom-x eye-x) (zoom-x hit-x)) 2) 105)
                            (+ (/ (+ (zoom-y eye-z) (zoom-y hit-z)) 2) 8)))
                   ((eq mode :shadow)
                    (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#52b7ff\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-blue)\"/><path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#7ee787\" stroke-width=\"4\" fill=\"none\" stroke-dasharray=\"10 7\" marker-end=\"url(#arrow-green)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#a7f3b1\" font-size=\"19\">光が届くか調べるレイ（影レイ）</text>~%"
                            (zoom-x eye-x) (zoom-y eye-z) (zoom-x hit-x) (zoom-y hit-z)
                            (zoom-x hit-x) (zoom-y hit-z) (zoom-x 600.0f0) (zoom-y 200.0f0)
                            (+ (/ (+ (zoom-x hit-x) (zoom-x 600.0f0)) 2) 25)
                            (+ (/ (+ (zoom-y hit-z) (zoom-y 200.0f0)) 2) 35)))
                   ((eq mode :local)
                    (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#ff9f1c\" stroke-width=\"6\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#ffcf70\" font-size=\"20\">直接照明：光源からHへ届く明るさ</text>~%"
                            (zoom-x 600.0f0) (zoom-y 200.0f0) (zoom-x hit-x) (zoom-y hit-z)
                            (+ (/ (+ (zoom-x hit-x) (zoom-x 600.0f0)) 2) 18)
                            (- (/ (+ (zoom-y hit-z) (zoom-y 200.0f0)) 2) 18)))
                   ((eq mode :reflection)
                    (format stream "<path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#52b7ff\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-blue)\"/><path d=\"M~,1F,~,1F L~,1F,~,1F\" stroke=\"#c084fc\" stroke-width=\"5\" fill=\"none\" stroke-dasharray=\"10 7\" marker-end=\"url(#arrow-purple)\"/><text x=\"~,1F\" y=\"~,1F\" fill=\"#d8b4fe\" font-size=\"19\">反射した先を調べるレイ（反射レイ）</text>~%"
                            (zoom-x eye-x) (zoom-y eye-z) (zoom-x hit-x) (zoom-y hit-z)
                            (zoom-x hit-x) (zoom-y hit-z) (zoom-x reflection-end-x) (zoom-y reflection-end-z)
                            (+ (/ (+ (zoom-x hit-x) (zoom-x reflection-end-x)) 2) 22)
                            (- (/ (+ (zoom-y hit-z) (zoom-y reflection-end-z)) 2) 12))))
                 (format stream "</g></svg>~%")))
             (write-runtime-diagram (filename title kind)
               (with-open-file (stream (%explainer-path directory filename)
                                       :direction :output :if-exists :supersede)
                 (svg-header stream title)
                 (ecase kind
                   (:grid
                    (format stream "<text x=\"280\" y=\"85\" fill=\"#8ed0ff\" font-size=\"26\">1ブロック：16 × 16画素</text><text x=\"745\" y=\"85\" fill=\"#ffcf70\" font-size=\"24\">この橙の四角が1ブロック</text><rect x=\"280\" y=\"110\" width=\"300\" height=\"300\" rx=\"18\" fill=\"#1b2c3d\" stroke=\"#ff9f1c\" stroke-width=\"6\"/>~%")
                    (loop for n from 1 below 16
                          do (format stream "<path d=\"M~,1F,110 V410 M280,~,1F H580\" stroke=\"#789\" stroke-width=\"1\"/>~%"
                                     (+ 280 (* n 18.75)) (+ 110 (* n 18.75))))
                    (loop for row below 3
                          do (loop for column below 4
                                   for x = (+ 720 (* column 68))
                                   for y = (+ 135 (* row 82))
                                   for selected = (and (= row 1) (= column 1))
                                   do (format stream "<rect x=\"~,1F\" y=\"~,1F\" width=\"58\" height=\"70\" rx=\"8\" fill=\"~A\" stroke=\"~A\" stroke-width=\"3\"/>~%"
                                              x y (if selected "#ff9f1c" "#50657a")
                                              (if selected "#ffcf70" "#789"))))
                    ;; Draw this after the gray cells so its end visibly lands
                    ;; on the selected orange block rather than behind it.
                    (format stream "<path d=\"M600,260 H800\" stroke=\"#ff9f1c\" stroke-width=\"6\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><text x=\"610\" y=\"235\" fill=\"#ffcf70\" font-size=\"20\">この全体が</text>~%"))
                   (:thread-flow
                    (format stream "<text x=\"70\" y=\"105\" fill=\"#8ed0ff\" font-size=\"26\">完成画像の1画素</text><rect x=\"75\" y=\"140\" width=\"190\" height=\"190\" fill=\"#1b2c3d\" stroke=\"#52b7ff\" stroke-width=\"4\"/><g stroke=\"#789\" stroke-width=\"2\"><path d=\"M122,140 V330 M170,140 V330 M218,140 V330 M75,187 H265 M75,235 H265 M75,282 H265\"/></g><rect x=\"170\" y=\"235\" width=\"48\" height=\"47\" fill=\"#52b7ff\"/><path d=\"M290,235 H365\" stroke=\"#ff9f1c\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><rect x=\"390\" y=\"145\" width=\"270\" height=\"180\" rx=\"18\" fill=\"#1b2c3d\" stroke=\"#ff9f1c\" stroke-width=\"4\"/><text x=\"475\" y=\"190\" fill=\"#ffcf70\" font-size=\"30\">1 スレッド</text><path d=\"M435,230 H505 H575\" stroke=\"#52b7ff\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-blue)\"/><text x=\"420\" y=\"275\" fill=\"#8ed0ff\" font-size=\"22\">主レイ</text><path d=\"M575,230 H625\" stroke=\"#7ee787\" stroke-width=\"4\" fill=\"none\" stroke-dasharray=\"9 6\" marker-end=\"url(#arrow-green)\"/><text x=\"565\" y=\"275\" fill=\"#a7f3b1\" font-size=\"22\">影レイ</text><path d=\"M685,235 H780\" stroke=\"#ff9f1c\" stroke-width=\"5\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><rect x=\"820\" y=\"170\" width=\"210\" height=\"130\" rx=\"16\" fill=\"#1b2c3d\" stroke=\"#7ee787\" stroke-width=\"4\"/><text x=\"872\" y=\"225\" fill=\"#a7f3b1\" font-size=\"30\">最終RGB</text><g><rect x=\"865\" y=\"245\" width=\"42\" height=\"24\" fill=\"#f55\"/><rect x=\"912\" y=\"245\" width=\"42\" height=\"24\" fill=\"#5f5\"/><rect x=\"959\" y=\"245\" width=\"42\" height=\"24\" fill=\"#55f\"/></g>~%"))
                   (:scheduling
                    ;; Keep the scheduling diagram visual.  The subtitle
                    ;; introduces the terms, so labels cannot overflow boxes.
                    (format stream "<g fill=\"#1b2c3d\" stroke=\"#52b7ff\" stroke-width=\"4\"><rect x=\"90\" y=\"125\" width=\"280\" height=\"280\" rx=\"18\"/><rect x=\"500\" y=\"140\" width=\"190\" height=\"70\" rx=\"14\"/><rect x=\"500\" y=\"235\" width=\"190\" height=\"70\" rx=\"14\"/><rect x=\"500\" y=\"330\" width=\"190\" height=\"70\" rx=\"14\"/><rect x=\"830\" y=\"125\" width=\"300\" height=\"280\" rx=\"18\"/></g><g fill=\"#52b7ff\"><rect x=\"125\" y=\"160\" width=\"55\" height=\"55\"/><rect x=\"195\" y=\"160\" width=\"55\" height=\"55\"/><rect x=\"265\" y=\"160\" width=\"55\" height=\"55\"/><rect x=\"125\" y=\"230\" width=\"55\" height=\"55\"/><rect x=\"195\" y=\"230\" width=\"55\" height=\"55\"/><rect x=\"265\" y=\"230\" width=\"55\" height=\"55\"/><rect x=\"125\" y=\"300\" width=\"55\" height=\"55\"/><rect x=\"195\" y=\"300\" width=\"55\" height=\"55\"/><rect x=\"265\" y=\"300\" width=\"55\" height=\"55\"/></g><g fill=\"#ff9f1c\"><rect x=\"535\" y=\"160\" width=\"120\" height=\"30\" rx=\"8\"/><rect x=\"535\" y=\"255\" width=\"120\" height=\"30\" rx=\"8\"/><rect x=\"535\" y=\"350\" width=\"120\" height=\"30\" rx=\"8\"/></g><g fill=\"#7ee787\"><circle cx=\"905\" cy=\"195\" r=\"32\"/><circle cx=\"1055\" cy=\"195\" r=\"32\"/><circle cx=\"905\" cy=\"335\" r=\"32\"/><circle cx=\"1055\" cy=\"335\" r=\"32\"/></g><g stroke=\"#ff9f1c\" stroke-width=\"7\" fill=\"none\" marker-end=\"url(#arrow-orange)\"><path d=\"M385,265 H485\"/><path d=\"M705,265 H815\"/></g>~%"))
                   (:transfer
                    (format stream "<text x=\"70\" y=\"110\" fill=\"white\" font-size=\"30\">カーネル完了後に、完成済みバッファをCPUへコピー</text><rect x=\"110\" y=\"165\" width=\"370\" height=\"210\" rx=\"18\" fill=\"#1b2c3d\" stroke=\"#52b7ff\" stroke-width=\"4\"/><text x=\"205\" y=\"220\" fill=\"#8ed0ff\" font-size=\"30\">GPUメモリ</text><text x=\"165\" y=\"280\" fill=\"white\" font-size=\"24\">全画素の最終RGB配列</text><text x=\"165\" y=\"325\" fill=\"white\" font-size=\"24\">R / G / B</text><path d=\"M500,270 H740\" stroke=\"#ff9f1c\" stroke-width=\"8\" fill=\"none\" marker-end=\"url(#arrow-orange)\"/><text x=\"535\" y=\"240\" fill=\"#ffcf70\" font-size=\"21\">device → host コピー</text><rect x=\"780\" y=\"165\" width=\"370\" height=\"210\" rx=\"18\" fill=\"#1b2c3d\" stroke=\"#7ee787\" stroke-width=\"4\"/><text x=\"815\" y=\"215\" fill=\"#a7f3b1\" font-size=\"24\">PPM（Portable Pixmap）</text><text x=\"815\" y=\"270\" fill=\"white\" font-size=\"21\">各画素の R / G / B の数値</text><text x=\"815\" y=\"315\" fill=\"#a7f3b1\" font-size=\"21\">→ 画像ファイルとして保存</text>~%")))
                 (format stream "</g></svg>~%"))))
        (write-intro "intro.svg")
        (write-explanation-panel "scene.svg" "シーン全体（上から見た図）" :scene)
        (write-explanation-panel "ray-direction.svg" "光の経路と計算の経路" :physical)
        (write-explanation-panel "primary-ray.svg" "主レイ" :primary)
        (write-explanation-panel "shadow-ray.svg" "影レイ" :shadow)
        (write-explanation-panel "local-shading.svg" "直接照明" :local)
        (write-explanation-panel "reflection-ray.svg" "反射レイ" :reflection)
        (write-runtime-diagram "grid.svg" "ブロック" :grid)
        (write-runtime-diagram "thread-flow.svg" "スレッド" :thread-flow)
        (write-runtime-diagram "scheduling.svg" "実行" :scheduling)
        (write-runtime-diagram "transfer.svg" "転送" :transfer)))))

(defun write-explainer-stage-images
    (directory width height size marker-x marker-y
     out-r out-g out-b
     out-direct-r out-direct-g out-direct-b
     out-one-bounce-r out-one-bounce-g out-one-bounce-b)
  "Save only actual completed GPU output buffers used by the storyboard."
  (declare (ignore out-direct-r out-direct-g out-direct-b
                   out-one-bounce-r out-one-bounce-g out-one-bounce-b))
  (ensure-directories-exist (%explainer-path directory "placeholder"))
  ;; The opening and closing stills are the same completed GPU result.
  (write-ppm (%explainer-path directory "00-title.ppm") width height size out-r out-g out-b)
  (write-ppm-with-pixel-marker (%explainer-path directory "04-pixel.ppm")
                               width height size out-r out-g out-b marker-x marker-y)
  (write-ppm (%explainer-path directory "15-final.ppm") width height size out-r out-g out-b))

(defun %srt-timestamp (seconds)
  (multiple-value-bind (minutes remaining-seconds) (floor seconds 60)
    (format nil "00:~2,'0D:~2,'0D,000" minutes remaining-seconds)))

(defun %ass-timestamp (seconds)
  (multiple-value-bind (minutes remaining-seconds) (floor seconds 60)
    (format nil "0:~D:~2,'0D.00" minutes remaining-seconds)))

(defun %ass-caption-text (caption)
  (with-output-to-string (stream)
    (loop for character across caption
          do (if (char= character #\Newline)
                 (write-string "\\N" stream)
                 (write-char character stream)))))

(defun %validate-explainer-captions ()
  "Reject captions that would create accidental or unreadable subtitle breaks."
  (dolist (stage *explainer-stages*)
    (let ((caption (second stage))
          (line-length 0)
          (line-count 1))
      (loop for character across caption
            do (if (char= character #\Newline)
                   (progn
                     (when (or (zerop line-length) (> line-length 52))
                       (error "Invalid subtitle line in ~A: ~S" (first stage) caption))
                     (setf line-length 0)
                     (incf line-count))
                   (incf line-length)))
      (when (or (> line-count 2) (zerop line-length) (> line-length 52))
        (error "Subtitle must contain one or two short lines in ~A: ~S"
               (first stage) caption))
      ;; These patterns indicate a term was split in the middle, rather than
      ;; at a sentence boundary.  Keep the check explicit and fail early.
      (when (or (search (format nil "表面~%H") caption)
                (search (format nil "GPU~%は") caption))
        (error "Unexpected word break in subtitle ~A: ~S" (first stage) caption)))))

(defun %write-explainer-subtitles (directory seconds-per-stage)
  (%validate-explainer-captions)
  (with-open-file (stream (%explainer-path directory "explanation.srt")
                          :direction :output :if-exists :supersede)
    (loop for (filename caption) in *explainer-stages*
          for index from 1
          for start = (* (1- index) seconds-per-stage)
          for end = (* index seconds-per-stage)
          do (format stream "~D~%~A --> ~A~%~A~%~%"
                     index (%srt-timestamp start) (%srt-timestamp end) caption)))
  ;; ASS stores the top alignment and margins in the subtitle itself.  This
  ;; avoids FFmpeg-version-dependent handling of force_style on SRT files.
  (with-open-file (stream (%explainer-path directory "explanation.ass")
                          :direction :output :if-exists :supersede)
    (format stream "[Script Info]~%ScriptType: v4.00+~%PlayResX: 1280~%PlayResY: 720~%~%")
    (format stream "[V4+ Styles]~%")
    (format stream "Format: Name,Fontname,Fontsize,PrimaryColour,SecondaryColour,OutlineColour,BackColour,Bold,Italic,Underline,StrikeOut,ScaleX,ScaleY,Spacing,Angle,BorderStyle,Outline,Shadow,Alignment,MarginL,MarginR,MarginV,Encoding~%")
    (format stream "Style: Default,Noto Sans CJK JP,30,&H00FFFFFF,&H000000FF,&H00000000,&H80000000,0,0,0,0,100,100,0,0,1,2,0,8,120,120,36,1~%~%")
    (format stream "[Events]~%Format: Layer,Start,End,Style,Name,MarginL,MarginR,MarginV,Effect,Text~%")
    (loop for (filename caption) in *explainer-stages*
          for index from 1
          for start = (* (1- index) seconds-per-stage)
          for end = (* index seconds-per-stage)
          do (format stream "Dialogue: 0,~A,~A,Default,,120,120,44,,~A~%"
                     (%ass-timestamp start) (%ass-timestamp end)
                     (%ass-caption-text caption))))
  (with-open-file (stream (%explainer-path directory "storyboard.ffconcat")
                          :direction :output :if-exists :supersede)
    (format stream "ffconcat version 1.0~%")
    (dolist (stage *explainer-stages*)
      (format stream "file '~A'~%duration ~D~%" (first stage) seconds-per-stage))
    ;; The concat demuxer needs the final image repeated to honor its duration.
    (format stream "file '~A'~%" (first (car (last *explainer-stages*))))) )

(defun %write-explainer-readme (directory marker-x marker-y seconds-per-stage)
  (declare (ignore marker-x marker-y))
  (with-open-file (stream (%explainer-path directory "README.txt")
                          :direction :output :if-exists :supersede)
    (format stream "GPU Raytracer explainer storyboard~%~%")
    (format stream "The diagrams show the actual GPU launch structure and one representative pixel. ~%")
    (format stream "Each stage is displayed for ~D seconds.\n\n" seconds-per-stage)
    (dolist (stage *explainer-stages*)
      (format stream "~A: ~A~%" (first stage) (second stage)))))

(defun run-gpu-explainer (&key (res 8) (directory "gpu-explainer")
                                (seconds-per-stage 10))
  "Render a still scene and save numbered explanatory stages plus video metadata.

RUN.SH's MODE=explainer consumes storyboard.ffconcat and explanation.srt to
produce an MP4.  The yellow marker is intentionally added only to these
educational copies, never to the production render."
  (unless (plusp seconds-per-stage)
    (error "SECONDS-PER-STAGE must be positive, got ~S." seconds-per-stage))
  (let* ((width (* res 100))
         (height (* res 100))
         ;; The exact center looks at the floor in this scene.  This point
         ;; deliberately lands on a large foreground sphere, making the
         ;; primary-ray and reflection explanations visually meaningful.
         (marker-x (floor (* width 0.45f0)))
         (marker-y (floor (* height 0.275f0)))
         (final-path (%explainer-path directory "final-unannotated.ppm")))
    (ensure-directories-exist final-path)
    (run-gpu-raytracer :res res :output-file final-path
                        :write-debug-images t
                        :explain-directory directory
                        :explain-pixel-x marker-x
                        :explain-pixel-y marker-y
                        :progressive-directory directory
                        :progressive-bands 5)
    (%write-explainer-subtitles directory seconds-per-stage)
    (%write-explainer-readme directory marker-x marker-y seconds-per-stage)
    (format t "Explainer storyboard written to ~A~%" (%explainer-directory directory))))
