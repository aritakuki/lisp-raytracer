# Lisp Raytracer

## CPU版とGPU版

CPU版は `CPU/` にあり、Common Lispだけで画素を順番に計算します。CUDA対応GPUは不要で、
レンダリングの基礎実装と結果確認に使えます。

GPU版は `GPU/` にあり、Lispがシーン情報を用意してGPUへ送り、GPUカーネルが多数の画素を
並列に計算します。GPU版の実行にはCUDA対応GPUが必要です。通常動画と解説動画はいずれも
`GPU/run.sh` から作成します。

### CPU版の動画

CPU版は60枚のPPMフレームを出力します。CPUディレクトリで実行してください。

```bash
cd CPU
sbcl --script render_cpu.lsp
ffmpeg -framerate 30 -i spheres_frame_%03d.ppm -c:v libx264 -pix_fmt yuv420p spheres_cpu_animation.mp4
```

出力: `CPU/spheres_cpu_animation.mp4`

## GPU explainer video

Google Colabなど、CUDA対応GPUのある環境でリポジトリのルートから実行します。

日本語版（既定）:

```bash
env MODE=explainer RES=8 EXPLAIN_SECONDS=10 bash GPU/run.sh
```

出力: `GPU/gpu-raytracing-explainer.mp4`

英語版:

```bash
env MODE=explainer RES=8 EXPLAIN_SECONDS=10 EXPLAIN_LANGUAGE=en bash GPU/run.sh
```

出力: `GPU/gpu-raytracing-explainer-en.mp4`

`EXPLAIN_LANGUAGE` は `ja`（既定）または `en` を指定できます。`RES=8` は
800×800画素のレンダリングを指定し、`EXPLAIN_SECONDS=10` は各説明段階の表示時間です。

## 通常のレンダリング動画

カメラと球が動く通常のレンダリング動画は、次のように作成します。

```bash
env MODE=animation RES=8 FRAMES=300 FPS=60 bash GPU/run.sh
```

出力: `GPU/spheres_gpu_animation.mp4`

`FRAMES` は生成するフレーム数、`FPS` は動画の毎秒フレーム数です。上の例は
300フレームを60fpsでエンコードするため、5秒の動画になります。生成した各フレームは
`GPU/frames_gpu/` に保存されます。
