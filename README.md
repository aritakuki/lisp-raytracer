# Lisp Raytracer

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
