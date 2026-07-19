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
