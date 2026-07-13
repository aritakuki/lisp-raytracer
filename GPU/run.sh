#!/bin/bash
# Render the GPU raytracer and encode its animation in the current environment.
# The default is a 60-frame, 60fps (one second) MP4 suitable for Google Colab.

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

MODE="${MODE:-animation}"
RES="${RES:-8}"
FRAMES="${FRAMES:-60}"
FPS="${FPS:-60}"
FRAME_DIR="${FRAME_DIR:-frames_gpu}"
OUTPUT_VIDEO="${OUTPUT_VIDEO:-spheres_gpu_animation.mp4}"

case "$MODE" in
    animation|still) ;;
    *) echo "MODE must be animation or still, got: $MODE" >&2; exit 2 ;;
esac

for value_name in RES FRAMES FPS; do
    value="${!value_name}"
    if ! [[ "$value" =~ ^[1-9][0-9]*$ ]]; then
        echo "$value_name must be a positive integer, got: $value" >&2
        exit 2
    fi
done

# FRAME_DIR is embedded in a Lisp string below.
if [[ "$FRAME_DIR" == *'"'* || "$FRAME_DIR" == *\\* ]]; then
    echo "FRAME_DIR must not contain a quote or backslash." >&2
    exit 2
fi

export CPATH=/usr/local/cuda/include
export LIBRARY_PATH=/usr/local/cuda/lib64
export LD_LIBRARY_PATH=/usr/local/cuda/lib64${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}
export PATH=$PATH:/usr/local/cuda/bin

if ! command -v sbcl >/dev/null 2>&1; then
    echo "=== Installing SBCL & dependencies ==="
    sudo apt-get update
    sudo apt-get install -y sbcl libffi-dev
fi

if [ ! -f "$HOME/quicklisp/setup.lisp" ]; then
    echo "=== Installing Quicklisp ==="
    curl -O https://beta.quicklisp.org/quicklisp.lisp
    sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)"
    rm quicklisp.lisp
fi

if [ "$MODE" = "animation" ] && ! command -v ffmpeg >/dev/null 2>&1; then
    echo "=== Installing FFmpeg ==="
    sudo apt-get update
    sudo apt-get install -y ffmpeg
fi

echo "=== Loading cl-cuda ==="
sbcl --non-interactive --load "$HOME/quicklisp/setup.lisp" --eval "(ql:quickload :cl-cuda)"

cd "$SCRIPT_DIR"
mkdir -p generated-cuda

echo "=== Verifying expanded GPU kernel ==="
sbcl --noinform --non-interactive --load verify-expanded-kernel.lsp

if [ "$MODE" = "still" ]; then
    sbcl --non-interactive \
        --eval "(defparameter *cpu-init-random-state* (make-random-state nil))" \
        --load "$HOME/quicklisp/setup.lisp" \
        --load gpu-main.lsp \
        --eval "(setf cl-cuda.api.nvcc:*tmp-path* \"$SCRIPT_DIR/generated-cuda/\")" \
        --eval "(gpu-raytracer:run-gpu-raytracer :res $RES :output-file \"spheres_gpu.ppm\")"
    echo "=== Still image written: $SCRIPT_DIR/spheres_gpu.ppm ==="
else
    mkdir -p "$FRAME_DIR"
    rm -f "$FRAME_DIR"/spheres_frame_*.ppm

    echo "=== Rendering $FRAMES GPU frames ==="
    sbcl --non-interactive \
        --eval "(defparameter *cpu-init-random-state* (make-random-state nil))" \
        --load "$HOME/quicklisp/setup.lisp" \
        --load gpu-main.lsp \
        --eval "(setf cl-cuda.api.nvcc:*tmp-path* \"$SCRIPT_DIR/generated-cuda/\")" \
        --eval "(gpu-raytracer:run-gpu-animation :frames $FRAMES :res $RES :frame-directory \"$FRAME_DIR\")"

    echo "=== Encoding $OUTPUT_VIDEO at $FPS fps ==="
    ffmpeg -y -framerate "$FPS" -start_number 0 \
        -i "$FRAME_DIR/spheres_frame_%03d.ppm" \
        -c:v libx264 -pix_fmt yuv420p "$OUTPUT_VIDEO"
    echo "=== Animation written: $SCRIPT_DIR/$OUTPUT_VIDEO ==="
fi
