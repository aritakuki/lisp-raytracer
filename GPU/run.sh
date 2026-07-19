#!/bin/bash
# Render the GPU raytracer and encode its animation in the current environment.
# The default is a 300-frame, 60fps (five second) MP4 suitable for Google Colab.

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

MODE="${MODE:-animation}"
RES="${RES:-8}"
FRAMES="${FRAMES:-300}"
FPS="${FPS:-60}"
FRAME_DIR="${FRAME_DIR:-frames_gpu}"
OUTPUT_VIDEO="${OUTPUT_VIDEO:-spheres_gpu_animation.mp4}"
EXPLAIN_LANGUAGE="${EXPLAIN_LANGUAGE:-ja}"
EXPLAIN_SECONDS="${EXPLAIN_SECONDS:-10}"

case "$EXPLAIN_LANGUAGE" in
    ja)
        EXPLAIN_DIR="${EXPLAIN_DIR:-gpu-explainer}"
        EXPLAIN_VIDEO="${EXPLAIN_VIDEO:-gpu-raytracing-explainer.mp4}"
        ;;
    en)
        EXPLAIN_DIR="${EXPLAIN_DIR:-gpu-explainer-en}"
        EXPLAIN_VIDEO="${EXPLAIN_VIDEO:-gpu-raytracing-explainer-en.mp4}"
        ;;
    *) echo "EXPLAIN_LANGUAGE must be ja or en, got: $EXPLAIN_LANGUAGE" >&2; exit 2 ;;
esac

case "$MODE" in
    animation|still|explainer) ;;
    *) echo "MODE must be animation, still, or explainer, got: $MODE" >&2; exit 2 ;;
esac

for value_name in RES FRAMES FPS EXPLAIN_SECONDS; do
    value="${!value_name}"
    if ! [[ "$value" =~ ^[1-9][0-9]*$ ]]; then
        echo "$value_name must be a positive integer, got: $value" >&2
        exit 2
    fi
done

# These directories are embedded in Lisp strings below.
for directory_name in FRAME_DIR EXPLAIN_DIR; do
    directory="${!directory_name}"
    if [[ "$directory" == *'"'* || "$directory" == *\\* ]]; then
        echo "$directory_name must not contain a quote or backslash." >&2
        exit 2
    fi
done

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

if [ "$MODE" != "still" ] && ! command -v ffmpeg >/dev/null 2>&1; then
    echo "=== Installing FFmpeg ==="
    sudo apt-get update
    sudo apt-get install -y ffmpeg
fi

# FFmpeg's subtitle renderer needs a font that supports Japanese and English.
# Google Colab images do not consistently include one by default.
if [ "$MODE" = "explainer" ]; then
    if ! command -v fc-list >/dev/null 2>&1 || ! fc-list : family | grep -qi 'Noto Sans CJK'; then
        echo "=== Installing subtitle font ==="
        sudo apt-get update
        sudo apt-get install -y fonts-noto-cjk
    fi
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
elif [ "$MODE" = "animation" ]; then
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
else
    mkdir -p "$EXPLAIN_DIR"
    echo "=== Rendering GPU explainer stages ==="
    sbcl --non-interactive \
        --eval "(defparameter *cpu-init-random-state* (make-random-state nil))" \
        --load "$HOME/quicklisp/setup.lisp" \
        --load gpu-main.lsp \
        --eval "(setf cl-cuda.api.nvcc:*tmp-path* \"$SCRIPT_DIR/generated-cuda/\")" \
        --eval "(gpu-raytracer:run-gpu-explainer :res $RES :directory \"$EXPLAIN_DIR\" :seconds-per-stage $EXPLAIN_SECONDS :language :$EXPLAIN_LANGUAGE)"

    # Convert the explanatory diagrams to numbered PPM stages so the video
    # encoder can use one image format throughout.
    for diagram in intro scene ray-direction primary-ray shadow-ray local-shading \
                   reflection-ray grid thread-flow scheduling transfer; do
        case "$diagram" in
            intro) stage_number=01 ;;
            scene) stage_number=02 ;;
            ray-direction) stage_number=03 ;;
            primary-ray) stage_number=05 ;;
            shadow-ray) stage_number=06 ;;
            local-shading) stage_number=07 ;;
            reflection-ray) stage_number=08 ;;
            grid) stage_number=09 ;;
            thread-flow) stage_number=10 ;;
            scheduling) stage_number=11 ;;
            transfer) stage_number=14 ;;
        esac
        ffmpeg -y -i "$EXPLAIN_DIR/$diagram.svg" -frames:v 1 \
            "$EXPLAIN_DIR/$stage_number-$diagram.ppm"
    done

    # Normalize every explanatory image to one video size and fade it in/out.
    # This prevents an abrupt jump between the diagram (landscape) and the
    # square render buffers, while giving each explanation a visual boundary.
    EXPLAIN_CLIP_DIR="$EXPLAIN_DIR/video-stages"
    EXPLAIN_CLIP_MANIFEST="$EXPLAIN_CLIP_DIR/stages.ffconcat"
    EXPLAIN_SILENT_VIDEO="$EXPLAIN_CLIP_DIR/storyboard-silent.mp4"
    EXPLAIN_FADE_START=$((EXPLAIN_SECONDS - 1))
    # Keep the video order in one place: the Lisp-generated storyboard list.
    mapfile -t EXPLAIN_STAGES < <(awk -F "'" '/^file / { if (!seen[$2]++) print $2 }' \
        "$EXPLAIN_DIR/storyboard.ffconcat")
    mkdir -p "$EXPLAIN_CLIP_DIR"
    rm -f "$EXPLAIN_CLIP_DIR"/stage_*.mp4 "$EXPLAIN_CLIP_MANIFEST" "$EXPLAIN_SILENT_VIDEO"
    : > "$EXPLAIN_CLIP_MANIFEST"

    for index in "${!EXPLAIN_STAGES[@]}"; do
        stage="${EXPLAIN_STAGES[$index]}"
        printf -v clip_name 'stage_%02d.mp4' "$index"
        case "$stage" in
            00-title.ppm|04-pixel.ppm|12-progress-*.ppm|15-final.ppm)
                # Keep the raster content close to the top subtitles.
                video_filter="scale=1280:540:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:130:black"
                ;;
            *)
                # SVG diagrams already reserve their upper band for subtitles.
                video_filter="scale=1280:720:force_original_aspect_ratio=decrease,pad=1280:720:(ow-iw)/2:(oh-ih)/2:black"
                ;;
        esac
        ffmpeg -y -loop 1 -t "$EXPLAIN_SECONDS" -i "$EXPLAIN_DIR/$stage" \
            -vf "$video_filter,fade=t=in:st=0:d=1,fade=t=out:st=$EXPLAIN_FADE_START:d=1" \
            -r "$FPS" -c:v libx264 -pix_fmt yuv420p "$EXPLAIN_CLIP_DIR/$clip_name"
        printf "file '%s'\n" "$clip_name" >> "$EXPLAIN_CLIP_MANIFEST"
    done

    echo "=== Encoding explained storyboard ==="
    ffmpeg -y -f concat -safe 0 -i "$EXPLAIN_CLIP_MANIFEST" -c copy "$EXPLAIN_SILENT_VIDEO"
    ffmpeg -y -i "$EXPLAIN_SILENT_VIDEO" \
        -vf "ass=$EXPLAIN_DIR/explanation.ass" \
        -r "$FPS" -c:v libx264 -pix_fmt yuv420p "$EXPLAIN_VIDEO"
    echo "=== Explainer video written: $SCRIPT_DIR/$EXPLAIN_VIDEO ==="
fi
