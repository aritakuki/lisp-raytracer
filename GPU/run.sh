#!/bin/bash
# Run the Common Lisp GPU raytracer.
# This script intentionally contains no Lisp source: gpu-raytracer.lsp is the
# source of truth and can now be split into focused files without duplicating it.

set -euo pipefail

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"

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

echo "=== Loading cl-cuda ==="
sbcl --non-interactive --load "$HOME/quicklisp/setup.lisp" --eval "(ql:quickload :cl-cuda)"

cd "$SCRIPT_DIR"

echo "=== Verifying expanded GPU kernel ==="
sbcl --noinform --non-interactive --load verify-expanded-kernel.lsp

sbcl --non-interactive \
    --eval "(defparameter *cpu-init-random-state* (make-random-state nil))" \
    --load "$HOME/quicklisp/setup.lisp" \
    --load gpu-main.lsp \
    --eval "(gpu-raytracer:run-gpu-raytracer :res 8 :output-file \"spheres_gpu.ppm\")"
