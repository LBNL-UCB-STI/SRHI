#!/bin/bash
# Allows running scripts from inside the image, with data mounted at /data

set -e

show_help() {
    cat << EOF
Usage: docker run --rm -v /host/data:/data my-r-image <command> [args]

Commands:
  run <script_path>        Execute R script (path relative to /app, e.g., R_scripts/script1.R)
  cat <script_path>        Print content of the script
  ls [dir]                 List files in /app (default) or subdir
  bash                     Start a bash shell
  R                        Start interactive R session
  --help                   Show this help

Examples:
  docker run --rm -v $(pwd)/data:/data my-r-image run analysis/clean.R
  docker run --rm -v $(pwd)/data:/data my-r-image cat R_scripts/script1.R
EOF
}

if [ $# -eq 0 ]; then
    show_help
    exit 0
fi

case "$1" in
    run)
        if [ -z "$2" ]; then
            echo "Error: missing script path."
            exit 1
        fi
        SCRIPT_PATH="/app/$2"
        if [ ! -f "$SCRIPT_PATH" ]; then
            echo "Error: Script not found: $SCRIPT_PATH"
            exit 1
        fi
        echo "Running $SCRIPT_PATH ..."
        Rscript "$SCRIPT_PATH"
        ;;
    cat)
        if [ -z "$2" ]; then
            echo "Error: missing script path."
            exit 1
        fi
        FILE_PATH="/app/$2"
        if [ ! -f "$FILE_PATH" ]; then
            echo "Error: File not found: $FILE_PATH"
            exit 1
        fi
        cat "$FILE_PATH"
        ;;
    ls)
        TARGET="/app/${2:-.}"
        ls -la "$TARGET"
        ;;
    bash)
        exec bash
        ;;
    R)
        exec R
        ;;
    --help)
        show_help
        ;;
    *)
        echo "Unknown command: $1"
        show_help
        exit 1
        ;;
esac