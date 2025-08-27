#!/bin/bash

# 配置
CONTAINER_NAME="filebrowser"
HOST_PORT=8765
DATA_DIR="$HOME/filebrowser_data"

# 自动创建文件目录（如果不存在）
mkdir -p "$DATA_DIR"

echo "Starting new filebrowser container..."
podman run -d \
    --replace \
    --name "$CONTAINER_NAME" \
    -e FB_PORT=8080 \
    -p "$HOST_PORT":8080 \
    -v "$DATA_DIR":/srv \
    docker.io/filebrowser/filebrowser

echo "Filebrowser is running. Visit http://localhost:$HOST_PORT"

ip a | grep 192

