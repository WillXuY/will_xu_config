#!/bin/bash

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
OUTPUT_DIR="$HOME/Videos"

# 使用 slurp 选择录制区域
echo "请选择录制区域..."
region=$(slurp)

# 检查选择区域是否为空
if [ -z "$region" ]; then
    echo "未选择区域，退出录制。"
    exit 1
fi

# 启动 wf-recorder 录制
echo "开始录制..."
wf-recorder -g "$region" -f "$OUTPUT_DIR/record_$TIMESTAMP.mp4"

