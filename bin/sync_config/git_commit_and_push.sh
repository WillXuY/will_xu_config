#!/bin/bash

# 切换到目标目录
cd ~/git/will_xu_config/ || { echo "无法切换到目录 ~/git/will_xu_config/"; exit 1; }

# 确保工作目录没有未暂存的更改
git status

# 提示用户输入提交信息
echo "请输入提交信息："
read -r commit_message

# 如果没有输入提交信息，使用默认信息
if [[ -z "$commit_message" ]]; then
  commit_message="自动提交 $(date)"
fi

# 执行 Git 提交操作
git add .
git commit -m "$commit_message"

# 推送到远程仓库
git push origin main || { echo "推送失败，请检查网络连接或远程仓库设置"; exit 1; }

echo "提交并推送完成"

