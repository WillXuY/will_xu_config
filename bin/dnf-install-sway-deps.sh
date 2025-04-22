#!/bin/bash

# 安全选项：任何未定义变量报错；pipestatus 错误时退出；pipefail 会传递错误码
set -euo pipefail

echo "🎉 Fedora Sway 安装脚本开始执行..."

# ========== 软件安装 ==========
echo "📦 安装必要软件包..."
REQUIRED_PKGS=(
  sway foot rofi i3blocks swaylock swayidle mako
  fcitx5 fcitx5-configtool fcitx5-gtk fcitx5-qt
  grim slurp variety brightnessctl pavucontrol
  flatpak wget unzip curl zsh git
)

for pkg in "${REQUIRED_PKGS[@]}"; do
  if ! rpm -q "$pkg" >/dev/null 2>&1; then
    echo "🔹 安装 $pkg..."
    sudo dnf install -y "$pkg"
  else
    echo "✅ $pkg 已安装，跳过"
  fi
done
