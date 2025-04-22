#!/bin/bash
# install-sway-deps.sh - 一键安装 Fedora 上 Sway 桌面环境所需的所有软件

set -e

echo "🧩 开始安装 Sway 相关软件包..."

# 基础窗口管理器和组件
sudo dnf install -y \
  sway \                         # Wayland 窗口管理器
  swayidle \                    # 空闲锁屏、自动休眠控制
  swaylock \                    # 屏幕锁
  swaybg \                      # 壁纸设置工具

# 常用终端和启动器
  foot \                        # 轻量终端模拟器
  rofi \                        # 启动器

# 状态栏
  i3blocks \                    # i3bar 状态栏用的模块系统

# 输入法支持
  fcitx5 \
  fcitx5-configtool \           # 输入法配置工具（支持中文）

# 多媒体与电源管理
  pulseaudio-utils \            # pactl 音量控制命令
  brightnessctl \               # 调节屏幕亮度

# 通知与美化
  mako \                        # 通知服务
  variety \                     # 壁纸轮换工具

# 截图工具
  grim \                        # 截图
  slurp \                       # 与 grim 配合进行区域选择

# 常用浏览器（你的快捷键中有使用）
  firefox \
  chromium

# Flatpak 支持（用于 DBeaver）
  flatpak

echo "✅ 软件安装完成！"

# Flatpak 提示
echo "💡 请确保你已添加 flathub 源，例如："
echo "   flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo"

# 权限提醒
echo "🔐 请确保 input-remapper.sh 脚本和 autoswitch_display.sh 可执行并已放置在 ~/bin/ 目录下。"

# 配置完成提示
echo "🎉 所有依赖已准备就绪，欢迎开始使用你的 Fedora Sway 桌面环境！"

