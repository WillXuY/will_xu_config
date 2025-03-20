#!/bin/bash
# 提取显示器名称：搜索包含 "name": 的行，然后模糊匹配 "HDMI" 或 "EDP"
HDMI=$(swaymsg -t get_outputs | grep -i '"name":' | grep -i "HDMI" | head -n1 | sed 's/.*"name": "\([^"]*\)".*/\1/')
LAPTOP=$(swaymsg -t get_outputs | grep -i '"name":' | grep -i "EDP" | head -n1 | sed 's/.*"name": "\([^"]*\)".*/\1/')

echo "Detected HDMI: $HDMI"
echo "Detected Laptop (eDP): $LAPTOP"

# 获取 HDMI 显示器对应的输出块，从出现该设备 "name" 行开始，直到下一次出现 "name"
HDMI_BLOCK=$(swaymsg -t get_outputs | awk -v dev="$HDMI" '
  BEGIN {flag=0}
  /"name":/ {
    # 当遇到新输出时，判断是否为目标设备
    if ($0 ~ dev) {
      flag=1
    } else {
      flag=0
    }
  }
  flag { print }
')

# 判断 HDMI 输出块中是否包含 '"active": true'
if echo "$HDMI_BLOCK" | grep -q '"active": true'; then
    HDMI_ACTIVE=1
else
    HDMI_ACTIVE=0
fi

if [ "$HDMI_ACTIVE" -eq 1 ]; then
    swaymsg output "$LAPTOP" disable
    swaymsg output "$HDMI" enable
    echo "HDMI display is active, enabling HDMI only."
else
    swaymsg output "$HDMI" disable
    swaymsg output "$LAPTOP" enable
    echo "HDMI display is not active, enabling laptop display only."
fi

