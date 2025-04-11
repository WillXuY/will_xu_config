#!/bin/bash
echo "🚫 系统挂起和锁屏已被抑制，按 Ctrl+C 恢复"
systemd-inhibit --what=idle:sleep --why="临时禁止挂起和锁屏" bash

