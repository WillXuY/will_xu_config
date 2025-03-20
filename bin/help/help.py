#!/usr/bin/env python3
import os
import yaml

def main():
    # 配置文件路径
    config_path = os.path.expanduser('~/bin/help/config.yml')
    
    # 读取 YAML 文件
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            data = yaml.safe_load(f)
    except Exception as e:
        print(f"读取配置文件失败: {e}")
        return

    # 假设 YAML 文件内容格式如下：
    # - file manager: ranger
    # - text editor: vim
    # 此时 data 为列表，列表中的每个元素是一个字典
    # 获取所有软件功能名称，计算最大宽度用于对齐
    features = [list(item.keys())[0] for item in data]
    max_width = max(len(feature) for feature in features)
    
    # ANSI 转义码：绿色文本
    green = '\033[1;32m'
    reset = '\033[0m'

    # 输出两列对齐内容
    for item in data:
        for feature, command in item.items():
            # 绿色文本输出命令部分
            print(f"{feature.ljust(max_width)} : {green}{command}{reset}")


if __name__ == '__main__':
    main()

