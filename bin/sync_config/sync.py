import os
import shutil
import yaml

# 配置文件路径
config_file = os.path.expanduser('~/bin/sync_config/config.yml')

# 目标目录
destination_dir = os.path.expanduser('~/git/will_xu_config/')

# 读取配置文件
def load_config():
    with open(config_file, 'r') as file:
        return yaml.safe_load(file)

# 强制覆盖文件或目录
def overwrite_and_copy(src, dst):
    # 如果目标路径已存在，删除
    if os.path.exists(dst):
        if os.path.isdir(dst):
            shutil.rmtree(dst)  # 删除目录
        else:
            os.remove(dst)  # 删除文件

    # 复制文件或目录
    if os.path.isfile(src):
        # 如果是文件，复制文件
        dst_dir = os.path.dirname(dst)
        if not os.path.exists(dst_dir):
            os.makedirs(dst_dir)
        shutil.copy2(src, dst)
        print(f"强制覆盖文件: {src} -> {dst}")
    elif os.path.isdir(src):
        # 如果是目录，复制整个目录
        dst_dir = dst  # 目标目录已经传入
        if not os.path.exists(dst_dir):
            os.makedirs(dst_dir)
        shutil.copytree(src, dst_dir, dirs_exist_ok=True)
        print(f"强制覆盖目录: {src} -> {dst_dir}")
    else:
        print(f"源路径无效: {src}")

# 复制文件或目录
def copy_files(config):
    for item in config:
        for key, value in item.items():
            src = os.path.expanduser(value)  # 获取源文件路径
            # 生成相对路径以保留结构
            relative_path = os.path.relpath(src, os.path.expanduser('~'))  # 从用户根目录开始相对路径
            dst = os.path.join(destination_dir, relative_path)

            overwrite_and_copy(src, dst)

# 主函数
def main():
    config = load_config()
    copy_files(config)

if __name__ == "__main__":
    main()

