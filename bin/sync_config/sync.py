import os
import shutil
import yaml
import subprocess
from datetime import datetime

# 定义配置文件路径和目标目录路径
CONFIG_FILE = '~/bin/sync_config/config.yml'
DEST_DIR = '~/git/will_xu_config'

def copy_config_files(config_file, dest_dir):
    """复制配置文件到 Git 仓库目录"""
    with open(config_file, 'r') as f:
        config = yaml.safe_load(f)  # 加载配置文件
    
    for item in config:
        for key, value in item.items():
            src = os.path.expanduser(value)  # 获取源文件路径
            dest = os.path.join(dest_dir, key)  # 获取目标路径
            
            try:
                # 如果是目录，使用 copytree
                if os.path.isdir(src):
                    # 如果目标目录已存在，强制覆盖
                    if os.path.exists(dest):
                        shutil.rmtree(dest)  # 删除目标目录
                    shutil.copytree(src, dest, dirs_exist_ok=True)  # 复制目录并允许覆盖
                    print(f"Copied directory {src} to {dest}")
                else:
                    # 如果是文件，使用 copy 强制覆盖
                    shutil.copy(src, dest)
                    print(f"Copied file {src} to {dest}")
            except Exception as e:
                print(f"Error copying {src} to {dest}: {e}")

def copy_bin_files(bin_dir, dest_dir):
    """复制 ~/bin/ 目录下的所有文件到目标目录"""
    for filename in os.listdir(bin_dir):
        src = os.path.join(bin_dir, filename)
        dest = os.path.join(dest_dir, 'bin', filename)
        try:
            if os.path.isdir(src):
                # 复制目录
                if os.path.exists(dest):
                    shutil.rmtree(dest)
                shutil.copytree(src, dest, dirs_exist_ok=True)
                print(f"Copied directory {src} to {dest}")
            else:
                # 复制文件
                shutil.copy(src, dest)
                print(f"Copied file {src} to {dest}")
        except Exception as e:
            print(f"Error copying {src} to {dest}: {e}")

def commit_changes():
    """提交到 Git 仓库"""
    try:
        # 获取当前时间
        commit_msg = f"update config for will {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"
        # 提示输入 commit 信息，默认是当前时间
        user_commit_msg = input(f"请输入 commit message (默认: '{commit_msg}'): ")
        commit_msg = user_commit_msg if user_commit_msg else commit_msg
        
        # 执行 git 提交操作
        subprocess.run(['git', 'add', '.'], check=True)
        subprocess.run(['git', 'commit', '-m', commit_msg], check=True)
        print("Git commit successful.")
        
        # 推送到远程仓库
        subprocess.run(['git', 'push'], check=True)
        print("Git push successful.")
    except subprocess.CalledProcessError as e:
        print(f"Git 命令执行失败: {e}")

def main():
    # 转换路径为真实路径
    config_file = os.path.expanduser(CONFIG_FILE)
    dest_dir = os.path.expanduser(DEST_DIR)
    
    # 确保目标目录存在
    os.makedirs(dest_dir, exist_ok=True)
    
    # 第一步：复制配置文件
    print("复制配置文件到 Git 配置目录...")
    copy_config_files(config_file, dest_dir)

    # 第二步：复制 ~/bin/ 下的所有文件到 ~/git/will_xu_config/bin/
    bin_dir = os.path.expanduser('~/bin')
    print("复制 ~/bin/ 下的所有文件到 Git 配置目录的 bin 子目录...")
    copy_bin_files(bin_dir, dest_dir)

    # 第三步：执行 Git 提交和推送
    print("提交变更到 Git 仓库...")
    commit_changes()

if __name__ == "__main__":
    main()

