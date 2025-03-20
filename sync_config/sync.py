#!/usr/bin/env python3
import os
import yaml
import shutil
import subprocess
from datetime import datetime

# 配置文件路径与目标仓库目录
config_file = os.path.expanduser("~/bin/sync_config/config.yml")
dest_dir = os.path.expanduser("~/git/will_xu_config")

# 复制文件到目标路径
def copy_files(src_dir, dest_dir):
    if not os.path.exists(dest_dir):
        os.makedirs(dest_dir)
    
    # 遍历源目录并复制文件
    for item in os.listdir(src_dir):
        s = os.path.join(src_dir, item)
        d = os.path.join(dest_dir, item)
        if os.path.isdir(s):
            # 如果是目录，递归复制
            shutil.copytree(s, d, dirs_exist_ok=True)
        else:
            # 如果是文件，直接复制
            shutil.copy2(s, d)
        print(f"Copied {s} to {d}")

def load_config(config_path):
    """加载 YAML 配置文件，返回配置项列表"""
    with open(config_path, 'r', encoding='utf-8') as f:
        return yaml.safe_load(f)

def sync_configs(config_list, dest_dir):
    """
    遍历配置项，将每个配置文件复制到目标目录中，
    保留 ~ 为根目录的目录结构。
    例如：~/.bashrc --> ~/git/will_xu_config/.bashrc
    """
    home = os.path.expanduser("~")
    for entry in config_list:
        for key, src_path in entry.items():
            src_full = os.path.expanduser(src_path)
            # 如果源路径以 home 开头，则保留相对路径
            if src_full.startswith(home):
                rel_path = src_full[len(home):]
            else:
                rel_path = os.path.basename(src_full)
            # 构造目标路径，去除开头的分隔符
            dest_full = os.path.join(dest_dir, rel_path.lstrip(os.sep))
            # 创建目标目录（如果不存在）
            os.makedirs(os.path.dirname(dest_full), exist_ok=True)
            # 复制文件，覆盖目标文件
            shutil.copy2(src_full, dest_full)
            print(f"Copied {src_full} to {dest_full}")

def git_commit(dest_dir, commit_message):
    """
    在目标目录下调用 git 命令，添加所有更改，
    提交 commit，并检查远程仓库配置后推送 commit。
    """
    # 保存当前目录，然后切换到目标仓库目录
    cwd = os.getcwd()
    os.chdir(dest_dir)
    try:
        # 添加所有更改
        subprocess.run(["git", "add", "."], check=True)
        # 提交 commit
        subprocess.run(["git", "commit", "-m", commit_message], check=True)
        # 检查是否配置了远程仓库
        remote_list = subprocess.run(["git", "remote"], stdout=subprocess.PIPE, text=True).stdout.strip()
        if remote_list:
            subprocess.run(["git", "push"], check=True)
            print("Pushed commit to remote repository.")
        else:
            print("No remote repository configured. Skipping push.")
    except subprocess.CalledProcessError as e:
        print("Git 命令执行失败:", e)
    finally:
        os.chdir(cwd)

def main():
    # 复制 ~/bin/ 下的所有文件到 ~/git/will_xu_config/
    print("复制 ~/bin/ 下的所有文件到 Git 配置目录...")
    copy_files(os.path.expanduser('~/bin'), dest_dir)
    
    # 加载配置项
    config_list = load_config(config_file)
    # 同步配置文件到目标目录
    sync_configs(config_list, dest_dir)
    
    # 获取 commit message：提示输入，不输入则使用默认 commit 信息
    default_msg = f"update config for will {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}"
    commit_msg = input(f"请输入 commit message (默认: '{default_msg}'): ").strip()
    if not commit_msg:
        commit_msg = default_msg
    
    # 提交到 git 仓库
    git_commit(dest_dir, commit_msg)

if __name__ == "__main__":
    main()

