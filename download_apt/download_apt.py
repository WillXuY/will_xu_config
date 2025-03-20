import os
import yaml
import subprocess
import shutil

def download_package(package_name):
    # 下载包及其所有依赖项到指定目录
    download_dir = os.path.expanduser('~/download_apt/')
    
    # 创建目标目录，如果不存在
    os.makedirs(download_dir, exist_ok=True)
    
    # 使用 apt-get 下载包及其所有依赖项到目标目录
    try:
        subprocess.run(
            ['sudo', 'apt-get', 'install', '--download-only', package_name],
            check=True,
            env={'APT_CONFIG': '/dev/null'},  # 忽略配置文件
        )
        
        # 复制下载的文件到目标目录
        apt_cache_dir = '/var/cache/apt/archives/'
        for file_name in os.listdir(apt_cache_dir):
            if file_name.endswith('.deb'):
                src = os.path.join(apt_cache_dir, file_name)
                dest = os.path.join(download_dir, file_name)
                # 直接复制文件
                shutil.copy(src, dest)
        print(f"Downloaded and copied {package_name} to {download_dir}")
    except subprocess.CalledProcessError as e:
        print(f"Error downloading {package_name}: {e}")

def main():
    # 配置文件路径
    config_path = os.path.expanduser('~/bin/download_apt/config.yml')
    
    # 读取 YAML 文件
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            config_data = yaml.safe_load(f)
    except Exception as e:
        print(f"Failed to read config file: {e}")
        return
    
    # 处理配置文件内容并下载软件包
    for item in config_data:
        for package_name in item:
            print(f"Processing {package_name}...")
            download_package(package_name)

if __name__ == '__main__':
    main()

