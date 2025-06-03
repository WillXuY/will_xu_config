;;; init-local.el --- local configurations for Purcell Emacs.

;;; Commentary:
;; 这个文件包含了对 purcell Emacs 的本地化修改。

;;; Code:

;; 本地语言环境设置
;; (set-locale-environment "en_US.UTF-8")
;; 这个是为了在 org mode 中用英文显示日期，默认是中文
;; (setq system-time-locale "C")

;;;; 添加 eshell 的环境变量，禁用 __pycache__ 的生成
(with-eval-after-load 'eshell
  ;; 确保每次新建 Eshell 缓冲区时，都先设置环境变量
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (setenv "PYTHONDONTWRITEBYTECODE" "1"))))
;;;; End eshell 的环境变量

;;;; 在加载完成配置后在修改软件源
(with-eval-after-load 'init-elpa
  (require 'package)
  (setq package-archives
        '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
          ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
  (message "✅ 使用了自定义 ELPA 镜像源."))
;;;; End 软件源

;;;; 快捷键相关
;; EVIL 配置
(when (maybe-require-package 'evil)
  ;; 关闭这个参数才能使用 evil collection
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (require 'evil)
  (evil-mode 1))

(when (maybe-require-package 'evil-collection)
  (require 'evil-collection)
  (evil-collection-init))

;; general 自定义快捷键
(when (maybe-require-package 'general)
  (require 'general)
  (general-create-definer my-space-leader-def
    :states '(normal visual motion emacs) ;; insert mode
    :prefix "SPC"                         ;; 定义 Space 为 Leader 键
    :keymaps 'override)                   ;; 防止被覆盖
  (my-space-leader-def
    "b" '(:ignore t :which-key "buffers")
    "b b" 'switch-to-buffer
    "b e" 'eval-buffer
    "b i" 'ibuffer
    "b k" 'kill-this-buffer
    "b n" 'next-buffer
    "b p" 'previous-buffer

    "e" '(:ignore t :which-key "eglot")
    "e a" 'eglot-code-actions

    "f" '(:ignore t :which-key "files")
    "f f" 'find-file
    "f s" 'save-buffer
    "f r" 'recentf-open

    "g" '(:ignore t :which-key "git")
    "g s" 'magit-status
    "g l" '(magit-log-current :which-key "log current")

    "o" '(:ignore t :which-key "org-mode")
    "o h" 'org-insert-heading

    "p" '(:ignore t :which-key "projectile")
    "p p" 'projectile-switch-project
    "p f" 'projectile-find-file
    "p g" 'projectile-ripgrep

    "t" '(:ignore t :which-key "treemacs")
    "t t" 'treemacs

    "x" '(:ignore t :which-key "run")
    "x x" 'execute-extended-command))
;;;; End 快捷键相关

;;;; UI 外观系列修改
;; theme catppuccin
(when (maybe-require-package 'catppuccin-theme)
  (setq catppuccin-flavor 'frappe)
  (load-theme 'catppuccin :no-confirm))

;; font in GUI: use cnfonts
;; 只在 gui 界面调试字体，其他交给终端
(when (display-graphic-p)
  ;; 设置英文字符字体为 Hack Nerd Font, 字体大小为 13
  (set-face-attribute 'default nil :font "Hack Nerd Font-13")
  ;; 设置中文字符字体为 WenQuanYi Micro Hei, 字体大小为 15.5
  (set-fontset-font t 'han "WenQuanYi Micro Hei Mono-15.5"))

;; modeline
(when (maybe-require-package 'doom-modeline)
  ;; 只要显示文件名
  (setq doom-modeline-buffer-file-name-style 'name
        doom-modeline-icon t)
  (add-hook 'after-init-hook #'doom-modeline-mode))

;; nyan cat mode
(when (maybe-require-package 'nyan-mode)
  (setq nyan-bar-length 20      ;; 设置 Nyan Cat 的长度
        nyan-animate-nyancat t  ;; 启用动画
        nyan-wavy-trail t)      ;; 启用波浪尾巴
  (nyan-mode 1))

(maybe-require-package 'ripgrep)

;; Treemacs 文件管理器, 不使用 require 就不会立即加载
(when (require-package 'treemacs)
  ;; 设置字体大小为缩小1个级别
  (with-eval-after-load 'treemacs
    ;; 添加 treemacs 的禁用自动换行
    (add-hook 'treemacs-mode-hook
              (lambda ()
                ;; 关闭 visual-line-mode（如果已启用）
                ;; (when (bound-and-true-p visual-line-mode)
                ;; (visual-line-mode -1))
                ;; 在当前 Treemacs 缓冲区启用行截断
                (setq-local truncate-lines t
                            ;; （可选）在侧边窄窗口也截断
                            truncate-partial-width-windows t)))
    (text-scale-set 0) ;; 字体缩放
    (text-scale-decrease 1))
  (when (maybe-require-package 'treemacs-evil)
    (require 'treemacs-evil)))

;; 确保在 theme 和 evil-mode 后再调整光标
(with-eval-after-load 'evil
  (with-eval-after-load 'catppuccin-theme
    ;; 1. 全局方块光标
    (setq-default cursor-type 'box)
    ;; 2. 统一所有 Evil 状态的光标为白色方块
    (setq evil-normal-state-cursor   '(box   "white")
          evil-insert-state-cursor   '(bar   "white")
          evil-visual-state-cursor   '(box   "green")
          evil-replace-state-cursor  '(box   "red")
          evil-emacs-state-cursor    '(box   "white")
          evil-operator-state-cursor '(box   "white"))
    ;; 3. 直接设置光标脸谱，防止其它插件覆盖
    (set-face-attribute 'cursor nil :background "white")))
;;;; End UI 外观系列

;;;; Org mode config
;; org 设置全局的头参数
(setq org-babel-default-header-args
      '((:eval . "never-export")
        (:cache . "no")
        (:results . "output")))
;; text 执行确认
(setq org-confirm-babel-evaluate
      (lambda (lang _) (not (member lang '("text")))))
;; text 设置输出
(defun org-babel-execute:text (body params)
  "Org-babel 扩展,当遇到 text 头时，把 body 原样当结果返回.
BODY is the content os the block as a string.
PARAMS is an alist of block header arguments.
Returns BODY unchanged as the result."
  body)

;; python3 支持
(setq org-babel-python-command "python3"
      python-shell-interpreter "python3")

;; 保持编辑src时在同一个窗口，减少焦点问题
(setq org-src-window-setup 'current-window
      org-src-fontify-natively t  ;; 添加代码块显示时的额外高亮
      org-edit-src-content-indentation 0  ;; 不要在代码块中做额外缩进
      org-src-preserve-indentation t) ;; 源码的原样缩进不做修改
;;;; End Org mode config

;;;; eglot(LSP) with corfu
(require 'eglot)

(defun my-eglot-use-project-pylsp ()
  "自动为当前 Python 项目配置基于项目名的虚拟环境和 pylsp."
  (when (derived-mode-p 'python-mode)
    (let* ((project-root (or (vc-root-dir) default-directory))
           (project-name (file-name-nondirectory (directory-file-name project-root)))
           (venv-path (expand-file-name project-name "~/.virtualenvs/"))
           (pylsp-path (expand-file-name "bin/pylsp" venv-path)))
      (when (file-executable-p pylsp-path)
        ;; 设置局部 process-environment，影响 eglot 启动时的环境
        (setq-local process-environment
                    (cons (format "VIRTUAL_ENV=%s" venv-path)
                          (cons (format "PATH=%s:%s"
                                        (expand-file-name "bin" venv-path)
                                        (getenv "PATH"))
                                process-environment)))

        ;; 设置 eglot-server-programs 为局部变量
        (setq-local eglot-server-programs `((python-mode . (,pylsp-path))))

        (message "🐍 使用虚拟环境 %s 中的 pylsp" project-name)))))

;; Hook 先设置环境和 eglot-server-programs
(add-hook 'python-mode-hook #'my-eglot-use-project-pylsp)
;; 再启动 eglot
(add-hook 'python-mode-hook #'eglot-ensure)

;; 其它语言通用 hook（如 Java）
(add-hook 'java-mode-hook #'eglot-ensure)

;; eglot jdtls for Java
;; 自定义 java 工作目录
(defun my/java-workspace-dir ()
  "使用 Projectile 动态设置工作区路径."
  (let ((project-root (projectile-project-root)))
    (if project-root
        ;; 尝试自动创建 .lsp-workspace 工作目录
        (expand-file-name
         (concat ".lsp-workspace/"
		 (file-name-nondirectory project-root))
	 project-root)
      (error "未找到 Java 项目的根目录"))))

(with-eval-after-load 'eglot
  ;; 动态设置工作区路径
  (setq eglot-java-workspace-dir-function #'my/java-workspace-dir)
  ;; 配置 jdtls 服务的启动命令
  (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))
  ;; jdtls 相关选项
  ;; 启用 import 的自动整理
  ;; (setq lsp-java-save-action-organize-imports t)
  ;; 禁用 Gradle（使用的是 Maven）
  (setq eglot-java-import-gradle-enabled nil
        eglot-java-import-maven-enabled t))

;; todo 配置顶部作快捷栏

;;;; End eglot(LSP) with corfu

;;;; AI coding
;; gptel
(maybe-require-package 'gptel)
(with-eval-after-load 'gptel
  ;; 定义多个 Ollama 后端
  (gptel-make-ollama "qwen2.5-ollama-podman"
    :host "127.0.0.1:11434"
    :stream t
    :models '("qwen2.5-coder:7b"))
  (gptel-make-ollama "deepseek-coder-ollama-podman"
    :host "127.0.0.1:11434"
    :stream t
    :models '("deepseek-coder:6.7b"))
  (gptel-make-ollama "codellama-ollama-podman"
    :host "127.0.0.1:11434"
    :stream t
    :models '("codellama:7b"))

  ;; 设置默认后端
  (setq gptel-backend (gptel-get-backend "qwen2.5-ollama-podman")))
;;;; End AI coding

;; Purcell Code
(provide 'init-local)

;;; init-local.el ends here
