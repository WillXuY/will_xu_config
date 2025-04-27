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
  (require 'evil)
  (evil-mode 1))

;; general 自定义快捷键
(when (maybe-require-package 'general)
  (require 'general)
  (general-create-definer my-space-leader-def
    :states '(normal visual motion emacs)  ;; insert mode
    :prefix "SPC"                          ;; 定义 Space 为 Leader 键
    :keymaps 'override)                    ;; 防止被覆盖
  (my-space-leader-def
    "b" '(:ignore t :which-key "buffers")
    "b b" 'switch-to-buffer
    "b e" 'eval-buffer
    "b k" 'kill-this-buffer
    "b n" 'next-buffer
    "b p" 'previous-buffer

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

;; 自动在 Emacs 空闲 300 秒（5分钟）后启动 zone 模式
(require 'zone)
(zone-when-idle 300)

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
    (text-scale-set 0)  ;; 字体缩放
    (text-scale-decrease 1))
  (when (maybe-require-package 'treemacs-evil)
    (require 'treemacs-evil)))
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
;; Lsp mode for python
(add-hook 'python-mode-hook #'eglot-ensure)
;; Lsp for Java
(add-hook 'java-mode-hook 'eglot-ensure)
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
  ;; jdk 和 jdtls 的路径
  (setq eglot-server-programs '((java-mode . ("jdtls")))
        eglot-java-java-path "~/software/jdk-21.0.7+6/bin/java"
        eglot-java-server-install-dir "~/software/jdtls-1.46.1/")
  ;; 启用 import 的自动整理
  ;; (setq lsp-java-save-action-organize-imports t)
  ;; 禁用 Gradle（使用的是 Maven）
  (setq eglot-java-import-gradle-enabled nil
        eglot-java-import-maven-enabled t))
;;;; End eglot(LSP) with corfu

;; Purcell Code
(provide 'init-local)

;;; init-local.el ends here
