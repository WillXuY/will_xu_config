;; 本地语言环境设置
;; (set-locale-environment "en_US.UTF-8")
;; 这个是为了在 org mode 中用英文显示日期，默认是中文
;; (setq system-time-locale "C")

;;;; 在加载完成配置后在修改软件源
(with-eval-after-load 'init-elpa
  (require 'package)
  (setq package-archives
        '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
          ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
  (message "✅ 使用了自定义 ELPA 镜像源."))

;; 自动在 Emacs 空闲 300 秒（5分钟）后启动 zone 模式
(require 'zone)
(zone-when-idle 300)

;;;; theme catppuccin
(when (maybe-require-package 'catppuccin-theme)
  (setq catppuccin-flavor 'frappe)
  (catppuccin-reload))

;;;; font in GUI: use cnfonts
;; 只在 gui 界面调试字体，其他交给终端
(when (display-graphic-p)
  ;; 设置英文字符字体为 Hack Nerd Font, 字体大小为 13
  (set-face-attribute 'default nil :font "Hack Nerd Font-13")
  ;; 设置中文字符字体为 WenQuanYi Micro Hei, 字体大小为 15.5
  (set-fontset-font t 'han "WenQuanYi Micro Hei Mono-15.5"))

;;;; UI
;; modeline
(when (maybe-require-package 'doom-modeline)
  ;; (maybe-require-package 'all-the-icons)
  (require 'doom-modeline)
  ;; 只要显示文件名
  (setq doom-modeline-buffer-file-name-style 'name)
  (setq doom-modeline-icon t)
  (doom-modeline-mode 1))

(when (maybe-require-package 'nyan-mode)
  (setq nyan-bar-length 20)  ;; 设置 Nyan Cat 的长度
  (setq nyan-animate-nyancat t)  ;; 启用动画
  (setq nyan-wavy-trail t)  ;; 启用波浪尾巴
  (nyan-mode 1))

;;;; EVIL 配置
;; 暂时移除 evil collection
(when (maybe-require-package 'evil)
  (setq evil-want-integration t)    ;; 启用基本集成
  (require 'evil)
  (evil-mode 1))

;;;; Treemacs 文件管理器, 不使用 require 就不会立即加载
(when (maybe-require-package 'treemacs)
  (maybe-require-package 'treemacs-evil)
  (require 'treemacs-evil))
;; 美化 treemacs
;; (maybe-require-package 'treemacs-nerd-icons)
;; (require 'treemacs-nerd-icons)
;; (treemacs-load-theme 'nerd-icons)
;; (maybe-require-package 'treemacs-projectile)

;;;; 安装 general 自定义快捷键
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

;;;; org-babel 扩展 text 内容可以被 python3 读取
(defun org-babel-execute:text (body params) body)
;; text 运行时不需要再询问
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (member lang '("text")))))
;; 支持 python3 的运行
(setq org-babel-python-command "python3")
;; 保持编辑src时在同一个窗口，减少焦点问题
(setq org-src-window-setup 'current-window)

;;;; LSP with corfu
;; 停止自动配置补全
(setq lsp-completion-provider :none)

;;;; Lsp mode for python
(when (maybe-require-package 'lsp-mode)
  (add-hook 'python-mode-hook #'lsp-deferred))

;;;; Lsp for Java
(defun my/java-workspace-dir ()
  "使用 Projectile 动态设置工作区路径."
  (let ((project-root (projectile-project-root)))
    (if project-root
        ;; 尝试自动创建 .lsp-workspace 工作目录
        (expand-file-name (concat ".lsp-workspace/"
				  (file-name-nondirectory project-root))
			  project-root)
      (error "未找到 Java 项目的根目录"))))

(when (maybe-require-package 'lsp-java)
  ;; jdk 和 jdtls 的路径
  (setq lsp-java-java-path "~/software/jdk-21.0.7+6/bin/java")
  (setq lsp-java-server-install-dir "~/software/jdtls-1.46.1/")
  ;; 动态设置工作区路径
  (setq lsp-java-workspace-dir-function #'my/java-workspace-dir)

  (with-eval-after-load 'lsp-java
			;; 启用 import 的自动整理
			(setq lsp-java-save-action-organize-imports t)
			;; 禁用 Gradle（你使用的是 Maven）
			(setq lsp-java-import-gradle-enabled nil)
			(setq lsp-java-import-maven-enabled t))
			;; 自动构建项目
			;; (setq lsp-java-autobuild-enabled t)

  (add-hook 'java-mode-hook #'lsp-deferred))

;; Purcell Code
(provide 'init-local)
