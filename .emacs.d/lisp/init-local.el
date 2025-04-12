;; 本地环境
(set-locale-environment "en_US.UTF-8")
;; 这个是为了在 org mode 中用英文显示日期，默认是中文
(setq system-time-locale "C")

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; theme
(use-package catppuccin-theme
  :ensure t)
(setq catppuccin-flavor 'frappe)
(catppuccin-reload)

(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-animate-nyancat t)
  (nyan-mode 1)
  :commands (nyan-mode))

;;;; font & size ;;;;
;; use cnfonts
;; 只在 gui 界面调试字体，其他交给终端
(when (display-graphic-p)
  (use-package cnfonts
    :ensure t
    :config
    (cnfonts-enable)))

(setq evil-want-keybinding nil)
;; 使用 use-package 安装和配置 evil
(use-package evil
  :ensure t
  :init
  :config
  (setq evil-want-integration t)   ;; 启用基本集成
  (setq evil-want-keybinding nil)  ;; 禁用自动绑定配合 evil-collection
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil                      ;; 确保 evil 先加载
  :ensure t
  :config
  (evil-collection-init))          ;; 初始化默认的 evil 快捷键绑定

;; 文件：init-treemacs.el
;; 说明：集成 treemacs 文件管理器，用于侧边栏浏览项目文件结构
(use-package treemacs
  :ensure t               ;; 如果包未安装，自动从包仓库安装
  :defer t                ;; 延迟加载（直到调用相关命令）
  :init
  :config)
(use-package treemacs-evil
  :after (treemacs evil)    ;; 需要在 treemacs 和 evil 加载后加载
  :ensure t)                ;; 集成 treemacs 和 evil 的快捷键绑定
(use-package treemacs-projectile
  :after (treemacs projectile)  ;; 集成 treemacs 与项目管理工具 projectile
  :ensure t)

;; 安装 general 自定义快捷键
(unless (package-installed-p 'general)
  (package-refresh-contents)
  (package-install 'general))
(require 'general)
;; 自定义 general
(general-create-definer my-space-leader-def
                        ;; insert mode
                        :states '(normal visual motion emacs)
                        ;; 定义 Space 为 Leader 键
                        :prefix "SPC"
                        ;; 防止被覆盖
                        :keymaps 'override)
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
  "x x" 'execute-extended-command)

;; 配置 org 中的 text 内容可以被 python3 读取
(defun org-babel-execute:text (body params) body)
;; text 运行时不需要再询问
(setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (member lang '("text")))))
;; 支持 python3 的运行
(setq org-babel-python-command "python3")

;;;; Python + Flask 配置
;; LSP 配置
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp))
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; pyvenv 配置
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

;; Flask 项目运行命令
(defun my/python-flask-run ()
  "Run current Flask app"
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (compile "FLASK_APP=app.py FLASK_ENV=development flask run")))
;; debug for python
(use-package dap-mode
  :ensure t
  :config
  (require 'dap-python))

;; Purcell Code
(provide 'init-local)
