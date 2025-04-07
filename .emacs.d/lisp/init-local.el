;; 本地环境
(set-locale-environment "en_US.UTF-8")
;; 这个是为了在 org mode 中用英文显示日期，默认是中文
(setq system-time-locale "C")

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; 使用 use-package 安装和配置 evil
(use-package evil
  :ensure t
  ;; 可选：在 Emacs 启动时直接启用 evil-mode
  :init
  :config
  ;; 启用 Evil 模式
  (evil-mode 1))

;; theme
(use-package catppuccin-theme
  :ensure t)
(setq catppuccin-flavor 'macchiato)
(catppuccin-reload)

;;;; font & size ;;;;
;; use cnfonts
(use-package cnfonts
  :ensure t
  :config
  (cnfonts-enable))

;; config lsp for python
(use-package lsp-mode
  :hook (python-mode . lsp)
  commands lsp)
(use-package lsp-pyright
  :ensure t
  :config
  (setq lsp-pyright-typechecking-mode "basic"))

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
  "f f"   'find-file
  "m g"   'magit-status
  "x b"   'switch-to-buffer
  "x c"   'save-buffers-kill-terminal
  "x k"   'kill-buffer
  "x s"   'save-buffer
  "x x"   'execute-extended-command)

;; Purcell Code
(provide 'init-local)
