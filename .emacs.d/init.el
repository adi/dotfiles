(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f4c8f0b999a6407211a899401315a628e1a5ae2f408c04a33b14d7aa3ed86187" default))
 '(package-selected-packages
   '(git-gutter+ vscode-dark-plus-theme centaur-tabs yasnippet-snippets flycheck projectile yasnippet neotree magit company lsp-ui lsp-mode go-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; enable tabs
(centaur-tabs-mode t)

;; git gutter
(require 'git-gutter+)

;; If you enable global minor mode
(global-git-gutter+-mode t)

(defadvice magit-stage-item (after run-my-after-stage-hooks activate)
  "Run `my-magit-after-stage-hooks` after staging an item in magit."
  (when (called-interactively-p 'interactive)
    (run-hooks 'my-magit-after-stage-hooks)))

(defadvice magit-unstage-item (after run-my-after-unstage-hooks activate)
  "Run `my-magit-after-unstage-hooks` after unstaging an item in magit."
  (when (called-interactively-p 'interactive)
    (run-hooks 'my-magit-after-unstage-hooks)))

(defun my-refresh-visible-git-gutter-buffers ()
  "Refresh git-gutter+-mode on all visible git-gutter-mode buffers."
  (dolist (buff (buffer-list))
    (with-current-buffer buff
      (when (and git-gutter+-mode (get-buffer-window buff))
        (git-gutter+-mode t)))))

(add-hook 'my-magit-after-unstage-hooks
          'my-refresh-visible-git-gutter-buffers)
(add-hook 'my-magit-after-stage-hooks
          'my-refresh-visible-git-gutter-buffers)


;; enable language server interaction
(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)

;; disable lsp breadcrumbs from top bar
(setq lsp-headerline-breadcrumb-enable nil)

;; disable lsp eldoc from bottom bar
(setq lsp-eldoc-enable-hover nil)

;; hide symbol highlights
;;(setq lsp-enable-symbol-highlighting nil)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; activate snippets
(require 'yasnippet)
(yas-global-mode 1)
(defun iy-ac-tab-noconflict ()
  (let ((command (key-binding [tab]))) ; remember command
    (local-unset-key [tab]) ; unset from (kbd "<tab>")
    (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")
(add-hook 'go-mode-hook 'iy-ac-tab-noconflict)

;; activate Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; stop creating temp files
(setq create-lockfiles nil)

;; don't show menu bar
(menu-bar-mode -1)

;; tab should be of width 4
(setq-default tab-width 4)

;; show line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; map NeoTree toggle
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq neo-window-position 'right)

(defun neo-open-file-hide (full-path &optional arg)
  "Open a file node and hides tree."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Enters file and hides neotree directly"
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

(add-hook
 'neotree-mode-hook
 (lambda ()
   (define-key neotree-mode-map (kbd "RET") 'neotree-enter-hide)))

;; set vscode theme
(load-theme 'vscode-dark-plus t)