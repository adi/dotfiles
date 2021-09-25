(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets flycheck projectile yasnippet neotree magit company lsp-ui lsp-mode go-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
