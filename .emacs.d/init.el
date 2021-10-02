;; enable melpa if it isn't enabled
(require 'package)
(when (not (assoc "melpa" package-archives))
  (setq package-archives (append '(("melpa" . "https://melpa.org/packages/")) package-archives)))
(package-initialize)

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install use-package if it isn't already installed
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install and configure pacakges

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize))
  )

(use-package lsp-mode
  :ensure t
  ;; uncomment to enable gopls http debug server
  ;; :custom (lsp-gopls-server-args '("-debug" "127.0.0.1:0"))
  :config
  ;; use flycheck, not flymake
  (setq lsp-prefer-flymake nil))

;; optional - provides fancy overlay information
(use-package lsp-ui
  :ensure t
  :config
  ;; disable breadcrumbs from top bar
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; disable lsp eldoc from bottom bar
  (setq lsp-eldoc-enable-hover nil))

(use-package company
  :ensure t
  :config
  ;; don't add any delay before trying to complete thing being typed
  ;; the call/response to gopls is asynchronous so this should have little
  ;; to no affect on edit latency
  (setq company-idle-delay 0)
  ;; start completing after a single character instead of 3
  (setq company-minimum-prefix-length 1)
  ;; align fields in completions
  (setq company-tooltip-align-annotations t)
  ;; enable it globally
  (global-company-mode))

;; optional package to get the error squiggles as you edit
(use-package flycheck
  :ensure t)

;; the Go features
(use-package go-mode
  :ensure t
  :after (lsp-mode)
  :hook ((go-mode . lsp-deferred)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

;; the PHP features
(use-package php-mode
  :ensure t
  :after (lsp-mode)
  :hook ((php-mode . lsp-deferred)))

;; yaml helper
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode)
		 ("\\.yaml$" . yaml-mode)))

;; markdown features
(use-package markdown-mode
  :ensure t
  :mode ("\\.markdown$" . markdown-mode)
  :init (add-hook 'markdown-mode-hook 'auto-fill-mode))

;; optional, provides snippets for method signature completion
(use-package yasnippet
  :ensure t
  :after (go-mode)
  :config
  (defun iy-ac-tab-noconflict ()
	(let ((command (key-binding [tab]))) ; remember command
      (local-unset-key [tab]) ; unset from (kbd "<tab>")
      (local-set-key (kbd "TAB") command))) ; bind to (kbd "TAB")
  (add-hook 'prog-mode 'iy-ac-tab-noconflict)
  (yas-global-mode))

;; Git helpers
(use-package magit
  :ensure t
  :config
  (global-set-key [f12] 'magit))
  
(use-package git-gutter+
  :ensure t
  :after (magit)
  :config
  (defun git-gutter-update-all-windows ()
	"Update git-gutter+ information for all visible buffers."
	(interactive)
	(dolist (buf (buffer-list))
	  (when (get-buffer-window buf 'visible)
		(with-current-buffer buf
		  (when git-gutter+-mode
			(git-gutter+-mode)
			(git-gutter+-mode))))))
  (add-hook 'magit-post-refresh-hook 'git-gutter-update-all-windows)
  :hook (
		 (prog-mode . global-git-gutter+-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alter basic emacs behavior that doesn't make sense

;; stop creating temp and auto-save files
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; don't display start-up message
(setq inhibit-startup-screen t)

;; start maximized if that's an option
(toggle-frame-maximized)

;; fix scrolling
(setq scroll-conservatively 10000)

;; disable top menu
(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XP improvements

;; completion and narrowing of selection
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
		 ("C-x C-f" . helm-find-files)
		 ("C-x b" . helm-buffers-list)
		 ("C-s" . helm-occur)
		 ("C-r" . helm-occur)		 
		 ("M-y" . helm-show-kill-ring))
  :config
  (setq completion-styles '(flex))
  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-lsp
  :ensure t)

;; tab should be of width 4
(setq-default tab-width 4)

;; show line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; load the theme
(use-package vscdark-theme
  :ensure t
  :config (load-theme 'vscdark t))

;; seeing through brackets
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; learning keys
(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	   which-key-side-window-max-width 0.33
	   which-key-idle-delay 1.0)
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The rest of this file is edited by emacs itself automatically

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(helm-ag exec-path-from-shell git-gutter+ helm magit vscdark-theme rainbow-delimiters which-key markdown-mode yaml-mode php-mode go-mode flycheck company lsp-ui lsp-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
