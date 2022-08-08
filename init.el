;;; Init.el --- summary -*- lexical-binding:t -*-
;;; Commentary:
;;; TODO: have a better description including minimum requirements
;;; TODO: make paths using user-emacs-directory
;;; TODO: make this org and refactor
;;; TODO: fix yasnippet to be more friendly in auto-complete-mode
;;; TODO: optimize load-time
;;; This is my personal Emacs configuration
;;; Code:


;; The size of allocated bytes for each garbage collection
(setq gc-cons-threshold (* 100 1024 1024))   ;; 100mb, the default is 800 kilobytes
;; Maximum number of bytes to read from a subprocess in a single chuck
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Set the eln-cache path inside .cache directory
;(setq native-comp-eln-load-path '("~/.config/emacs/.cache/temp/eln-cache/")); "/usr/lib/emacs/28.1/native-lisp/"))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Change the user-emacs-directory to keep unwanted things out of ~/.config/emacs
; (setq user-emacs-directory (expand-file-name "~/.config/emacs/.cache/")
;       url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Take the "customize interface" generated lines out of here
(setq custom-file
      (expand-file-name
       (format "emacs-custom-%s.el" (user-uid)) "~/.config/emacs/.cache/customization"))
(load custom-file t)

;; Info
(setq user-full-name "Ushita Mohammadi")
(setq user-mail-address
      "ushita.mohammadi@gmail.com")

;; Aspell, a spell-checker
(setq ispell-program-name "aspell")

;; turning cl-lib warns off
(setq byte-compile-warnings '(cl-functions))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)
(setq warning-suppress-types '((comp)))

;; More minimalistic view
;; & some minor visual enhancements
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(set-fringe-mode 10)                           ; give some breathing room
;; (global-linum-mode 1)                          ; set line numbers on globally
(global-hl-line-mode 1)                        ; set the line highlighted
(set-face-attribute 'default nil               ; set the font to Fira
		    :font "Fira Code Retina"
		    :height 101)
(load-theme 'modus-vivendi)		; set the theme to modus-vivendi

;; having last 25 files opened with emacs
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 25)
;; (setq recentf-max-saved-items 25)

;; make the links clickable
(global-goto-address-mode 1)

;; remember the last cursor position
;;(save-place-mode 1)

;; Write backup files to one directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
		 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Set locale to UTF8
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Setting the org-mode as initial mode
(setq initial-major-mode 'org-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; set base dir of straight inside .cache folder
(setq straight-base-dir "~/.config/emacs/.cache/var/")


;; Bootstrap 'straight'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" "~/.config/emacs/.cache/var/"))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; setting use-package to use straight
(setq straight-use-package-by-default t)
;; keeping emacs away from package in favor of straight
;(setq package-enable-at-startup nil)
;; install 'use-package' with straight
(straight-use-package 'use-package)


;;; Packages

;; Use no-littering to automatically set common paths
;; to the new user-emacs-directory
(setq no-littering-etc-directory
      (expand-file-name ".cache/etc/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name ".cache/var/" user-emacs-directory))
(use-package no-littering)
;; having last 25 files opened with emacs
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
;; remember the last cursor position
(save-place-mode 1)




(setq auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Appearance
 (use-package doom-modeline
   :hook (after-init . doom-modeline-mode))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Utilities
(use-package multiple-cursors
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package tree-sitter
  :init
  (global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package fira-code-mode
  :delight
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode)

(use-package consult
  :bind (
	 ("C-<backspace>" . consult-recent-file)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)

         ("M-g f" . consult-flycheck)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         
         ("M-s e" . consult-isearch-history)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h b" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
		     :includes (vertico-directory))
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides 'nil))

(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'consult-completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

(use-package delight)

(use-package zenity-color-picker ;; you need to have zenity installed
  :bind
  ("C-c c c" . zenity-cp-color-at-point-dwim))

(use-package apheleia
  :delight
  :config
  (apheleia-global-mode +1))

(use-package corfu
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-scroll-margin 5)
  (corfu-cycle t)
  (corfu-preview-current nil)  		; default == 'insert
  :init
  (global-corfu-mode))

(use-package cape
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
	 )
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package smartparens
  :delight
  :hook
  (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package devdocs
  :config
  (global-set-key (kbd "C-c d d") 'devdocs-lookup))

(use-package yasnippet
  :delight
  :custom
  (yas-snippet-dirs
   '("~/.config/emacs/snippets"))      ;; personal snippets
  :config
  (yas-global-mode 1))

(use-package consult-yasnippet
  :after (consult yasnippet))

(use-package react-snippets)

(use-package yasnippet-snippets)

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package flycheck
  :after lsp
  :hook lsp-mode)

(use-package flycheck-rust)

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-lsp
  :after (lsp consult))

(use-package cargo)

;; programming supports

(use-package lsp-mode
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :hook (((mhtml-mode
	  css-mode
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
	  rust-mode       ; rust-analyzer
          web-mode        ; vue
          ) . lsp-deferred)
	 (lsp-completion-mode . my/lsp-mode-setup-completion))
  :commands lsp
  :custom (lsp-completion-provider :none) ; we use corfu
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setenv "TSSERVER_LOG_FILE"
	  (expand-file-name ".cache/temp/lsp-log/tsserver.log" user-emacs-directory))
  (setq lsp-restart 'auto-restart)
  (setq lsp-idle-delay 0.5))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
;  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(use-package magit
  :defer t)

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package skewer-mode
  :hook js2-mode (css-mode . skewer-css-mode) (html-mode . skewer-html-mode))

(use-package impatient-mode
  :hook mhtml-mode web-mode css-mode js2-mode)

(use-package origami
  :config
  (global-origami-mode 1)
  :bind
  ("C-c C-<return>" . 'origami-toggle-node)
  ("C-c C-c C-<return>" . 'origami-toggle-all-nodes))

(use-package citre
  :defer t
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package mhtml-mode
  :config
  (add-hook 'mhtml-mode-hook
          (lambda () (setq-local devdocs-current-docs '("html" "tailwindcss"))))
  :bind
  ([f5] . sgml-validate))

(use-package css-mode
  :custom
  (css-indent-offset 2)
  :config
  (add-hook 'css-mode-hook
            (lambda () (setq-local devdocs-current-docs '("css" "tailwindcss" "sass")))))

(use-package json-mode)

(use-package web-mode
  :mode ("\\.vue\\'" . web-mode)
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :config
  (add-hook 'web-mode-hook
            (lambda () (setq-local devdocs-current-docs '("vue~3" "vuex~4" "vue_router~4")))))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook
            (lambda () (setq-local devdocs-current-docs '("javascript" "react" "redux" "react_router" "express" "node" "tailwindcss" "dom"))))
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  (js2-highlight-level 3)
  ;; (js-indent-level 2)
  )

(use-package rjsx-mode)

(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript")))))

(use-package emmet-mode
  :init
  (setq emmet-move-cursor-between-quotes t) 
  :hook
  (mhtml-mode css-mode web-mode js-mode))

(use-package markdown-mode
  :mode (("readme\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package rust-mode
  :hook
  (cargo-minor-mode)
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq-local devdocs-current-docs '("rust")))))

;;; init.el ends here
