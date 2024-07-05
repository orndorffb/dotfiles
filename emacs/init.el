(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(eval-when-compile
  (require 'use-package))

;; Basic editor settings
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode)
(set-fringe-mode 10)

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)
(setq visible-bell       nil
      ring-bell-function #'ignore)

;; no backup files
(setq-default make-backup-files nil)
;; no lockfiles
(setq create-lockfiles nil)

;; Set font
(set-frame-font "Iosevka Fixed 13" nil t)

;; Org stuff
(add-hook 'org-mode-hook 'org-indent-mode)

(use-package exec-path-from-shell
  :ensure t)

(use-package ef-themes
  :ensure
  :init
  (load-theme 'ef-dream))

;; Lsp stuff
(use-package eglot
  :ensure t
  :bind (("C-c g r" . xref-find-references)
	 ("C-c g d" . xref-find-definitions)))

(setq eglot-workspace-configuration
  '((solargraph (diagnostics . t))))

;; Custom functions
(defun eglot-restart ()
  "Shutdown the buffer's lsp server and restarts it"
  (interactive)
  (eglot-shutdown)
  (eglot))

;; (use-package yasnippet
;;   :ensure t)

;; (use-package yasnippet-snippets
;;   :ensure t)

(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package expand-region
  :bind (("C-c s" . er/expand-region))
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-save-repository-buffers nil))

(use-package go-mode
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode))

(use-package ruby-mode
  :ensure t
  :init
  (setq ruby-indent-level 2)
  :config
  (defun set-ruby-breakpoint ()
    "Add 'require \"debug\"' at the top of the file and 'binding.break' above the current line."
    (interactive)
    (save-excursion
      ;; Add 'require "debug"' at the top of the file if it's not already present
      (goto-char (point-min))
      (unless (search-forward "require \"debug\"" nil t)
	(goto-char (point-min))
	(insert "require \"debug\"\n\n"))
      )

    ;; Add 'binding.break' above the current line
    (let ((current-line (line-number-at-pos)))
      (goto-line current-line)
      (beginning-of-line)
      (open-line 1)
      (insert "binding.break")))
(define-key ruby-mode-map (kbd "C-c b") 'set-ruby-breakpoint))

(use-package elixir-mode
  :ensure t)

(use-package ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  (setq aw-dispatch-always nil)
  (setq aw-minibuffer-flag t)
  :bind (( "M-o" . ace-window)))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-char-2)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (setq vertico-multiform-commands
	'((consult-ripgrep buffer)
	  (consult-find buffer)
	  (consult-imenu buffer)
	  (consult-xref buffer)))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

;; Currently disabled
(use-package vertico-posframe
  :ensure t
  :init)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
	 ("C-s" . consult-line)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s f" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s G" . consult-git-grep)
         ("M-s g" . consult-ripgrep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init

  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; "C-+"
)

  
;; Get path settings from zsh
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855" "d4b608d76e3a087b124c74c2b642c933d8121b24e53d4bbd5e7327c36cc69ccc" "159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954" "841b6a0350ae5029d6410d27cc036b9f35d3bf657de1c08af0b7cbe3974d19ac" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "80214de566132bf2c844b9dee3ec0599f65c5a1f2d6ff21a2c8309e6e70f9242" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" default))
 '(package-selected-packages
   '(ef-themes vterm sublime-themes corfu rg robe catppuccin-theme ivy-xref expand-region company exec-path-from-shell counsel ivy use-package magit lsp-mode eglot)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
