;; -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; 0. Package system bootstrap
;;; ---------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; ---------------------------------------------------------------------------
;;; 1. Frame / UI basics
;;; ---------------------------------------------------------------------------
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-buffer-choice t
      initial-major-mode 'fundamental-mode
      ring-bell-function 'ignore
      display-time-default-load-average nil
      scroll-margin 0
      use-dialog-box nil
      frame-title-format ""
      ns-use-proxy-icon nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(internal-border-width . 8))
(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(dolist (mode '(tool-bar-mode scroll-bar-mode menu-bar-mode blink-cursor-mode))
  (funcall mode 0))

(setq auth-sources '("~/.authinfo")
      custom-safe-themes t)

(use-package spacious-padding
  :ensure t
  :config
  ;;(spacious-padding-mode)
)

;;; ---------------------------------------------------------------------------
;;; 2. Fonts / typography
;;; ---------------------------------------------------------------------------
(defvar my/variable-pitch-font "Aporetic Sans")
(defvar my/fixed-pitch-font "Aporetic Sans Mono")

(add-to-list 'default-frame-alist `(font . ,(format "%s-14" my/fixed-pitch-font)))
(add-to-list 'default-frame-alist `(variable-pitch . ,(format "%s-14" my/variable-pitch-font)))

;;; ---------------------------------------------------------------------------
;;; 3. Core editing behavior
;;; ---------------------------------------------------------------------------
(delete-selection-mode 1)
(global-so-long-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(which-key-mode 1)

(setq auto-revert-interval 1
      auto-revert-verbose nil
      echo-keystrokes 0.1
      frame-inhibit-implied-resize 1
      sentence-end-double-space nil
      recentf-max-saved-items 1000
      use-short-answers t
      save-interprogram-paste-before-kill t
      history-length 25
      global-auto-revert-non-file-buffers t
      auto-save-default nil
      backup-directory-alist '(("." . "~/.saves"))
      backup-by-copying t
      visible-bell nil
      ring-bell-function #'ignore
      create-lockfiles nil)

(global-display-line-numbers-mode 1)
(dolist (hook '(org-mode-hook term-mode-hook vterm-mode-hook eat-mode-hook
                shell-mode-hook treemacs-mode-hook eshell-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;;; ---------------------------------------------------------------------------
;;; 4. Global keybindings
;;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "s-=") (lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

;;; ---------------------------------------------------------------------------
;;; 6. Org mode
;;; ---------------------------------------------------------------------------

;; Org-agenda configuration
(setq org-agenda-files '("~/org/roam")
      org-agenda-start-on-weekday nil
      org-agenda-span 7
      org-deadline-warning-days 7
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t)

;; Keybindings for org
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-list '(bullet checklist))
  (setq org-modern-star '("•"))
  (setq org-modern-block-fringe t
        org-modern-block-name t
        org-modern-timestamp t
        org-modern-keyword t
        org-modern-table t))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle))
  :config
  (org-roam-setup)

  ;; Org-roam capture templates (for new notes)
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("n" "note" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("j" "journal" plain "* %<%H:%M> %?\n"
           :target (file+head "journal/%<%Y%m%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :journal:\n\n")
           :unnarrowed t))))

;; Org-capture for todos (simple append to todo.org)
(setq org-capture-templates
      '(("t" "Todo" entry
         (file "~/org/roam/todo.org")
         "* TODO %?\nDEADLINE: %^{Deadline}t\n%i\n%a"
         :empty-lines 1
         :prepend t)
        ("T" "Todo (no deadline)" entry
         (file "~/org/roam/todo.org")
         "* TODO %?\n%i\n%a"
         :empty-lines 1
         :prepend t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(use-package mixed-pitch
  :ensure t
  :hook ((org-mode . mixed-pitch-mode)
         (LaTeX-mode . mixed-pitch-mode)))

;;; ---------------------------------------------------------------------------
;;; 7. UI packages (themes, modeline, auto-dark)
;;; ---------------------------------------------------------------------------
(defvar brian/default-dark-theme 'modus-vivendi)
(defvar brian/default-light-theme 'modus-operandi-tinted)
(defvar brian/default-dark-accent-colour "SkyBlue4")
(defvar brian/default-light-accent-color "#8fafe3")

(load-theme brian/default-light-theme t)

(use-package auto-dark
  :ensure t :init (auto-dark-mode 1)
  :hook
  (auto-dark-dark-mode
   . (lambda ()
       (mapc #'disable-theme custom-enabled-themes)
       (load-theme brian/default-dark-theme t)
       (custom-set-faces `(eval-sexp-fu-flash ((t (:background ,brian/default-dark-accent-colour)))))))
  (auto-dark-light-mode
   . (lambda ()
       (mapc #'disable-theme custom-enabled-themes)
       (load-theme brian/default-light-theme t)
       (custom-set-faces `(eval-sexp-fu-flash ((t (:background ,brian/default-light-accent-color))))))))

(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 0.8)
  :bind (("C-c o" . olivetti-mode)))

(use-package adaptive-wrap :ensure t :hook (visual-line-mode . adaptive-wrap-prefix-mode))
(use-package nerd-icons :ensure t)
(use-package autothemer :defer t)

;;; ---------------------------------------------------------------------------
;;; 8. Completion / navigation stack
;;; ---------------------------------------------------------------------------
(use-package vertico :ensure t :config (vertico-mode) (vertico-multiform-mode))
(use-package marginalia :ensure t :init (marginalia-mode))
(use-package orderless :ensure t :custom (completion-styles '(orderless basic)))
(use-package consult
  :ensure t
  :bind (("C-s"     . consult-line)
         ("C-x C-b" . consult-buffer)
         ("C-c h"   . consult-history)
         ("C-c s"   . consult-imenu)
         ("C-c d"   . consult-flymake)))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-require-match nil)
  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ([tab] . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;;; ---------------------------------------------------------------------------
;;; 9. Window management
;;; ---------------------------------------------------------------------------
(use-package window :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 0))
     ("\\*\\(lsp-help\\)\\*" 
      (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 0))
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window) (window-height . 0.25) (side . bottom) (slot . 1)))))

(use-package ace-window :ensure t :bind (("M-o" . ace-window)))
(use-package avy :ensure t :bind (("C-c j" . avy-goto-char-timer)
                                  ("C-c w" . avy-goto-word-0)))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll" :rev :newest)
  :init (setq scroll-conservatively 101 scroll-margin 0)
  :config (ultra-scroll-mode 1))

;;; ---------------------------------------------------------------------------
;;; 10. AI tools
;;; ---------------------------------------------------------------------------
(use-package gptel :ensure t
  :config
  (setq gptel-default-model "gpt-4"
        gptel-system-message "You are a helpful assistant.")
  (global-set-key (kbd "C-c <return>") 'gptel-send)
  (global-set-key (kbd "C-c C-<return>") 'gptel-menu)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell"))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el")
  :config
  (setq copilot-node-executable "node")
  (define-key copilot-completion-map (kbd "<tab>") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") #'copilot-accept-completion))

;;; ---------------------------------------------------------------------------
;;; 11. LSP + programming languages
;;; ---------------------------------------------------------------------------
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil
        lsp-signature-auto-activate nil
        lsp-disabled-clients '(ruby-ls rubocop-ls typeprof-ls steep-ls solargraph-ls
                               srb-ls semgrep-ls stree-ls pylsp mspyls))
  :hook ((ruby-ts-mode . lsp)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)))

(use-package lsp-pyright :ensure t :after lsp-mode)
(use-package lsp-ui :ensure t)

(use-package tree-sitter :ensure t :config (global-tree-sitter-mode))
(use-package tree-sitter-langs :ensure t :after tree-sitter)

(setq major-mode-remap-alist '((python-mode . python-ts-mode)))
(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

(use-package ruby-mode :ensure t :init (setq ruby-indent-level 2))
(use-package rspec-mode :ensure t :after ruby-mode)
(use-package rust-mode :ensure t :hook (rust-mode . (lambda ()
                                                      (setq indent-tabs-mode nil
                                                            rust-format-on-save t))))

;;; ---------------------------------------------------------------------------
;;; 12. Git tooling
;;; ---------------------------------------------------------------------------
(use-package magit :ensure t)
(use-package forge :ensure t :after magit)
(use-package git-link :ensure t)
(use-package blamer :ensure t)

;;; ---------------------------------------------------------------------------
;;; 13. Treesitter language sources
;;; ---------------------------------------------------------------------------
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(heex "https://github.com/phoenixframework/tree-sitter-heex")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;; ---------------------------------------------------------------------------
;;; 14. Misc utilities
;;; ---------------------------------------------------------------------------
(use-package imenu-list :ensure t :bind (("M-g i" . imenu-list-smart-toggle)))
(use-package expand-region :ensure t :bind (("C-c SPC" . er/expand-region)))

;;; ---------------------------------------------------------------------------
;;; 15. Custom functions
;;; ---------------------------------------------------------------------------
(defun open-init-file () (interactive) (find-file user-init-file))

(defun run-standardrb-on-current-file ()
  (interactive)
  (let* ((project-root (locate-dominating-file default-directory ".git"))
         (current-file (buffer-file-name))
         (cmd (concat (expand-file-name "bin/standardrb" project-root)
                      " " (shell-quote-argument current-file)
                      " --fix-unsafely")))
    (if (and project-root current-file)
        (shell-command cmd)
      (message "Could not find project root or current file."))))

;;; ---------------------------------------------------------------------------
;;; 16. Custom (auto-generated)
;;; ---------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window adaptive-wrap agent-shell-sidebar auto-dark blamer cape
		catppuccin-theme claude-code-ide company
		consult-denote copilot corfu denote-menu direnv eat
		evil-collection evil-nerd-commenter evil-org
		evil-surround exec-path-from-shell expand-region forge
		git-link gptel imenu-list lsp-pyright lsp-ui
		marginalia mixed-pitch multiple-cursors nerd-icons
		olivetti orderless org-modern org-roam poet-theme rg
		rspec-mode rust-mode south-theme spacious-padding
		tao-theme tree-sitter-langs ultra-scroll vertico vterm))
 '(package-vc-selected-packages
   '((agent-shell :url "https://github.com/xenodium/agent-shell")
     (acp :url "https://github.com/xenodium/acp.el")
     (claude-code :url
		  "https://github.com/stevemolitor/claude-code.el")
     (ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll"
		   :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
	      :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(eval-sexp-fu-flash ((t (:background "#8fafe3")))))
