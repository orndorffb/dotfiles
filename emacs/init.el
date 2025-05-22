
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun open-init-file ()
  "Open the user's Emacs init file."
  (interactive)
  (find-file user-init-file))

;;--https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4
;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    :height 140 :family "Essential PragmataPro")

;; --- Activate / Deactivate modes --------------------------------------------
(tool-bar-mode -1) (menu-bar-mode -1) (blink-cursor-mode -1)
(global-hl-line-mode 0) (global-display-line-numbers-mode 1)
(pixel-scroll-precision-mode 1)

;;--My stuff--------------------------------------------------------------------

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(setq default-frame-alist
      '((left-fringe . 0)
        (right-fringe . 0)
        (vertical-scroll-bars . nil)
        (bottom-divider-width . 0)
        (right-divider-width . 0))
      )
(add-to-list 'default-frame-alist '(undecorated-round . t))

(add-hook 'window-setup-hook 'toggle-frame-maximized)

(load-theme 'modus-operandi)

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

(delete-selection-mode 1)
(setq compilation-scroll-output t)

(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)
(setq visible-bell       nil
      ring-bell-function #'ignore)

;; no backup files
(setq-default make-backup-files nil)
;; no lockfiles
(setq create-lockfiles nil)

;; Some keybinds for basic stuff
(global-set-key (kbd "C-c s") 'consult-ripgrep)
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "C-c S") 'consult-imenu)
(global-set-key (kbd "C-c c") 'comment-dwim)
(global-set-key (kbd "C-c f") 'rg-dwim)

;; Org stuff
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(global-set-key (kbd "C-c C-c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(defun run-standardrb-on-current-file ()
  "Run <project_root>/bin/standardrb <current_file> --fix-unsafely."
  (interactive)
  (let* ((project-root (locate-dominating-file default-directory ".git")) ; Adjust this to your project's root indicator
         (current-file (buffer-file-name))
         (standardrb-command (concat (expand-file-name "bin/standardrb" project-root)
                                     " " (shell-quote-argument current-file)
                                     " --fix-unsafely")))
    (if (and project-root current-file)
        (shell-command standardrb-command)
      (message "Could not find project root or current file."))))

(use-package expand-region
  :ensure t
  :bind (("C-;" . er/expand-region)))

;; Taken from https://github.com/LionyxML/emacs-kick/blob/master/init.el#L301C1-L333C9
(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll"
            :rev :newest
            :branch "main")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))


(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (setq copilot-node-executable "node")
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(defun my-get-openai-api-key ()
  "Retrieve OpenAI API key from authinfo."
  (let ((auth (car (auth-source-search :host "api.openai.com"
                                       :user "apikey"
                                       :require '(:secret)))))
    (when auth
      (funcall (plist-get auth :secret)))))

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "OPENROUTER_API_KEY" (my-get-openai-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "o1"))

(use-package gptel
  :ensure t
  :config
  (setq gptel-default-model "gpt-4")
  (setq gptel-system-message "You are a helpful assistant.")
  (global-set-key (kbd "C-c C-<return>") 'gptel-menu)
  (global-set-key (kbd "C-c RET") 'gptel-send)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-format-on-save-mode t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-disabled-clients '(ruby-ls rubocop-ls typeprof-ls steep-ls solargraph-ls srb-ls semgrep-ls stree-ls))
  :hook
   (ruby-ts-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover t
        lsp-ui-imenu-enable t))

(use-package eglot
  :ensure t
  :hook (;(ruby-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (elixir-mode . eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (setq eglot-server-programs '(
                                ;;(ruby-mode . ("ruby-lsp"))
                                ;;(rust-mode . ("rust-analyzer"))
                                ;;(elixir-mode . ("elixir-ls"))
                                )))

(setq eglot-workspace-configuration
      '((solargraph (diagnostics . t))))

(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package magit
  :ensure t
  :config
  (setq magit-save-repository-buffers nil))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  :init
  (global-corfu-mode))

;; Rust mode setup
(use-package rust-mode
  :ensure t
  :hook (rust-mode . (lambda ()
                       (setq indent-tabs-mode nil) ; Use spaces instead of tabs
                       (setq rust-format-on-save t)))) ; Format on save


;; Use ruby-ts-mode instead of ruby-mode
(when (treesit-available-p)
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

(use-package ruby-mode
  :ensure t
  :init
  (setq ruby-indent-level 2))

(use-package rspec-mode
  :ensure t
  :after ruby-mode
  :config
  (setq rspec-use-spring-when-possible nil)
  (add-hook 'ruby-mode-hook 'rspec-mode))

  
(use-package ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-ignore-current t)
  (setq aw-dispatch-always nil)
  (setq aw-minibuffer-flag t)
  :bind (( "M-o" . ace-window)))

(use-package avy
  :ensure t
  :config
  (defun avy-goto-word-crt-line ()
    "Jump to a word start on the current line only."
    (interactive)
    (avy-with avy-goto-word-0
			  (avy-goto-word-0 nil (line-beginning-position) (line-end-position))))
  :bind (
		 ("C-'" . avy-goto-char-timer)
		 ("C-c w" . avy-goto-word-crt-line)
		 ))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (setq vertico-multiform-commands
		'((consult-project-buffer posframe)
		  (consult-buffer posframe)
		  (execute-extended-command posframe)
		  (project-find-file posframe)
		  ))
  :config
  (vertico-mode)
  (vertico-multiform-mode))

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
		 ("C-c d" . consult-flymake)
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
         ("M-s g" . consult-ripgrsep)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-ipsearch-history)
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

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "af238e93bc03da7ee4b2d30f2b3ea6e1553eb05b7d827da83bf35be1f6401992"
     "b350d78e608ff87218a78f62c9832e1710714c7279321fa72a3da889bfe3d408"
     "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1"
     "aa545934ce1b6fd16b4db2cf6c2ccf126249a66712786dd70f880806a187ac0b"
     "d0a4b929a8ca0715fc1db5eda6effc17b2ce4427809f245028a88a949429d50e"
     "622a74a498b3362ca51f23eea7b1efba62fc493267f2b0456751b053f3872db0"
     "9e296dbc86374778cca0f22cfd7cd44d35e7c2e678085417be97251ce7c75dcc"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     "d2ab3d4f005a9ad4fb789a8f65606c72f30ce9d281a9e42da55f7f4b9ef5bfc6"
     "fbf73690320aa26f8daffdd1210ef234ed1b0c59f3d001f342b9c0bbf49f531c"
     "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7"
     "0f76f9e0af168197f4798aba5c5ef18e07c926f4e7676b95f2a13771355ce850"
     "61607956384e528c1bc3ca5c9b703b309d4b3a63acfec3edb7f9a26549262add"
     "e8195801e30a76a2db6cbebfadde82311cfcdd365aaeacee915658fa099d661f"
     "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
     "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185"
     default))
 '(package-selected-packages
   '(ace-window aidermacs consult copilot corfu doom-themes eat
		exec-path-from-shell expand-region gptel lsp-ui magit
		marginalia mise olivetti orderless rbenv rg robe
		rspec-mode rust-mode stimmung-themes tree-sitter-langs
		ultra-scroll vertico-posframe vterm zoom-window))
 '(package-vc-selected-packages
   '((ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll"
		   :branch "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
	      :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
