(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defun open-init-file ()
  "Open the user's Emacs init file."
  (interactive)
  (find-file user-init-file))

(dolist (mode
         '(tool-bar-mode       ;; Remove toolbar
           scroll-bar-mode     ;; Remove scollbars
           menu-bar-mode       ;; Remove menu bar
           blink-cursor-mode)
	   global-display-line-numbers-mode) ;; Solid cursor, not blinking
  (funcall mode 0))

(setq inhibit-startup-message           t       ;; No startup message
      inhibit-startup-echo-area-message t       ;; No startup message in echo area
      inhibit-startup-screen            t       ;; No default startup screen
      initial-buffer-choice             t       ;; *scratch* is default startup buffer
      initial-major-mode                'fundamental-mode
      ring-bell-function                'ignore ;; No bell
      display-time-default-load-average nil     ;; Don't show me load time
      scroll-margin                     0       ;; Space between top/bottom
      use-dialog-box                    nil)    ;; Disable dialog

(add-to-list 'default-frame-alist     '(fullscreen . maximized))  ; don't use proxy icon
  (setq ns-use-proxy-icon nil)
  ; don't show buffer name in title bar
  (setq frame-title-format "")

(setq auth-sources '("~/.authinfo"))

;;--https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4
;; --- Typography stack -------------------------------------------------------

(defvar brian/font-height 115)

(when (eq system-type 'darwin)
  (setq brian/font-height 120))

(when (member "Fragment Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Fragment Mono" :height brian/font-height)
  (set-face-attribute 'fixed-pitch nil :family "Fragment Mono"))

(when (member "Open Sans" (font-family-list))
  (set-face-attribute 'variable-pitch nil :family "Open Sans"))

;; (set-face-attribute 'default nil
;;                     :height 160 :weight 'regular :family "Essential PragmataPro")

;;--General editing
(delete-selection-mode   t) ;; Replace selected text when yanking
(global-so-long-mode     t) ;; Mitigate performance for long lines
(global-auto-revert-mode t) ;; Revert buffers automatically when they change
(recentf-mode            t) ;; Remember recently opened files
(savehist-mode           t) ;; Remember minibuffer prompt history
(save-place-mode         t) ;; Remember last cursor location in file

(setq auto-revert-interval         1         ;; Refresh buffers fast
      auto-revert-verbose          nil       ;; Don't notify me about reverts
      echo-keystrokes              0.1       ;; Show keystrokes fast
      frame-inhibit-implied-resize 1         ;; Don't resize frame implicitly
      sentence-end-double-space    nil       ;; No double spaces
      recentf-max-saved-items      1000      ;; Show more recent files
      use-short-answers            t         ;; 'y'/'n' instead of 'yes'/'no' etc.
      save-interprogram-paste-before-kill t  ;; Save copies between programs
      history-length               25        ;; Only save the last 25 minibuffer prompts
      global-auto-revert-non-file-buffers t) ;; Revert Dired and other buffers


;;--My stuff--------------------------------------------------------------------

(setq package-install-upgrade-built-in t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
(global-set-key (kbd "C-c p") 'project-find-file)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (exec-path-from-shell-initialize))


(use-package nerd-icons
  :ensure t)

(use-package south-theme
  :vc (:url "https://github.com/SophieBosio/south"
       :rev :newest
       :branch "main"))

(defvar brian/default-dark-theme  'doom-nord)
(defvar brian/default-light-theme 'south)

(defvar brian/default-dark-accent-colour  "SkyBlue4")
(defvar brian/default-light-accent-colour "#D9EDFC")

(load-theme brian/default-dark-theme t)

(use-package autothemer
  :defer t)

(use-package auto-dark
  :ensure t
  :hook ((auto-dark-dark-mode
          .
          (lambda ()
            (interactive)
            (progn
              (custom-set-faces
               `(eval-sexp-fu-flash
                 ((t (:background
                      ,brian/default-dark-accent-colour)))))
              `(load-theme ,brian/default-dark-theme t))))
         (auto-dark-light-mode
          .
          (lambda ()
            (interactive)
            (progn
              (custom-set-faces
               `(eval-sexp-fu-flash
                 ((t (:background
                      ,soph/default-light-accent-colour)))))
              `(load-theme ,soph/default-light-theme t)))))
  :custom
  (auto-dark-themes                   `((,brian/default-dark-theme) (,brian/default-light-theme)))
  (auto-dark-polling-interval-seconds 5)
  (auto-dark-allow-osascript          t)
  :init (auto-dark-mode t))


(defvar lsp-modeline--code-actions-string nil)

(setq-default mode-line-format
  '("%e"
	(:propertize " " display (raise +0.4)) ;; Top padding
	(:propertize " " display (raise -0.4)) ;; Bottom padding

	(:propertize "λ " face font-lock-comment-face)
	mode-line-frame-identification
	mode-line-buffer-identification

	;; Version control info
	(:eval (when-let (vc vc-mode)
			 ;; Use a pretty branch symbol in front of the branch name
			 (list (propertize " ≡ " 'face 'font-lock-comment-face)
                   ;; Truncate branch name to 50 characters
				   (propertize (truncate-string-to-width
                                (substring vc 5) 50)
							   'face 'font-lock-comment-face))))

	;; Add space to align to the right
	(:eval (propertize
			 " " 'display
			 `((space :align-to
					  (-  (+ right right-fringe right-margin)
						 ,(+ 3
                             (string-width (or lsp-modeline--code-actions-string ""))
                             (string-width "%4l:3%c")))))))

    ;; LSP code actions
    (:eval (or lsp-modeline--code-actions-string ""))
	;; Line and column numbers
    (:propertize "%4l:%c" face mode-line-buffer-id)))

;; Line splitting sutff
(defun split-window-sensibly-prefer-horizontal (&optional window)
"Based on `split-window-sensibly', but prefers to split WINDOW side-by-side."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
         ;; Split window horizontally
         (with-selected-window window
           (split-window-right)))
    (and (window-splittable-p window)
         ;; Split window vertically
         (with-selected-window window
           (split-window-below)))
    (and
         ;; If WINDOW is the only usable window on its frame (it is
         ;; the only one or, not being the only one, all the other
         ;; ones are dedicated) and is not the minibuffer window, try
         ;; to split it horizontally disregarding the value of
         ;; `split-height-threshold'.
         (let ((frame (window-frame window)))
           (or
            (eq window (frame-root-window frame))
            (catch 'done
              (walk-window-tree (lambda (w)
                                  (unless (or (eq w window)
                                              (window-dedicated-p w))
                                    (throw 'done nil)))
                                frame)
              t)))
     (not (window-minibuffer-p window))
     (let ((split-width-threshold 0))
       (when (window-splittable-p window t)
         (with-selected-window window
               (split-window-right))))))))

(defun split-window-really-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (if (> (window-total-width window) (* 2 (window-total-height window)))
        (with-selected-window window (split-window-sensibly-prefer-horizontal window))
      (with-selected-window window (split-window-sensibly window)))))

(setq split-window-preferred-function 'split-window-really-sensibly)

(setq-default split-height-threshold nil
              split-width-threshold  nil
              fill-column            80) ;; Maximum line width
              ;; window-min-width       80) ;; No smaller windows than this

;; Window and buffer management
(global-set-key (kbd "C-x |") 'split-window-horizontally)
(global-set-key (kbd "C-x -") 'split-window-vertically)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "M-<up>") 'scroll-down-line)

(delete-selection-mode 1)
(setq compilation-scroll-output t)

(setq auto-save-default nil)
(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)
(setq visible-bell       nil
      ring-bell-function #'ignore)

;; no backup files
(setq-default make-backup-files nil)
;; no lockfiles
(setq create-lockfiles nil)

;; Some keybinds for basic stuff


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

(use-package imenu-list
  :ensure t
  :bind  ("M-g i" . imenu-list-smart-toggle))

(use-package expand-region
  :ensure t
  :bind (("C-'" . er/expand-region)))

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

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

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
                                ;;(ruby-mode . ("ruby-lsp")
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

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-save-repository-buffers nil))

(use-package forge
  :ensure t
  :after (magit transient))

(use-package git-link
  :ensure t
  :init
  (setq git-link-use-commit t
        git-link-open-in-browser t))

(use-package blamer
  :ensure t
  :after magit
  :bind (("C-c g i" . blamer-show-commit-info)
         ("C-c g b" . blamer-show-posframe-commit-info))
  :defer 20
  :config
  (global-blamer-mode)
  :custom
  (blamer-idle-time                 0.3)
  (blamer-min-offset                4)
  (blamer-max-commit-message-length 100)
  (blamer-datetime-formatter        "[%s]")
  (blamer-commit-formatter          " ● %s")
  :custom-face
  (blamer-face ((t :foreground "#7aa2cf"
                    :background nil
                    :height 1
                    :italic nil))))

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
		 ("C-c j" . avy-goto-char-timer)
		 ("C-c w" . avy-goto-word-crt-line)
		 ))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (setq read-extended-command-predicate       'command-completion-default-include-p
        vertico-count                         32  ; Show more candidates
        read-file-name-completion-ignore-case t   ; Ignore case of file names
        read-buffer-completion-ignore-case    t   ; Ignore case in buffer completion
        completion-ignore-case                t)) ; Ignore case in completion
(use-package vertico-posframe
  :ensure t
  :init
  (setq vertico-posframe-parameters   '((left-fringe  . 12)    ;; Fringes
                                        (right-fringe . 12)
                                        (undecorated  . nil))) ;; Rounded frame
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width        96                       ;; Narrow frame
        vertico-posframe-height       vertico-count            ;; Default height
        ;; Don't create posframe for these commands
        vertico-multiform-commands    '((consult-line    (:not posframe))
                                        (consult-ripgrep (:not posframe)))))

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
	 ("C-c s" . consult-imenu)
	 ("C-x C-b" . consult-buffer)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ("C-c d" . consult-flymake))
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "3ef71018ff2043d308f8bc266787591acfaf8a0007621ca1304b0e3db6772c19"
     "54ba478b95c6a5efbe02642003d68ea9a713cd38f2c03da176a1b69578addf74"
     "a759f5bf996d821b4e5798c23ec80ff69571fbad7f574beaa75cf429e81579aa"
     "2082ebeb3b4871bff2d2154f239456fcf165c3de80121f875cd8c7d82bd13803"
     "b45b0d072e3e328e5e81b19969d6be8958ffc7609d2bfb3814e9c9ca1473daed"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
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
     "b29ba9bfdb34d71ecf3322951425a73d825fb2c002434282d2e0e8c44fce8185" default))
 '(package-selected-packages
   '(ace-window aidermacs auto-dark blamer claude-code consult copilot corfu
		default-text-scale doom-themes doric-themes eat
		exec-path-from-shell expand-region forge git-link gptel
		imenu-list lsp-ui marginalia mise nerd-icons olivetti orderless
		rbenv rg robe rspec-mode rust-mode south-theme stimmung-themes
		tree-sitter-langs ultra-scroll vertico-posframe vterm
		zoom-window))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")
     (ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll" :branch
		   "main")
     (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
