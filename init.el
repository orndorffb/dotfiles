;; Define and initialise package repositories
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; use-package to simplify the config file
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
(setq use-package-always-ensure 't)

;;Basic nice QoL
(setq visible-bell       nil
      ring-bell-function #'ignore)


;;Line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq backup-directory-alist '(("." . "~/.saves")))
(setq backup-by-copying t)

;;Theme-ing
(use-package modus-themes
  :ensure                         ; omit this to use the built-in themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))


;;Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;;Ivy
(use-package ivy
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))
(provide 'init-ivy)

;;Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;;Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
			 (lsp))))  ; or lsp-deferred

;;Markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aa72e5b41780bfff2ff55d0cc6fcd4b42153386088a4025fed606c1099c2d9b8" "74a50f18c8c88eac44dc73d7a4c0bbe1f3e72ff5971aac38fcf354ddad0d4733" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "2c613514f52fb56d34d00cc074fe6b5f4769b4b7f0cc12d22787808addcef12c" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "3325e2c49c8cc81a8cc94b0d57f1975e6562858db5de840b03338529c64f58d1" "b77a00d5be78f21e46c80ce450e5821bdc4368abf4ffe2b77c5a66de1b648f10" default))
 '(package-selected-packages
   '(blacken py-autopep8 flycheck elpy magit modus-vivendi-theme modus-operandi-theme gruvbox-theme zenburn-theme use-package projectile ayu-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
