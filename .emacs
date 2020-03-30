(package-initialize)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Basic Setup
  (server-start)
  (set-frame-font "Fira Code:size=11")
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (toggle-word-wrap)
  (global-display-line-numbers-mode 1)
  (setq inhibit-compacting-font-caches t)
  (setq explicit-bash-args '("--noediting" "--login"))
  (setq inhibit-splash-screen t)
  (switch-to-buffer "*scratch*")
  (setq make-backup-files nil) ; stop creating backup~ files
  (setq auto-save-default nil) ; stop creating #autosave# files
  
;;Packages
(require 'package)


;;MacOS settings for GUi launch
(require 'exec-path-from-shell) ;; if not using the ELPA package
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;;Json
(require 'json)

;;Org-mode settings
(require 'org)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (setq org-log-done t)
  (add-hook 'org-mode-hook 'org-hide-block-all)

;;Theme
(load-theme 'sanityinc-tomorrow-night t)

;;Magit
(require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)

;;Ivy
(require 'ivy)
  (ivy-mode 1)
  (global-set-key (kbd "M-x") #'counsel-M-x)
  (global-set-key (kbd "C-x C-f") #'counsel-find-file) 
  (global-set-key (kbd "C-x b") #'ivy-switch-buffer)
  (global-set-key (kbd "C-s") #'swiper)
  (global-set-key (kbd "M-y") #'counsel-yank-pop)
  (global-set-key (kbd "M-.") #'counsel-etags-find-tag-at-point)
  (global-set-key (kbd "C-c g") #'counsel-git)

;; Projectile
(require 'projectile)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq counsel-find-file-ignore-regexp "\\.nupkg\\'")
  (projectile-global-mode)
  (setq projectile-git-submodule-command nil)
  (setq projectile-enable-caching t)
  (counsel-projectile-mode 1)

;;Custom Functions
(defun dice-magic-generic (message)
   "Make a request to dice magic."
   (interactive "sWhat do you want to ask DiceMagic? ")
   (setq request (format "curl -s -X POST -H \"application/json\" -d \"{\\\"cmd\\\":\\\"%s\\\"}\" https://api.dicemagic.io/roll" message))
   (setq result (shell-command-to-string request))
   (let ((json-object-type 'plist)
	 (json-array-type 'list))
     (setq parsedResponse (json-read-from-string result)))
   (message "%s" parsedResponse))
(global-set-key (kbd "C-c d") 'dice-magic-generic)


(defun lookup-dotnet-docs ()
  (interactive)
  (let (word)
    (setq word
	  (if (use-region-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (format "https://docs.microsoft.com/en-us/dotnet/api/?view=netcore-2.2&term=%s" word))
    ;; (eww myUrl) ; emacs's own browser
    ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   '(company-etags company-omnisharp company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
		   (company-dabbrev-code company-gtags company-etags company-keywords)
		   company-oddmuse company-dabbrev))
 '(custom-safe-themes
   '("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "05a4b82c39107308b5c3720fd0c9792c2076e1ff3ebb6670c6f1c98d44227689" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "7f89ec3c988c398b88f7304a75ed225eaac64efa8df3638c815acc563dfd3b55" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" default))
 '(helm-buffer-max-length 40)
 '(package-selected-packages
   '(lsp-mode exec-path-from-shell omnisharp material-theme counsel-etags counsel-projectile counsel swiper ivy flycheck color-theme-sanityinc-tomorrow zenburn-theme csharp-mode evil expand-region doom-themes monokai-theme json-mode projectile powershell company helm magit gruvbox-theme))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

