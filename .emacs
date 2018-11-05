(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(load-theme 'gruvbox t)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook 'org-hide-block-all)

(require 'ob-sh)
(org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

(defun dice-magic (hitMod damage)
   "Says hello."
   (interactive "sHit Mod:\nsDamage Die: ")
   (defvar request (format "curl -s -X POST -H \"application/json\" -d \"{\\\"cmd\\\":\\\"Roll 1d20%s and %s\\\"}\" https://api.dicemagic.io/roll" hitMod damage))
   (defvar result (shell-command-to-string request))
   (message "%s" result))
