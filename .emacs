;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'company)
(require 'company-quickhelp)
(require 'company-tern)
(require 'company-web)
(require 'flycheck)
(require 'web-mode)

(add-hook 'after-init-hook (lambda()
			     (global-company-mode t)
			     (global-flycheck-mode t)
			     (company-quickhelp-mode 1)))
(add-hook 'web-mode-hook (lambda()
			   (subword-mode t)
			   (tern-mode t)))

(add-to-list 'company-backends 'company-tern)
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-use-system-font t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (web-mode company-web flycheck company-quickhelp company-tern)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(flycheck-add-mode 'javascript-eslint 'web-mode)

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] (lambda ()
				    (interactive)
				    (other-window -1)))
(global-set-key [C-next] 'other-window)
(global-set-key [C-prior] (lambda ()
                            (interactive)
                            (other-window -1)))

(setq-default company-files-exclusions (append completion-ignored-extensions (quote (".*~" ".*#"))))
(setq-default company-quickhelp-delay 0.4)

(provide '.emacs)
;;; .emacs ends here
