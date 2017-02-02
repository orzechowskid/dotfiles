;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'company)
(require 'company-quickhelp)
(require 'company-tern)
(require 'flycheck)

(add-hook 'after-init-hook (lambda()
			     (global-company-mode t)
			     (global-flycheck-mode t)
			     (company-quickhelp-mode 1)))

(add-to-list 'company-backends 'company-tern)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-use-system-font t)
 '(inhibit-splash-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (flycheck company-quickhelp company-tern)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq-default company-files-exclusions (append completion-ignored-extensions (quote (".*~" ".*#"))))
(setq-default company-quickhelp-delay 0.4)

(provide '.emacs)
;;; .emacs ends here
