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
			   (tern-mode t)
			   (web-mode-set-content-type "jsx")))

(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-use-system-font t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (js-doc web-mode company-web flycheck company-quickhelp company-tern)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Make JSX highlighting great again."

  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil)) ad-do-it)
    ad-do-it))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-iso-lefttab] (lambda ()
				    (interactive)
				    (other-window -1)))
(global-set-key [C-next] 'other-window)
(global-set-key [C-prior] (lambda ()
                            (interactive)
                            (other-window -1)))

(setq-default company-backends '(company-capf company-files company-tern))
(setq-default company-files-exclusions (append completion-ignored-extensions (quote (".*~" ".*#"))))
(setq-default company-quickhelp-delay 0.4)
(setq-default css-indent-offset 2)
(setq-default web-mode-attr-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-enable-auto-quoting nil)
(setq-default web-mode-markup-indent-offset 2)

(provide '.emacs)
;;; .emacs ends here
