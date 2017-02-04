;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'company)
(require 'company-quickhelp)
(require 'company-tern)
(require 'company-web)
(require 'flycheck)
(require 'js-doc)
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
 '(company-backends
   (quote
    (company-capf company-css company-files company-tern company-web company-capf)))
 '(company-files-exclusions
   (quote
    (".o" "~" "#" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/")))
 '(company-minimum-prefix-length 0)
 '(company-tooltip-align-annotations t)
 '(font-use-system-font t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (js2-mode web-mode company-web flycheck company-quickhelp company-tern)))
 '(tool-bar-mode nil)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight nil)
 '(web-mode-enable-heredoc-fontification nil)
 '(web-mode-enable-html-entities-fontification nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "white"))))
 '(company-scrollbar-fg ((t (:background "darkgray"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-annotation-selection))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 '(web-mode-current-column-highlight-face ((t (:background "#f0f0f0")))))

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

(setq-default company-backends '(company-capf company-css company-files company-tern company-web))
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
