;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "web-mode")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'css-mode)
(require 'f)
(require 'flycheck)
(require 'handlebars-mode)
(require 'highlight-symbol)
(require 'import-js)
(require 'js2-mode)
(require 'rcirc)
(require 'rcirc-notify)

(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'css-mode-hook (lambda()
                           (setq css-indent-offset 2)))
(add-hook 'js-mode-hook (lambda()
                          (subword-mode t)
                          (auto-complete-mode t)
                          (setq js-indent-level 2)))

(add-hook 'web-mode-hook (lambda()
                           (web-mode-set-content-type "jsx")))
(add-hook 'web-mode-hook (lambda()
                           (subword-mode t)
                           (auto-complete-mode t)))
(add-hook 'prog-mode-hook (lambda()
                            (highlight-symbol-mode t)))

(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.template$" . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (atom-dark)))
 '(custom-safe-themes (quote ("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" default)))
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil)
 '(visible-bell t))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Do some shit with JSX highlighting."

  (if (equal web-mode-content-type "js")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
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

(setq flycheck-eslintrc "~/.eslintrc")

(setq-default column-number-mode t)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers '(javascript-jshint)))
(setq-default flycheck-temp-prefix ".flycheck")
(setq-default highlight-symbol-idle-delay 0.5)
(setq-default import-js-project-root "/home/dorzechowski/repos/platform-js")
(setq-default indent-tabs-mode nil)
(setq-default web-mode-attr-indent-offset 2)
(setq-default web-mode-css-indent-offset 2)
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq-default web-mode-enable-current-element-highlight t)
(set-face-background 'web-mode-current-element-highlight-face "#ddffdd")

(provide '.emacs)
;;; .emacs ends here
