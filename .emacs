;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

;; load any non-standard packages from here
(add-to-list 'load-path "~/.emacs.d/lisp")

;; enable package-loading from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; code completion
(require 'company)
(require 'company-quickhelp) ; tooltip for code-completion popup
(require 'company-tern) ; Javascript code analysis
(require 'company-web) ; HTML completion
;; automatic syntax checking
(require 'flycheck)
;; JSON major mode
(require 'json-mode)
;; web-development major mode
(require 'web-mode)

;; do some things after initializing the Emacs session:
(add-hook 'after-init-hook (lambda()
                             ;; auto-complete everything everywhere
			     (global-company-mode t)
                             ;; enable help tooltips for code-completion popup
			     (company-quickhelp-mode 1)))
;; do some things after turning on scss-mode for a given buffer:
(add-hook 'scss-mode-hook (lambda()
                            ;; turn on camelCase-aware code navigation
                            (subword-mode t)))
;; do some things after turning on json-mode for a given buffer:
(add-hook 'json-mode-hook (lambda()
                            (make-local-variable 'js-indent-level)
                            (setq js-indent-level 2)))
;; do some things after turning on web-mode for a given buffer:
(add-hook 'web-mode-hook (lambda()
                           ;; auto-check everything
                           (global-flycheck-mode t)
                           ;; turn on camelCase-aware code navigation
			   (subword-mode t)
                           ;; assume JS, and enable the JS code analysis engine
			   (tern-mode t)
                           ;; assume JSX
			   (web-mode-set-content-type "jsx")
                           ;; auto-indent upon Enter keypress
                           (electric-indent-mode t)
                           ))

;; turn on json-mode for every file ending in '.json':
(add-to-list 'json-mode-auto-mode-list '("\\.json\\'" . json-mode))
;; turn on web-mode for every file ending in '.js':
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
;; turn on scss-mode for every file ending in '.css' or '.scss':
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . scss-mode))

(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(company-backends
   (quote
    (company-capf company-css company-files company-tern company-web)))
 '(company-files-exclusions
   (quote
    (".o" "~" "#" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/")))
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.25)
 '(company-tooltip-align-annotations t)
 '(css-indent-offset 4)
 '(cursor-type 'bar)
 '(electric-indent-mode nil)
 '(hl-line-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (json-mode markdown-mode js2-mode web-mode company-web flycheck company-quickhelp company-tern)))
 '(tool-bar-mode nil)
 '(visual-bell t)
 '(web-mode-attr-indent-offset 4)
 '(web-mode-code-indent-offset 4)
 '(web-mode-css-indent-offset 4)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-enable-auto-quoting nil)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight nil)
 '(web-mode-enable-heredoc-fontification nil)
 '(web-mode-enable-html-entities-fontification nil)
 '(web-mode-indent-style 2)
 '(web-mode-markup-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight light :width normal))))
 '(company-scrollbar-bg ((t (:background "white"))))
 '(company-scrollbar-fg ((t (:background "darkgray"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-annotation-selection))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 '(web-mode-current-column-highlight-face ((t (:background "#f0f0f0")))))

;; give advice to web-mode-highlight-part on how to do its job
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  "Make JSX highlighting great again."

  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil)) ad-do-it)
    ad-do-it))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; turn on ESLint for each file opened in web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'scss-lint 'scss-mode)

(global-hl-line-mode t)

;; Ctrl-Backspace -> delete a word instead of killing it
(global-set-key [C-backspace] 'backward-delete-word)
;; Ctrl-Delete -> forward-delete a word instead of killing it
(global-set-key [C-delete] 'delete-word)
;; Ctrl-Tab -> next window
(global-set-key [C-tab] 'other-window)
;; Ctrl-Shift-Tab -> previous window
(global-set-key [C-S-iso-lefttab] (lambda ()
				    (interactive)
				    (other-window -1)))
;; Ctrl-PgDn -> next window
(global-set-key [C-next] 'other-window)
;; Ctrl-PgUp -> previous window
(global-set-key [C-prior] (lambda ()
                            (interactive)
                            (other-window -1)))

(set-face-background 'hl-line "#f8f8f8")

(provide '.emacs)
;;; .emacs ends here
