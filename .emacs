;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

;; load any non-standard packages from here
(add-to-list 'load-path "~/.emacs.d/lisp")

;; enable package-loading from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; code completion
(require 'company)
(require 'company-quickhelp) ; tooltip for code-completion popup
(require 'company-tern) ; Javascript code analysis
(require 'company-web) ; HTML completion
;; inline code coverage indicators
(require 'coverlay)
;; automatic syntax checking
(require 'flycheck)
;; JSON major mode
(require 'json-mode)
;; fun mode-line customization
(require 'powerline)
;; Markdown major mode
(require 'markdown-mode)
;; SCSS major mode
;(require 'scss-mode)
;; web-development major mode
(require 'web-mode)

(defvar-local my-fci-mode-stack '()
  "track fci-mode state to aid advice functions.")

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun buffer-contains-string (string)
  "Search the top of the current buffer for STRING."
  (save-excursion
    (save-match-data
      (goto-char 0)
      (search-forward string 1023 t))))

(defun common-css-mode-hook ()
  "Do some things when entering a CSS mode."
  ;; turn on syntax-checking mode
  (flycheck-mode t)
  ;; turn off code-coverage mode
  (coverlay-mode nil)
  ;; enable code-completion mode
  (company-mode t)
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; select the appropriate syntax checker
  (flycheck-select-checker 'scss-stylelint))

(defun common-javascript-mode-hook ()
  "Do some things when entering a javascript mode."
  ;; turn on syntax-checking mode
  (flycheck-mode t)
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; assume JS, and enable the JS code analysis engine
  (tern-mode t)
  ;; enable code-completion mode
  (company-mode t)
  ;; set a vertical rule at column whatever
  (fci-mode t)
  ;; tell web-mode to use JSX where applicable
  (when (or (string-match-p ".js[x]?\\'" buffer-file-name)
            (buffer-contains-string "import React") ;; es6+ .js
            (or (buffer-contains-string "define([")
                (buffer-contains-string "require(["))) ;; requireJS
    (web-mode-set-content-type "jsx"))
  ;; auto-indent upon Enter keypress
  (electric-indent-mode t)
  ;; enable token highlighting
  (idle-highlight-mode t)
  ;; lint upon save
  (add-hook 'after-save-hook 'lint-fix-js)
  ;; code-coverage
  (unless (string-match-p "test.js[x]?\\'" buffer-file-name)
    ;; turn on coverage mode if not a test file
    (coverlay-mode t)
    (unless (bound-and-true-p coverlay--loaded-filepath)
      ;; load the closest coverage file if one hasn't been loaded yet
      (coverlay-watch-file (concat
                            (locate-dominating-file buffer-file-name "coverage")
                            "coverage/lcov.info")))))

(defun common-json-mode-hook ()
  "Do some things when entering a CSS-like mode."
  ;; turn on syntax-checking mode
  (flycheck-mode t)
  ;; turn off code-coverage mode
  (coverlay-mode nil)
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; select the appropriate syntax checker
  (flycheck-select-checker 'json-jsonlint))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun lint-fix-js ()
  "Run eslint --fix on current buffer."
  (if (executable-find "eslint_d")
      ;; progn to do things in guaranteed order
      (progn
        (if (eq 0 (call-process "eslint_d" nil "*ESLint Errors*" nil "--fix" buffer-file-name))
            (revert-buffer t t t)
          (message "eslint_d couldn't --fix due to errors")))
    (message "eslint_d not found")))

(defun fci-conditional-enable (&rest _)
  "Conditionally (re-)enable fci-mode."
  (when (eq (pop my-fci-mode-stack) t)
    (fci-mode t)))

(defun fci-get-and-disable (&rest _)
  "Store current status of fci-mode, and disable if needed."
  (when (boundp 'fci-mode)
    (push fci-mode my-fci-mode-stack)
    (when fci-mode
      ;; turns out nil actually enables fci-mode, not disables it :|
      (fci-mode -1))))

(defun fci-hack (advised-func &rest args)
  "Disable fci-mode, call ADVISED-FUNC with ARGS, then re-enable fci-mode."
  (progn
    (fci-get-and-disable)
    (apply advised-func args)
    (fci-conditional-enable)))

(defun set-powerline-theme ()
  "Powerline customization."
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let*
          ((active
            (powerline-selected-window-active))
           (mode-line-buffer-id
            (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
           (mode-line
            (if active 'mode-line 'mode-line-inactive))
           (face0
            (if active 'powerline-active0 'powerline-inactive0))
           (face1
            (if active 'powerline-active1 'powerline-inactive1))
           (face2
            (if active 'powerline-active2 'powerline-inactive2))
           (separator-left
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (car powerline-default-separator-dir))))
           (separator-right
            (intern
             (format "powerline-%s-%s"
                     (powerline-current-separator)
                     (cdr powerline-default-separator-dir))))
           (lhs
            (list
             (powerline-raw "%b %*" face0 'l)
             (funcall separator-left face0 face1)
             (when
                 (and
                  (boundp 'erc-track-minor-mode)
                  erc-track-minor-mode)
               (powerline-raw erc-modified-channels-object face1 'l))
             (powerline-major-mode face1 'l)
             (powerline-process face1)
             (powerline-minor-modes face1 'l)
             (powerline-narrow face1 'l)
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (powerline-vc face2 'r)
             ))

           (rhs
            (list )
            )
           )
        (concat
         (powerline-render lhs)
         (powerline-fill face2
                         (powerline-width rhs))
         (powerline-render rhs)))))))

;; do some things after initializing the Emacs session:
(add-hook 'after-init-hook
          (lambda ()
            ;; show line numbers in every file
            (global-linum-mode t)
            ;; enable help tooltips for code-completion popup (when enabled)
            (company-quickhelp-mode 1)))
;; run the appropriate type-specific hook after turning on web-mode for a given buffer:
(add-hook 'web-mode-hook
          (lambda ()
            (let ((buffer-type (file-name-extension (buffer-file-name))))
              (cond ((string-match-p "js[x]?\\'" buffer-type)
                     (common-javascript-mode-hook))
                    ((string-match-p "json\\'" buffer-type)
                     (common-json-mode-hook))))))

;; turn on web-mode for every file ending in '.js' or '.jsx':
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
;; turn on web-mode for every file ending in '.json':
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
;; turn on css-mode for every file ending in '.css' or '.scss':
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . css-mode))
;; turn on css-mode for every file ending in '.less':
(add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
;; turn on web-mode for every file ending in '.hbs':
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
;; turn on markdown-mode for every file ending in '.md':
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; set up some indent-related nonsense for web-mode
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

;; disable fci-mode while certain operations are in progress
(advice-add 'web-mode-on-after-change :around #'fci-hack)
(advice-add 'web-mode-on-post-command :around #'fci-hack)
(add-hook 'company-completion-started-hook 'fci-get-and-disable)
(add-hook 'company-completion-cancelled-hook 'fci-conditional-enable)
(add-hook 'company-completion-finished-hook 'fci-conditional-enable)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-capf company-css company-files company-tern company-web)))
 '(company-files-exclusions
   (quote
    (".o" "~" "#" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/")))
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.25)
 '(company-tooltip-align-annotations t)
 '(coverlay:tested-line-background-color nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(css-indent-offset 4)
 '(css-mode-hook (quote common-css-mode-hook))
 '(cua-mode t nil (cua-base))
 '(cursor-type (quote bar))
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flycheck-javascript-eslint-executable "eslint_d")
 '(frame-title-format (quote ("%f")) t)
 '(fringe-mode 20 nil (fringe))
 '(hl-line-mode t t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (fill-column-indicator import-js sass-mode powerline company-web string-inflection idle-highlight-mode coverlay json-mode markdown-mode web-mode company-quickhelp company-tern flycheck tern-context-coloring scss-mode)))
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
 '(tool-bar-mode nil)
 '(visual-bell t)
 '(web-mode-enable-auto-quoting nil))
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
 '(fringe ((t (:background "grey98"))))
 '(linum ((t (:background "gray98" :foreground "dim gray"))))
 '(powerline-active0 ((t (:background "tomato" :foreground "white" :weight bold))))
 '(powerline-active1 ((t (:background "gray95"))))
 '(powerline-active2 ((t (:background "gray95"))))
 '(powerline-inactive0 ((t (:background "gray98"))))
 '(powerline-inactive1 ((t (:background "gray98"))))
 '(powerline-inactive2 ((t (:background "gray98"))))
 '(web-mode-current-column-highlight-face ((t (:background "#f0f0f0")))))

;; replace the stock Flycheck double-arrow indicator with a bigger one
;; maxmum width is 16px according to emacs docs
(define-fringe-bitmap 'flycheck-big-indicator
  (vector #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0111000111000000
          #b0011100011100000
          #b0001110001110000
          #b0000111000111000
          #b0000011100011100
          #b0000011100011100
          #b0000111000111000
          #b0001110001110000
          #b0011100011100000
          #b0111000111000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000)
  16 16 'center)

;; turn on some syntax checkers
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'json-jsonlint 'web-mode)
(flycheck-add-mode 'scss-stylelint 'web-mode)

(flycheck-define-error-level 'warning
  :severity 1
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'flycheck-big-indicator
  :fringe-face 'flycheck-fringe-warning)
(flycheck-define-error-level 'error
  :severity 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-big-indicator
  :fringe-face 'flycheck-fringe-error)

(global-hl-line-mode t)

;; Ctrl-Backspace -> delete a word instead of killing it
(global-set-key [C-backspace] 'backward-delete-word)
;; Ctrl-Delete -> forward-delete a word instead of killing it
(global-set-key [C-delete] 'delete-word)
;; Ctrl-Tab -> next window
(global-set-key [C-tab] 'other-window)
;; Ctrl-Shift-Tab -> previous window
(global-set-key [C-S-iso-lefttab]
                (lambda ()
                  (interactive)
                  (other-window -1)))
;; Ctrl-PgDn -> next window
(global-set-key [C-next] 'other-window)
;; Ctrl-PgUp -> previous window
(global-set-key [C-prior]
                (lambda ()
                  (interactive)
                  (other-window -1)))

(set-face-background 'hl-line "#f8f8f8")
(set-powerline-theme)

(provide '.emacs)
;;; .emacs ends here
