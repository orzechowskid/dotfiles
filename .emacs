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
;; inline code coverage indicators
(require 'coverlay)
;; automatic syntax checking
(require 'flycheck)
;; Handlebars-templates major mode
(require 'handlebars-mode)
;; JSON major mode
(require 'json-mode)
;; fun mode-line customization
(require 'powerline)
;; Javascript formatting minor mode
(require 'prettier-js)
;; web-development major mode
(require 'web-mode)

(defun buffer-contains-string (string)
  "Search the top of the current buffer for STRING."
  (save-excursion
    (save-match-data
      (goto-char 0)
      (search-forward string 1023 t))))

(defun common-javascript-mode-hook ()
  "Do some things when entering a javascript mode."
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; assume JS, and enable the JS code analysis engine
  (tern-mode t)
  ;; enable prettier minor-mode for code formatting
  (prettier-js-mode t)
  ;; tell web-mode to use JSX where applicable
  (when (or (string-match-p ".jsx\\'" buffer-file-name)
            (buffer-contains-string "import React"))
    (web-mode-set-content-type "jsx"))
  ;; auto-indent upon Enter keypress
  (electric-indent-mode t)
  ;; enable test coverage
  (coverlay-mode t)
  ;; enable token highlighting
  (idle-highlight-mode t)
  ;; code-coverage
  (unless (string-match-p "test.js[x]?\\'" buffer-file-name)
    ;; turn on coverage mode if not a test file
    (coverlay-mode t)
    (unless (bound-and-true-p coverlay--loaded-filepath)
      ;; load the closest coverage file if one hasn't been loaded yet
      (coverlay-watch-file (concat
                            (locate-dominating-file buffer-file-name "coverage")
                            "coverage/lcov.info")))))

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
            ;; auto-complete everything everywhere
            (global-company-mode t)
            ;; show line numbers in every file
            (global-linum-mode t)
            ;; auto-check everything
            (global-flycheck-mode t)
            ;; enable help tooltips for code-completion popup
            (company-quickhelp-mode 1)))
;; do some things after turning on scss-mode for a given buffer:
(add-hook 'scss-mode-hook
          (lambda ()
            ;; turn on camelCase-aware code navigation
            (subword-mode t)))
;; do some things after turning on json-mode for a given buffer:
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
;; do some things after turning on web-mode for a given buffer:
(add-hook 'web-mode-hook 'common-javascript-mode-hook)

;; turn on web-mode for every file ending in '.js' or '.jsx':
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
;; turn on json-mode for every file ending in '.json':
(add-to-list 'json-mode-auto-mode-list '("\\.json\\'" . json-mode))
;; turn on scss-mode for every file ending in '.css' or '.scss':
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . scss-mode))
;; turn on scss-mode for every file ending in '.less':
(add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))
;; turn on handlebars-mode for every file ending in '.less':
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . handlebars-mode))
;; set up some indent-related nonsense for web-mode
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
 '(coverlay:tested-line-background-color nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(css-indent-offset 4)
 '(cursor-type (quote bar))
 '(electric-indent-mode nil)
 '(frame-title-format (quote ("%f")) t)
 '(fringe-mode 20 nil (fringe))
 '(hl-line-mode t t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(menu-bar-mode nil)
; '(mode-line-format
;   (quote
;    ("%e" mode-line-front-space
;     (:propertize "%b[%*]" face
;                  (:weight bold))
;     " " mode-line-modes " "
;     (vc-mode vc-mode)
;     mode-line-end-spaces)))
 '(package-selected-packages
   (quote
    (powerline prettier-js company-web string-inflection handlebars-mode idle-highlight-mode coverlay json-mode markdown-mode web-mode company-quickhelp company-tern flycheck tern-context-coloring scss-mode)))
 '(powerline-display-buffer-size nil)
 '(powerline-display-hud nil)
 '(powerline-display-mule-info nil)
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
 '(tool-bar-mode nil)
 '(visual-bell t))
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
 '(linum ((t (:background "gray95" :foreground "dim gray"))))
 '(powerline-active0 ((t (:background "tomato" :foreground "white" :weight bold))))
 '(powerline-active1 ((t (:background "gray95" :foreground "black"))))
 '(powerline-active2 ((t (:background "gray95" :foreground "black"))))
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

;; turn on ESLint for each file opened in rjsx-mode
;(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;; turn on ESLint for each file opened in web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; turn on stylelint for each file opened in scss-mode
(flycheck-add-mode 'scss-stylelint 'scss-mode)

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
