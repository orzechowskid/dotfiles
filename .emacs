;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; load any non-standard packages from here
(add-to-list 'load-path "~/.emacs.d/lisp")

;; enable package-loading from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents t)

;; code analysis
(load-file "~/.emacs.d/lisp/eglot.el")
(require 'eglot)
(add-to-list 'eglot-server-programs '(web-mode . ("javascript-typescript-stdio")))

;; code completion
(require 'company)
(require 'company-web-html)
(require 'company-quickhelp) ; tooltip for code-completion popup
;; inline code coverage indicators
(require 'coverlay)
;; linting
(require 'flycheck)
;; highlight current token
(require 'idle-highlight-mode)
;; JSON major mode
(require 'json-mode)
;; fun mode-line customization
(require 'powerline)
;; Markdown major mode
(require 'markdown-mode)
;; SCSS major mode
(require 'scss-mode)
;; web-development major mode
(require 'web-mode)

(defun my-company-transformer (candidates)
  "Do something with CANDIDATES, not sure what."
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun buffer-contains-string-near-top (string)
  "Search the top of the current buffer for STRING."
  (save-excursion
    (save-match-data
      (goto-char 0)
      (search-forward string 1023 t))))

(defun common-css-mode-hook ()
  "Do some things when entering a CSS mode."
  ;; enable code-completion mode
  (company-mode t)
  ;; documentation in minibuffer
  (eldoc-mode t)
  ;; linting
  (flycheck-mode t)
  ;; disable the bad scss linter
  (add-to-list 'flycheck-disabled-checkers 'scss-lint)
  ;; turn on camelCase-aware code navigation
  (subword-mode t))

(defun common-javascript-mode-hook ()
  "Do some things when entering a javascript mode."
  ;; turn on code-coverage mode
  (coverlay-minor-mode t)
  ;; hook up to LSP server
  (eglot-ensure)
  ;; documentation in minibuffer
  (eldoc-mode t)
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; enable code-completion mode
  (company-mode t)
  ;; tell web-mode to use JSX for frontend JS code
  (when (or (string-match-p ".js[x]?\\'" buffer-file-name)
            (buffer-contains-string-near-top "import React") ;; es6+ .js
            (or (buffer-contains-string-near-top "define([")
                (buffer-contains-string-near-top "require(["))) ;; requireJS
    (web-mode-set-content-type "jsx"))
  ;; auto-indent upon Enter keypress
  (electric-indent-mode t)
  ;; linting
  (flycheck-mode t)
  ;; lint upon save
  (add-hook 'after-save-hook 'lint-fix-js t t)
  ;; code-coverage
  (unless (string-match-p "test.js[x]?\\'" buffer-file-name)
    ;; turn on coverage mode if not a test file
    
    (coverlay-minor-mode t)
    (unless (bound-and-true-p coverlay--loaded-filepath)
      ;; load the closest coverage file if one hasn't been loaded yet
      (coverlay-watch-file (concat
                            (locate-dominating-file buffer-file-name "coverage")
                            "coverage/lcov.info")))))

(defun common-json-mode-hook ()
  "Do some things when entering a JSON mode."
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(defun common-lisp-mode-hook ()
  "Do some things when entering a Lisp mode."
  (eldoc-mode t)
  (flycheck-mode t)
  (company-mode t))

(defun common-term-mode-hook ()
  "Do some things when a terminal is opened."
  ;; disable camelCase-aware navigation
  (subword-mode 0))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun lint-fix-js ()
  "Run 'eslint --fix' on current buffer."
  (if (executable-find "eslint_d")
      (progn
        (call-process "eslint_d" nil "*ESLint Errors*" nil "--fix" buffer-file-name)
        (revert-buffer t t t)
        (flycheck-buffer))
    (message "eslint_d not found")))

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
             (powerline-raw "%l:%c" face1 'r);
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

;; turn on company quickhelp for all buffers
(add-hook 'after-init-hook 'company-quickhelp-mode)

;; disable flymake - we like flycheck
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

;; do some things when entering lisp modes
(add-hook 'emacs-lisp-mode-hook 'common-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'common-lisp-mode-hook)

;; do some things after entering JSON mode
(add-hook 'json-mode-hook 'common-json-mode-hook)

;; do some things after entering css modes
(add-hook 'scss-mode-hook 'common-css-mode-hook)

;; do some things after entering web-mode
(add-hook 'web-mode-hook 'common-javascript-mode-hook)

;; do some things after launching a terminal
(add-hook 'term-mode-hook
          (lambda ()
            (hl-line-mode 0)))

;; turn on web-mode for every file ending in '.js' or '.jsx':
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

;; turn on json-mode for every file ending in '.json':
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; turn on scss-mode for every file ending in '.css' or '.scss':
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . scss-mode))

;; turn on scss-mode for every file ending in '.less':
(add-to-list 'auto-mode-alist '("\\.less\\'" . scss-mode))

;; turn on web-mode for every file ending in '.hbs':
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;; turn on markdown-mode for every file ending in '.md':
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; set up some indent-related nonsense for web-mode
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

;; replace the stock Flycheck warning/error indicators with a bigger one for hidpi
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-window-vscroll nil t)
 '(company-backends '(company-capf company-css company-files))
 '(company-files-exclusions
   '(".o" "~" "#" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/"))
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.25)
 '(company-tooltip-align-annotations t)
 '(coverlay:mark-tested-lines nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(css-indent-offset 4)
 '(css-mode-hook 'common-css-mode-hook t)
 '(cua-mode t nil (cua-base))
 '(cursor-type 'bar)
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flycheck-disabled-checkers (scss-lint))
 '(flycheck-javascript-eslint-executable "eslint_d")
 '(flycheck-scss-lint-executable "stylelint")
 '(frame-title-format '("%f") t)
 '(fringe-mode 20 nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(multi-term hl-fill-column flycheck flymake-cursor magit nlinum import-js sass-mode powerline company-web string-inflection idle-highlight-mode coverlay json-mode markdown-mode web-mode company-quickhelp company-tern tern-context-coloring scss-mode))
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
 '(term-mode-hook #'common-term-mode-hook)
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
 '(powerline-active0 ((t (:background "tomato" :foreground "white" :weight bold))))
 '(powerline-active1 ((t (:background "gray95"))))
 '(powerline-active2 ((t (:background "gray95"))))
 '(powerline-inactive0 ((t (:background "gray98"))))
 '(powerline-inactive1 ((t (:background "gray98"))))
 '(powerline-inactive2 ((t (:background "gray98"))))
 '(reb-match-0 ((t (:background "misty rose"))))
 '(web-mode-current-column-highlight-face ((t (:background "#f0f0f0"))))
 '(web-mode-jsx-depth-1-face ((t nil)))
 '(web-mode-jsx-depth-2-face ((t nil)))
 '(web-mode-jsx-depth-3-face ((t nil)))
 '(web-mode-jsx-depth-4-face ((t nil)))
 '(web-mode-jsx-depth-5-face ((t nil)))
 '(web-mode-jsx-depth-6-face ((t nil)))
 '(web-mode-jsx-depth-7-face ((t nil))))

;; turn on some syntax checkers for some major modes
(flycheck-add-mode 'javascript-eslint 'web-mode)

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

(global-eldoc-mode -1)

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
;; Ctrl-a -> select entire file
(global-set-key [C-a] 'mark-whole-buffer)

(set-face-background 'hl-line "#f8f8f8")
(set-powerline-theme)

(provide '.emacs)
;;; .emacs ends here
