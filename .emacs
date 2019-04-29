;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;;;
;;; utility functions
;;;

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

;;;
;;; packages
;;;

;; load any non-standard packages from here
(add-to-list 'load-path "~/.emacs.d/lisp")

;; enable package-loading from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents t) ;; refresh contents on startup

;; these packages are used by multiple major modes and/or I want to set some config on them
;; at bootup; they're not good candidates for autoload

;; code completion
(require 'company)
(require 'company-quickhelp) ; tooltip for code-completion popup
(require 'company-web-html) ; not an ELPA package, but rather provided by company
;; code analysis
(require 'eglot)
;; linting
(require 'flycheck)
;; fun mode-line customization
(require 'powerline)
;; web-development major mode
(require 'web-mode)

;; these guys, however...

(autoload 'coverlay-minor-mode "coverlay"
  "Use the coverlay package to provide 'coverlay-minor-mode on-demand."
  t)
(autoload 'json-mode "json-mode"
  "Use the json-mode package to provide 'json-mode on-demand."
  t)
(autoload 'markdown-mode "markdown-mode"
  "Use the markdown-mode package to provide 'markdown-mode on-demand."
  t)
(autoload 'scss-mode "scss-mode"
  "Use the scss-mode package to provide 'scss-mode on-demand."
  t)

;;;
;;; mode configuration
;;;

;; associate web-mode with a particular langserver (eglot doesn't do this out of the box)
;(push '(web-mode . ("javascript-typescript-stdio")) eglot-server-programs)

;; we use stylelint, not scss-lint and friends
;(push 'scss-lint flycheck-disabled-checkers)

;; associate some major modes with some file extensions
(push '("\\.js[x]?\\'" . web-mode) auto-mode-alist)
(push '("\\.json\\'" . json-mode) auto-mode-alist)
(push '("\\.[s]?css\\'" . scss-mode) auto-mode-alist)
(push '("\\.less\\'" . scss-mode) auto-mode-alist)
(push '("\\.hbs\\'" . web-mode) auto-mode-alist)
(push '("\\.md\\'" . markdown-mode) auto-mode-alist)

;;;
;;; mode hooks
;;;

(defun common-css-mode-hook ()
  "Do some things when entering a CSS mode."
  ;; enable code-completion mode
  (company-mode t)
  ;; documentation in minibuffer
  (eldoc-mode t)
  ;; linting
  (flycheck-mode t)
  ;; turn on camelCase-aware code navigation
  (subword-mode t))

(defun common-javascript-mode-hook ()
  "Do some things when entering a javascript mode."
  ;; hook up to LSP server
;  (eglot-ensure)
  ;; documentation in minibuffer
  (eldoc-mode t)
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; enable code-completion mode
  (company-mode t)
  ;; tell web-mode to use JSX for frontend JS code
  (when (string-match-p ".js[x]?\\'" buffer-file-name)
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

;;; eglot appears to run mode-hooks when displaying descriptions.  that's a potentially
;;; expensive thing to do, so we disable our hook if we detect a temporary buffer
;(advice-add 'common-javascript-mode-hook :before-until
;            (lambda (&rest args)
;              (string-prefix-p " *temp*" (buffer-name) t)))

(defun common-json-mode-hook ()
  "Do some things when entering a JSON mode."
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; turn on syntax checking
  (flycheck-mode t)
  ;; json-mode inherits from js-mode?
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
(add-hook 'term-mode-hook 'common-term-mode-hook)

;; set up some indent-related nonsense for web-mode
(push '("lineup-args" . nil) web-mode-indentation-params)
(push '("lineup-calls" . nil) web-mode-indentation-params)
(push '("lineup-concats" . nil) web-mode-indentation-params)
(push '("lineup-ternary" . nil) web-mode-indentation-params)

;; turn on some syntax checkers for some major modes
(flycheck-add-mode 'javascript-eslint 'web-mode)

;;;
;;; variables
;;;

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

;; Emacs provides this facility for us

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
 '(cua-mode t nil (cua-base))
 '(cursor-type 'bar)
 '(electric-indent-mode nil)
 '(fill-column 80)
 '(flycheck-javascript-eslint-executable "eslint_d")
 '(flycheck-scss-lint-executable "stylelint")
 '(fringe-mode 20 nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(request yaml-mode web-mode scss-mode powerline markdown-mode json-mode idle-highlight-mode git-commit flycheck coverlay company-web company-tern company-quickhelp))
 '(powerline-display-buffer-size nil)
 '(powerline-display-hud nil)
 '(powerline-display-mule-info nil)
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
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

;;;
;;; keyboard shortcuts
;;;

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
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;;;
;;; post-init hook
;;;

;; a bunch of stuff which I want to do upon startup but didn't fit anywhere else
(add-hook 'after-init-hook
          (lambda ()
;            (set-face-background 'hl-line "#f8f8f8")
            (powerline-default-theme)
            (global-eldoc-mode -1)
            (push "~/.npm-global/bin" exec-path)
            ;; keep me as last
            (message "startup time: %s" (emacs-init-time))))

(provide '.emacs)
;;; .emacs ends here
