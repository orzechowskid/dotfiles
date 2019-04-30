;;; .emacs --- Summary:
;;; Commentary:
;;; Code:

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")


;;; package things


;; enable package-loading from MELPA
(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)

;; these things are used by multiple major modes and/or get configured before any
;; buffers are opened, so they're not good candidates for autoload
(require 'company)
(require 'company-quickhelp)
(require 'powerline)
(require 'eglot)
(require 'flymake)
;(require 'smart-jump)

;; these things, however...
(autoload 'coverlay-mode "coverlay"
  "Use the coverlay package to provide 'coverlay-mode on-demand."
  t)
(autoload 'flymake-eslint-enable "flymake-eslint"
  "Use the flymake-eslint package to provide 'flymake-eslint-enable on-demand."
  t)
(autoload 'json-mode "json-mode"
  "Use the json-mode package to provide 'json-mode on-demand."
  t)
(autoload 'markdown-mode "markdown-mode"
  "Use the markdown-mode package to provide 'markdown-mode on-demand."
  t)
(autoload 'rjsx-mode "rjsx-mode"
  "Use the rjsx-mode package to provide 'rjsx-mode on-demand."
  t)
(autoload 'scss-mode "scss-mode"
  "Use the scss-mode package to provide 'scss-mode on-demand."
  t)
;(autoload 'web-mode "web-mode"
;  "Use the web-mode package to provide 'web-mode on-demand."
;  t)


;;; utility functions and advices


(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))


;; prefer contents of help-at-point (e.g., Flymake) over whatever eldoc outputs
(advice-add 'eldoc-message :around
            (lambda (oldfn doc-msg)
              (let ((echo-help-string (help-at-pt-string)))
                (if echo-help-string
                    (display-local-help)
                  (funcall oldfn doc-msg)))))


;;; mode hooks and config


(defun my-css-mode-hook ()
  "Do some things when opening [S]CSS files."
  (company-mode t)
  (eldoc-mode t)
  ;; disabled-checkers list is buffer-local
  (subword-mode t))

(defun my-flymake-mode-hook ()
  "Do some things when enabling Flymake."
  (setq-local help-at-pt-timer-delay 0.3)
  (help-at-pt-set-timer)
  (setq-local help-at-pt-display-when-idle t))

(defun common-javascript-mode-hook ()
  "Do some things when opening JavaScript files."
  ;; enable code-completion
  (company-mode t)
  ;; enable documentation in echo area
  (eldoc-mode t)
  ;; enable camelCase-aware code navigation
  (subword-mode t)
  ;; connect to a language server
  (add-hook 'eglot--managed-mode-hook
            (lambda ()
              ;; eglot's built-in Flymake backend doesn't work
              (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)
              ;; add our own
              (flymake-eslint-enable))
            nil t)
  (eglot-ensure)
  ;; linting
  (flymake-eslint-enable)
  ;; turn on inline test coverage if not a test file
  (unless (string-match-p "test.js[x]?\\'" buffer-file-name)
    (coverlay-minor-mode t)
    ;; load and watch the closest coverage data if none loaded yet
    (unless (bound-and-true-p coverlay--loaded-filepath)
      (coverlay-watch-file (concat (locate-dominating-file buffer-file-name "coverage")
                                   "coverage/lcov.info")))))

(defun my-json-mode-hook ()
  "Do some things when opening JSON files."
  (make-local-variable 'js-indent-level)
;  (set 'js-indent-level 2)
  )

(defun my-lisp-mode-hook ()
  "Do some things when opening LISP files."
  (eldoc-mode t)
  (company-mode t)
  (when (not (string= (buffer-name) "*scratch*"))
    (flymake-mode t)))

(defun my-terminal-mode-hook ()
  "Do some things when opening a terminal."
  (subword-mode nil))

(add-hook 'scss-mode-hook 'my-css-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'rjsx-mode-hook 'common-javascript-mode-hook)
(add-hook 'json-mode-hook 'my-json-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'term-mode-hook 'my-terminal-mode-hook)
(add-hook 'flymake-mode-hook 'my-flymake-mode-hook)

;; associate some major modes with some file extensions
(push '("\\.js[x]?\\'" . rjsx-mode) auto-mode-alist)
(push '("\\.json\\'" . json-mode) auto-mode-alist)
(push '("\\.[s]?css\\'" . scss-mode) auto-mode-alist)
(push '("\\.less\\'" . scss-mode) auto-mode-alist)
(push '("\\.md\\'" . markdown-mode) auto-mode-alist)

(push '(rjsx-mode . ("javascript-typescript-stdio")) eglot-server-programs)

;; replace the stock Flymake warning/error indicators with a bigger one for hidpi
;; maxmum width is 16px according to emacs docs
(define-fringe-bitmap 'flymake-big-indicator
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


;;; variables and faces


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.25)
 '(company-tooltip-align-annotations t)
 '(coverlay:mark-tested-lines nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(cua-mode t nil (cua-base))
 '(flymake-error-bitmap '(flymake-big-indicator compilation-error))
 '(flymake-eslint-executable-name "eslint_d")
 '(flymake-no-changes-timeout 0.5)
 '(flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
 '(frame-title-format '("%f") t)
 '(fringe-mode 20 nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-switch-indent-offset 2)
 '(js2-highlight-external-variables nil)
 '(package-selected-packages
   '(flymake-eslint origami powerline company package-lint package-lint-flymake treepy request smart-jump rjsx-mode web-mode-edit-element web-mode scss-mode multi-term markdown-mode json-mode eglot coverlay company-web company-quickhelp))
 '(powerline-display-buffer-size nil)
 '(powerline-display-hud nil)
 '(powerline-display-mule-info nil)
 '(scroll-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-warning ((t (:underline (:color "dark orange" :style wave)))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(js2-error ((t nil)))
 '(js2-external-variable ((t nil)))
 '(powerline-active0 ((t (:background "tomato" :foreground "white" :weight bold))))
 '(powerline-active1 ((t (:background "gray95"))))
 '(powerline-active2 ((t (:background "gray95"))))
 '(powerline-inactive0 ((t (:background "gray98"))))
 '(powerline-inactive1 ((t (:background "gray98"))))
 '(powerline-inactive2 ((t (:background "gray98"))))
 '(rjsx-attr ((t (:inherit rjsx-tag :weight normal))))
 '(rjsx-tag ((t (:foreground "dim gray" :weight bold))))
 '(rjsx-tag-bracket-face ((t (:inherit rjsx-tag))))
 '(rjsx-text ((t nil))))


;;; key commands


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
;; Ctrl-a -> select entire buffer
(global-set-key (kbd "C-a") 'mark-whole-buffer)
;; use useful Flycheck key bindings in Flymake
(define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)


;;; post-init


(add-hook 'after-init-hook
	  (lambda ()
            (company-quickhelp-mode)
            (powerline-default-theme)
;            (smart-jump-register :modes '(js2-mode eglot-mode))
            (auto-compression-mode -1)
            (auto-encryption-mode -1)
            (blink-cursor-mode -1)
            (file-name-shadow-mode -1)
            (global-auto-composition-mode -1)
            (package-refresh-contents t)
            ;; keep me last
	    (message (emacs-init-time))))

(provide 'emacs)
;;; .emacs ends here
