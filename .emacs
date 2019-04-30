;;; .emacs --- Summary:
;;; Commentary:
;;; Code:


(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")

;;;
;;; packages
;;;


;; load any non-standard packages from here
(add-to-list 'load-path "~/.emacs.d/lisp")

;; enable package-loading from MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; these packages are used by multiple major modes and/or I want to set some config on them
;; at bootup; they're not good candidates for autoload

;; code completion
(require 'company)
(require 'company-quickhelp) ; documentation tooltips for code-completion popup
(require 'company-web-html) ; not an ELPA package, but rather provided by company
;; mode line cleaner-upper
(require 'delight)
;; code analysis
(require 'eglot)
;; mode line customization
(require 'powerline)

;; these guys, however...

(autoload 'coverlay-minor-mode "coverlay"
  "Use the coverlay package to provide 'coverlay-minor-mode on-demand."
  t)
(autoload 'flymake-eslint-enable "flymake-eslint"
  "Use the flymake-eslint package to provide 'flymake-eslint-enable on demand."
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
(autoload 'rjsx-mode "rjsx-mode"
  "Use the rjsx-mode package to provide 'rjsx-mode on-demand."
  t)


;;;
;;; utility functions and advices
;;;


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

;; tell powerline to honor the modifications made by delight
(advice-add 'powerline-major-mode :around
            (lambda (original-fn &rest args)
              (let ((inhibit-mode-name-delight nil))
                (funcall original-fn args))))
;; shorten "Emacs-Lisp" major-mode string
(delight '((emacs-lisp-mode "ELisp")))
;; remove some minor-mode strings
(delight '((subword-mode nil "subword")
           (company-mode nil "company")
           (coverlay-minor-mode nil "coverlay")
           (eldoc-mode nil "eldoc")
           (auto-dim-other-buffers-mode nil "auto-dim-other-buffers")))
;; shorten dynamically-generated Flymake minor-mode string
(advice-add 'flymake--mode-line-format :filter-return
            (lambda (&rest return-value)
              (setf (seq-elt (car return-value) 0) " !")
              return-value))

;;;
;;; mode-specific config
;;;


(defun common-javascript-mode-hook ()
  "Do some things when entering a javascript mode."
  ;; turn on camelCase-aware code navigation
  (subword-mode t)
  ;; enable code-completion mode
  (company-mode t)
  (company-quickhelp-mode t)
  ;; hook up to LSP server
  ;; tell eglot to ignore its own Flymake backend (which doesn't seem to do anything)
  (add-hook 'eglot--managed-mode-hook
            (lambda ()
              (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t) ; t for buffer-local
              (flymake-eslint-enable)))
  (eglot-ensure)
  ;; lint upon save
;  (add-hook 'after-save-hook 'lint-fix-js t t)
  ;; code-coverage
  (unless (string-match-p "test.js[x]?\\'" buffer-file-name)
    ;; turn on coverage mode if not a test file

    (coverlay-minor-mode t)
    (unless (bound-and-true-p coverlay--loaded-filepath)
      ;; load the closest coverage file if one hasn't been loaded yet
      (coverlay-watch-file (concat
                            (locate-dominating-file buffer-file-name "coverage")
                            "coverage/lcov.info")))))

(defun common-lisp-mode-hook ()
  "Do some things when entering a Lisp mode."
  ;; enable code-completion mode
  (company-mode t)
  (company-quickhelp-mode t)
  ;; enable documentation in echo area
  (eldoc-mode t)
  ;; linter
  (flymake-mode t))

;; do some things after entering certain major modes
(add-hook 'rjsx-mode-hook 'common-javascript-mode-hook)
(add-hook 'lisp-mode-hook 'common-lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'common-lisp-mode-hook)

;; associate some major modes with some file extensions
(push '("\\.js[x]?\\'" . rjsx-mode) auto-mode-alist)

;; tell eglot to use the JS language server for web-mode
(push '(rjsx-mode . ("javascript-typescript-stdio")) eglot-server-programs)


;; tell eglot to prefer help-at-point (e.g. Flymake) over eldoc
(advice-add 'eglot-eldoc-function :around
            (lambda (original-fn)
              (let ((help (help-at-pt-kbd-string)))
                (if help (message "%s" help) (funcall original-fn)))))


;;;
;;; variables
;;;


;; replace the stock Flymake warning/error indicators with a bigger one for hidpi
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
 '(auto-dim-other-buffers-mode t)
 '(company-quickhelp-color-background nil)
 '(company-quickhelp-color-foreground nil)
 '(company-tooltip-align-annotations t)
 '(coverlay:mark-tested-lines nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(cua-mode t nil (cua-base))
 '(flymake-error-bitmap (quote (flycheck-big-indicator compilation-error)))
 '(flymake-eslint-executable-name "eslint_d")
 '(flymake-warning-bitmap (quote (flycheck-big-indicator compilation-warning)))
 '(fringe-mode (quote (20)) nil (fringe))
 '(help-at-pt-display-when-idle (quote (flymake-diagnostic)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.25)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-include-browser-externs nil)
 '(js2-include-jslint-declaration-externs nil)
 '(js2-include-jslint-globals nil)
 '(js2-indent-switch-body t)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-cond-assign-warning nil)
 '(js2-strict-inconsistent-return-warning nil)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-var-hides-function-arg-warning nil)
 '(js2-strict-var-redeclaration-warning nil)
 '(mode-line-format
   (quote
    ("%e"
     (:eval
      (let*
          ((active
            (powerline-selected-window-active))
           (mode-line-buffer-id
            (if active
                (quote mode-line-buffer-id)
              (quote mode-line-buffer-id-inactive)))
           (mode-line
            (if active
                (quote mode-line)
              (quote mode-line-inactive)))
           (face0
            (if active
                (quote powerline-active0)
              (quote powerline-inactive0)))
           (face1
            (if active
                (quote powerline-active1)
              (quote powerline-inactive1)))
           (face2
            (if active
                (quote powerline-active2)
              (quote powerline-inactive2)))
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
             (powerline-raw "%*" face0
                            (quote l))
             (powerline-buffer-id
              (\`
               (mode-line-buffer-id
                (\, face0)))
              (quote l))
             (powerline-raw " " face0)
             (funcall separator-left face0 face1)
             (powerline-raw " " face1)
             (powerline-major-mode face1)
             (powerline-process face1)
             (powerline-minor-modes face1
                                    (quote l))
             (powerline-narrow face1
                               (quote l))
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (powerline-vc face2
                           (quote r))))
           (rhs
            (list
             (powerline-raw global-mode-string face2
                            (quote r))
             (funcall separator-right face2 face1)
             (unless window-system
               (powerline-raw
                (char-to-string 57505)
                face1
                (quote l)))
             (powerline-raw "%4l" face1
                            (quote l))
             (powerline-raw ":" face1
                            (quote l))
             (powerline-raw "%3c" face1
                            (quote r))
             (funcall separator-right face1 face0)
             (powerline-raw " " face0)
             (powerline-fill face0 0))))
        (concat
         (powerline-render lhs)
         (powerline-fill face2
                         (powerline-width rhs))
         (powerline-render rhs)))))))
 '(package-selected-packages
   (quote
    (auto-dim-other-buffers delight company rjsx-mode xref-js2 flymake-eslint yaml-mode scss-mode request powerline markdown-mode json-mode idle-highlight-mode git-commit eglot coverlay company-web company-quickhelp)))
 '(powerline-display-buffer-size nil)
 '(powerline-display-hud nil)
 '(powerline-display-mule-info nil)
 '(powerline-gui-use-vcs-glyph t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#f8f8f8" :foreground "#808080"))))
 '(company-scrollbar-bg ((t (:background "gray95"))))
 '(company-scrollbar-fg ((t (:background "tomato"))))
 '(company-tooltip-annotation ((t (:foreground "firebrick4"))))
 '(company-tooltip-common ((t (:foreground "black"))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 '(flymake-warning ((t (:underline (:color "dark orange" :style wave)))))
 '(js2-error ((t nil)))
 '(js2-warning ((t nil)))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(powerline-active0 ((t (:background "tomato" :foreground "white" :weight bold))))
 '(powerline-active1 ((t (:background "gray95"))))
 '(powerline-active2 ((t (:background "gray95"))))
 '(powerline-inactive0 ((t (:background "gray98"))))
 '(powerline-inactive1 ((t (:background "gray98"))))
 '(powerline-inactive2 ((t (:background "gray98"))))
 '(rjsx-attr ((t (:foreground "dim gray"))))
 '(rjsx-tag ((t (:foreground "dim gray"))))
 '(rjsx-tag-bracket-face ((t (:inherit default :foreground "dim gray" :weight bold)))))


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


;; a bunch of stuff I want to do upon startup but which didn't fit anywhere else
(add-hook 'after-init-hook
          (lambda ()
;            (set-face-background 'hl-line "#f8f8f8")
            (global-eldoc-mode -1)
;            (push "~/.npm-global/bin" exec-path)
            (package-refresh-contents t) ; refresh contents on startup
            ;; keep me as last
            (message "startup time: %s" (emacs-init-time))))

(provide '.emacs)
;;; .emacs ends here
