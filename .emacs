;;; .emacs --- Summary:
;;; Commentary:
;;; Code:


(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")


;;; package things


(package-initialize)

;; enable package-loading from MELPA
(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)

(push "~/.emacs.d/elisp" load-path)


;; these things are used by multiple major modes and/or get configured before any
;; buffers are opened, so they're not good candidates for autoload

;; code completion
(require 'company)
(require 'company-quickhelp)
;; code analysis
(require 'lsp)
(require 'lsp-clients)
;; mode line cleaner-upper
(require 'delight)
;; linting
(require 'flymake)
;; tree-navigation mode
(require 'treemacs-projectile)
;; change how files with the same basename are differentiated
(require 'uniquify)

;; these guys, however...

(autoload 'coverlay-minor-mode "coverlay"
  "Use the coverlay package to provide 'coverlay-minor-mode on-demand."
  t)
(autoload 'flymake-eslint-enable "flymake-eslint"
  "Use the flymake-eslint package to provide 'flymake-eslint-enable on-demand."
  t)
(autoload 'flymake-stylelint-enable "flymake-stylelint"
  "Use the flymake-stylelint package to provide 'flymake-stylelint-enable on-demand."
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


;;; utility functions and advices


(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun close-buffer ()
  "Closes current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun eslint-fix-buffer ()
  "Run `eslint --fix' on current buffer."
  (interactive)
  (if (executable-find "eslint_d")
      (progn
        (call-process "eslint_d" nil "*ESLint Errors*" nil "--fix" buffer-file-name)
        (revert-buffer t t t))
    (message "eslint_d not found")))

(defun ignore-file-p (name path)
  "Tell projectile to ignore some stuff based on the NAME and PATH of a file in the project."
  (or (string= name "node_modules")
      (string= name ".git")
      (string= name "coverage")))

(defun noop ()
  "Does nothing.  Use as a keyboard-shortcut handler instead of NIL to suppress the '<foo> is undefined' message."
  (interactive))

;; header- and mode-line rendering function
(defun status-line-render (left-content right-content)
  "Render LEFT-CONTENT and RIGHT-CONTENT, appropriately justified."
  (let* ((left-str (format-mode-line left-content))
         (right-str (format-mode-line right-content))
         (available-width
          (- (window-total-width)
             (length left-str)
             (length right-str)
             2))) ; left and right padding
    (format " %s%s%s "
            (propertize left-str 'face nil)
            (format (format "%%%ds" available-width) "")
            (propertize right-str 'face nil))))

;; prefer contents of help-at-point (e.g., Flymake) over whatever eldoc outputs
(advice-add 'eldoc-message :around
            (lambda (oldfn doc-msg)
              (let ((echo-help-string (help-at-pt-string)))
                (if echo-help-string
                    (display-local-help)
                  (funcall oldfn doc-msg)))))

;; shorten some major-mode modeline strings
(delight '((emacs-lisp-mode "ELisp")))

;; remove some minor-mode modeline strings
(delight '((subword-mode nil "subword")
           (company-mode nil "company")
           (lsp-mode nil "lsp")
           (projectile-mode nil t)
           (coverlay-minor-mode nil "coverlay")
           (eldoc-mode nil "eldoc")))

;; shorten dynamically-generated Flymake minor-mode string
(advice-add 'flymake--mode-line-format :filter-return
            (lambda (&rest return-value)
              (setf
               (seq-elt
                (car return-value)
                0)
               " ✓")
              return-value))


;;; mode hooks and config


(defun my-css-mode-hook ()
  "Do some things when opening [S]CSS files."
  (company-mode t)
  (eldoc-mode t)
  (flymake-stylelint-enable)
  (subword-mode t))

(defun my-flymake-mode-hook ()
  "Do some things when enabling Flymake."
  (help-at-pt-set-timer)
  (setq-local help-at-pt-display-when-idle t))

(defun my-javascript-mode-hook ()
  "Do some things when opening JavaScript files."
;;  ;; run `eslint --fix' upon save
;;  (add-hook 'after-save-hook 'eslint-fix-buffer t t)

  ;; assume JSX even if you don't see an import/require of React
  (js-jsx-enable)

  ;; treat this file as part of a larger project (when applicable)
  (projectile-mode)

  ;; code analysis via a language server
  (lsp)

  ;; add an eslint backend to flymake
  (flymake-eslint-enable)
  (setq flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js"))

  ;; code-completion
  (company-mode t)
  (company-quickhelp-mode t)

  ;; camelCase-aware navigation
  (subword-mode t)

  ;; turn on coverage mode if not a test file
  (unless (string-match-p "test.js[x]?\\'" buffer-file-name)
    (when-let
        ((coverage-dir
          (locate-dominating-file
           (buffer-file-name)
           "coverage")))
      (coverlay-minor-mode t)
      (setq coverlay:base-path
            (expand-file-name coverage-dir "coverage"))
      (unless
          (bound-and-true-p coverlay--loaded-filepath)
        ;; load the closest coverage file if one hasn't been loaded yet
        (coverlay-watch-file
         (concat
          (locate-dominating-file buffer-file-name "coverage")
          "coverage/lcov.info")))))

  ;; I want the key command in the xref map instead of this one
  (define-key js-mode-map (kbd "M-.") nil))

(defun my-json-mode-hook ()
  "Do some things when opening JSON files."
  (make-local-variable 'js-indent-level)

  ;; enable camelCase-aware navigation
  (subword-mode t))

(defun my-js-json-mode-hook ()
  "Emacs' json major mode descends from its js major mode, so the hook situation is all messed up without something like this."
  (if (string-match-p "json\\'" (buffer-file-name))
      (my-json-mode-hook)
    (my-javascript-mode-hook)))

(defun common-lisp-mode-hook ()
  "Do some things when entering a Lisp mode."

  ;; enable code-completion mode
  (company-mode t)
  (company-quickhelp-mode t)

  ;; enable documentation in echo area
  (eldoc-mode t)

  ;; linter (most of the time)
  (when (not (string= (buffer-name) "*scratch*"))
    (flymake-mode t)))

(defun my-terminal-mode-hook ()
  "Do some things when opening a terminal."

  ;; for terminals only, we want the default face to be reversed
  (face-remap-add-relative 'default '((:background "black") (:foreground "white")))

  ;; our terminal doesn't need a mode- or header-line
  (setq mode-line-format nil)
  (setq header-line-format nil) ; why doesn't this work?

  ;; it doesn't need fringes either
  ;; ...but all of the fringe-set functions either don't work or apply to the whole frame :(
  (face-remap-add-relative 'fringe '((:background "black")))

  ;; we don't need line/column numbers in our header line either
  (setq-local header-line-format
              '((:eval
                 (status-line-render
                  (format "%%b")
                  (format " "))))))

;; run custom functions when some major modes are entered
(add-hook 'scss-mode-hook 'my-css-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'common-lisp-mode-hook)
(add-hook 'js-mode-hook 'my-js-json-mode-hook)
(add-hook 'json-mode-hook 'my-js-json-mode-hook)
(add-hook 'lisp-mode-hook 'common-lisp-mode-hook)
(add-hook 'term-mode-hook 'my-terminal-mode-hook)
(add-hook 'flymake-mode-hook 'my-flymake-mode-hook)

;; associate some major modes with some file extensions
(push '("\\.js[x]?\\'" . js-mode) auto-mode-alist)
(push '("\\.json\\'" . json-mode) auto-mode-alist)
(push '("\\.[s]?css\\'" . scss-mode) auto-mode-alist)
(push '("\\.less\\'" . scss-mode) auto-mode-alist)
(push '("\\.md\\'" . markdown-mode) auto-mode-alist)


;;; variables and faces


;; HTML codes for the Source Code Pro glyphs to use as fringe indicators
(set-display-table-slot standard-display-table 'truncation 8230)
(set-display-table-slot standard-display-table 'wrap 8601)
;; transparent background-color for fringe
(set-face-attribute 'fringe nil :background nil)
;; hide fringe line-wrap bitmap
(assoc-delete-all 'continuation fringe-indicator-alist)
;; frame defaults
(setq-default default-frame-alist
              (list
               '(undecorated . t)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 20)
               '(drag-internal-border . t)
               '(font . "Source Code Pro Light 12")))
(set-frame-parameter (selected-frame)
                     'internal-border-width 20)

(push 'ignore-file-p treemacs-ignored-file-predicates)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(blink-cursor-mode nil)
 '(company-backends '(company-files company-capf company-semantic))
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-background nil)
 '(company-quickhelp-color-foreground nil)
 '(company-quickhelp-delay 1.0)
 '(company-tooltip-align-annotations t)
 '(coverlay:mark-tested-lines nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(cua-mode t nil (cua-base))
 '(debug-on-error t)
 '(flymake-error-bitmap '(flymake-big-indicator compilation-error))
 '(flymake-eslint-executable-args "\"-f unix\"")
 '(flymake-eslint-executable-name "eslint")
 '(flymake-jslint-args '("--no-color" "--no-ignore" "--stdin"))
 '(flymake-jslint-command "eslint")
 '(flymake-no-changes-timeout 0.5)
 '(flymake-stylelint-executable-args "-q")
 '(flymake-stylelint-executable-name "stylelint")
 '(flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
 '(font-use-system-font nil)
 '(frame-title-format '("%f") t)
 '(fringe-mode '(24 . 8) nil (fringe))
 '(header-line-format
   '((:eval
      (status-line-render
       (format "%s %%b"
               (if
                   (and
                    (buffer-file-name)
                    (buffer-modified-p))
                   "*" " "))
       (format "%s"
               (format-mode-line "%l:%c"))))) t)
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.25)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-chain-indent nil)
 '(js-enabled-frameworks nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 4)
 '(lsp-auto-configure nil)
 '(lsp-diagnostic-package :flymake)
 '(lsp-enable-snippet nil)
 '(menu-bar-mode nil)
 '(mode-line-format
   '((:eval
      (status-line-render
       (format-mode-line
        (list " " mode-name minor-mode-alist))
       (if
           (stringp vc-mode)
           (format "%s"
                   (format "%s%s"
                           (char-to-string 57504)
                           (format-mode-line
                            '(vc-mode vc-mode))))
         "")))))
 '(package-selected-packages
   '(auto-dim-other-buffers lsp-ui lsp-mode flymake-json editorconfig dotenv-mode web-mode js-doc projectile treemacs-projectile treemacs 2048-game dockerfile-mode request flymake-stylelint company scss-mode markdown-mode json-mode flymake-eslint delight coverlay company-quickhelp))
 '(read-process-output-max (* 1024 1024) t)
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(widget-image-enable nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:inherit company-tooltip-selection))))
 '(company-scrollbar-bg ((t (:background "gray95"))))
 '(company-scrollbar-fg ((t (:background "#808080"))))
 '(company-tooltip-annotation ((t (:foreground "firebrick4"))))
 '(company-tooltip-common ((t (:foreground "black"))))
 '(company-tooltip-selection ((t (:background "steel blue" :foreground "white"))))
 '(flymake-warning ((t (:underline (:color "dark orange" :style wave)))))
 '(font-lock-comment-face ((t (:inherit default :foreground "Firebrick"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "steel blue" :weight bold))))
 '(header-line ((t (:inherit default :background "gray95" :foreground "gray33" :weight bold))))
 '(mode-line ((t (:background "steel blue" :foreground "white" :weight bold))))
 '(mode-line-highlight ((t (:inherit mode-line))))
 '(mode-line-inactive ((t (:inherit nil :background "nil" :foreground "gray33" :weight bold))))
 '(region ((t (:extend t :background "gray90" :distant-foreground "gtk_selection_fg_color"))))
 '(term ((t (:background "black" :foreground "white"))))
 '(term-bold ((t (:background "black" :foreground "white" :weight bold))))
 '(term-color-blue ((t (:background "DeepSkyBlue4" :foreground "DeepSkyBlue4"))))
 '(treemacs-directory-face ((t (:inherit font-lock-function-name-face :weight normal))))
 '(treemacs-git-ignored-face ((t (:inherit font-lock-comment-face))))
 '(treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(treemacs-git-untracked-face ((t (:inherit nil :foreground "forest green" :weight bold)))))


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
;; keyboard shortcut to force auto-completion
(define-key company-mode-map (kbd "M-/") 'company-complete)
;; keyboard shortcut to find a file in the current project, like VSCode does
(define-key projectile-mode-map (kbd "C-p") 'projectile-find-file)
;; I don't use these
(global-set-key (kbd "C-b") 'noop)
(global-set-key (kbd "C-f") 'noop)
(global-set-key (kbd "C-n") 'noop)
(global-set-key (kbd "C-p") 'noop)
(global-set-key (kbd "C-x C-k") nil)
;; Ctrl-w -> close buffer.  CUA-ish
(global-set-key (kbd "C-w") 'close-buffer)


;;; post-init


(add-hook 'after-init-hook
          (lambda ()
            (auto-compression-mode -1)
            (auto-encryption-mode -1)
            (blink-cursor-mode -1)
            (file-name-shadow-mode -1)
            (global-auto-composition-mode -1)
            (package-refresh-contents t)
            ;; project view
            (treemacs)
            ;; open a terminal at the bottom of the frame
            (select-window (next-window))
            (select-window (split-window-below -10))
            (term "/bin/bash")
            (select-window (previous-window))
            ;; keep me last
            (message "startup time: %s" (emacs-init-time))))

(provide 'emacs)
;;; .emacs ends here
