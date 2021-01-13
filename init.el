;;; init.el --- personal config file -*- lexical-binding: t; -*-
;;; Summary:
;;; Commentary:
;;; Code:


(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq gc-cons-threshold most-positive-fixnum)

(defvar my-gc-cons-threshold (* 1024 1024 20))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package
 '(bbjson-mode :type git :host github :repo "orzechowskid/bbjson-mode.el"))
(straight-use-package 'company)
(straight-use-package 'company-web)
(straight-use-package 'counsel)
(straight-use-package
 '(css-in-js-mode :type git :host github :repo "orzechowskid/css-in-js.el"))
(straight-use-package 'dap-mode)
(straight-use-package 'delight)
(straight-use-package 'dockerfile-mode)
(straight-use-package 'eldoc)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'flymake-eslint)
(straight-use-package
 '(flymake-stylelint :type git :host github :repo "orzechowskid/flymake-stylelint"))
(straight-use-package 'lsp-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'mmm-mode)
(straight-use-package 'projectile)
(straight-use-package 'scss-mode)
(straight-use-package 'typescript-mode)
(straight-use-package 'vs-light-theme)
(straight-use-package 'yaml-mode)

(with-eval-after-load 'css-mode
  ;; until someone (*cough*) files a PR against css-mode
  (push "sticky" (alist-get "position" css-property-alist nil nil #'equal)))

(with-eval-after-load 'projectile
  ;; Ctrl-p -> find file in project
  (define-key projectile-mode-map (kbd "C-p") 'projectile-find-file))

(with-eval-after-load 'counsel
  (push '(counsel-find-file . my/find-file-regex) ivy-re-builders-alist))

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome))

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "M-/") 'company-complete))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
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
    16 16 'center))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(c-basic-offset 2)
 '(company-backends
   '(company-semantic company-capf company-files company-web-html))
 '(company-idle-delay 0.0)
 '(counsel-find-file-ignore-regexp "\\(?:\\`\\|[/\\]\\)\\(?:[#.]\\)")
 '(css-indent-offset 2)
 '(debug-on-error nil)
 '(flymake-error-bitmap '(flymake-big-indicator compilation-error))
 '(flymake-stylelint-executable-args "-q")
 '(flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
 '(fringe-mode '(24 . 0) nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ivy-magic-tilde nil)
 '(js-enabled-frameworks nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(lsp-diagnostics-provider t)
 '(lsp-headerline-breadcrumb-enable nil)
 '(menu-bar-mode nil)
 '(projectile-completion-system 'ivy)
 '(smie-config nil)
 '(straight-check-for-modifications '(check-on-save find-when-checking))
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(vc-make-backup-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:inherit company-tooltip-selection))))
 '(company-preview-common ((t nil)))
 '(company-preview-search ((t nil)))
 '(company-scrollbar-bg ((t (:background "gray95"))))
 '(company-scrollbar-fg ((t (:background "#808080"))))
 '(company-tooltip ((t (:background "white smoke" :foreground "black"))))
 '(company-tooltip-annotation ((t (:foreground "gray45"))))
 '(company-tooltip-common ((t (:foreground "gray14" :weight normal))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(dap-ui-breakpoint-verified-fringe ((t (:background "black" :weight bold))))
 '(dap-ui-pending-breakpoint-face ((t (:background "pale goldenrod"))))
 '(flymake-warning ((t (:underline (:color "dark orange" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "#647892"))))
 '(highlight ((t (:background "light steel blue"))))
 '(mode-line ((t (:background "steel blue" :foreground "white" :weight normal))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((t (:background "grey75" :foreground "white")))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my/delete-word (- arg)))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my/find-file-regex (pattern)
  "Better matching for `counsel-find-file'.  Only search for PATTERN at the start of file basenames."
  (concat "^" pattern))

(defun my/configure-frames-and-fonts ()
  ;; add some fonts to the standard fontset
  ;; (highest-priority first)
  (let ((my-fonts '("Source Code Pro Light 12"
                    "Symbola 12")))
    (create-fontset-from-fontset-spec standard-fontset-spec)
    (dolist (font (reverse my-fonts))
      (set-fontset-font "fontset-standard" 'unicode font nil 'prepend)))
  ;; HTML codes for the Source Code Pro glyphs to use as fringe indicators
  ;; (requires fringes to be nil)
  (set-display-table-slot standard-display-table 'truncation 8230)
  (set-display-table-slot standard-display-table 'wrap 8601)
  ;; set some frame parameters
  (setq-default
   default-frame-alist
   (list
    '(font . "fontset-standard")
    '(internal-border-width . 20)
    '(undecorated . t)
    '(vertical-scroll-bars . nil))))

(defun my/configure-keyboard-shortcuts ()
  ;; make Ctrl-x / Ctrl-v / etc behave as expected
  (cua-mode)
  ;; Ctrl-Backspace -> delete a word instead of killing it
  (global-set-key [C-backspace] 'my/backward-delete-word)
  ;; Ctrl-PgDn -> next window
  (global-set-key [C-next] 'other-window)
  ;; Ctrl-PgUp -> previous window
  (global-set-key [C-prior]
                  (lambda ()
                    (interactive)
                    (other-window -1)))
  ;; Ctrl-a -> select entire buffer
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  ;; M-x -> counsel extended-command finder
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; Ctrl-x Ctrl-f -> counsel file finder
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; Ctrl-x b -> counsel buffer switcher
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)

  ;; see also the various `with-eval-after-load' calls for more shortcut assignments
  )

(defun my/configure-misc ()
  (global-linum-mode t)
  (blink-cursor-mode -1)
  ;; transparent fringe background
  (set-face-attribute 'fringe nil :background nil)
  ;; no arrow bitmaps on fringe
  (assoc-delete-all 'continuation fringe-indicator-alist)
  ;; nvm / node.js
  (exec-path-from-shell-initialize)
  ;; camelCase-aware navigation
  (global-subword-mode t)
  ;; prefer contents of help-at-point (e.g., Flymake) over whatever eldoc outputs
  (advice-add
   'eldoc-message :around
   (lambda (oldfn doc-msg)
     (let ((echo-help-string (help-at-pt-string)))
       (if echo-help-string
           (display-local-help)
         (funcall oldfn doc-msg)))))
  ;; apply theme
  (vs-light-theme))

(defun my/render-modeline (left-content center-content right-content)
  "Return a string containing LEFT-CONTENT and RIGHT-CONTENT appropriately justified."
  (let* ((left-str (format-mode-line left-content))
         (center-str (format-mode-line center-content))
         (right-str (format-mode-line right-content))
         (column-width (/ (window-total-width) 3))
         (left-gutter 1)
         (right-gutter 1)
         (left-spacing (- column-width
                          (+ left-gutter
                             (length left-str))))
         (right-spacing (- (window-total-width)
                           right-gutter
                           (length right-str)
                           (length center-str)
                           left-spacing
                           (length left-str)
                           left-gutter)))
                           
    (format "%s%s%s%s%s%s%s"
            (format (format "%%%ds" left-gutter) "")
            left-str
            (format (format "%%%ds" left-spacing) "")
            center-str
            (format (format "%%%ds" right-spacing) "")
            right-str
            (format (format "%%%ds" right-gutter) ""))))

(defun my/configure-modeline ()
  ;; modify some major-mode strings
  (delight '((emacs-lisp-mode "ELisp")))
  ;; remove some minor-mode strings ('C-h m' if I need to see them)
  (delight
   ;; ( <mode> <replacement string> <file which provides <mode>> )
   '((company-mode nil "company")
     (eldoc-mode nil "eldoc")
     (lsp-mode nil "lsp-mode")
     (mmm-mode nil nil)
     (projectile-mode nil "projectile")
     (subword-mode nil "subword")))
  ;; customize the flymake mode-line string
  (advice-add
   'flymake--mode-line-format :filter-return
   (lambda (&rest return-value)
     (setf (seq-elt (car return-value) 0) " ✓")
     return-value))
  (setq-default mode-line-format
        '((:eval
           (my/render-modeline
            ;; left
            (list
             (if (and (buffer-modified-p) (buffer-file-name)) "⚫ " "  ")
             (propertize "%b" 'face 'mode-line-buffer-id))
            ;; center
            (list
             mode-name
             minor-mode-alist)
            ;; right
            (list
             (if (stringp vc-mode)
                 (format "%s"
                         (format "%s%s"
                                 (char-to-string 57504)
                                 (format-mode-line '(vc-mode vc-mode))))
               "")))))))

(defun my/configure-file-associations ()
  ;; associate file types with major modes
  (push '("\\.js[x]?\\'" . js-mode) auto-mode-alist)
  (push '("\\.json\\'" . bbjson-mode) auto-mode-alist)
  (push '("\\.[s]?css\\'" . scss-mode) auto-mode-alist)
  (push '("\\.less\\'" . scss-mode) auto-mode-alist)
  (push '("\\.md\\'" . markdown-mode) auto-mode-alist)
  (push '("\\.y[a]?ml\\'" . yaml-mode) auto-mode-alist)
  (push '("\\.ts[x]?\\'" . typescript-mode) auto-mode-alist)
  ;; run custom functions when some major modes are entered
  (add-hook 'scss-mode-hook 'my/css-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my/common-lisp-mode-hook)
  (add-hook 'js-mode-hook 'my/js-mode-hook)
;  (add-hook 'json-mode-hook 'my/json-mode-hook)
  (add-hook 'lisp-mode-hook 'my/common-lisp-mode-hook)
;  (add-hook 'term-mode-hook 'my/terminal-mode-hook)
;  (add-hook 'flymake-mode-hook 'my/flymake-mode-hook)
  (add-hook 'typescript-mode-hook 'my/ts-mode-hook)
  (add-hook 'minibuffer-setup-hook 'my/minibuf-entrance-hook)
  (add-hook 'minibuffer-exit-hook 'my/minibuf-exit-hook)
  (add-hook 'mhtml-mode-hook 'my/html-mode-hook))

(defun my/css-mode-hook ()
  (lsp)
  (setq-local company-backends '(company-web-html company-files company-capf))
  (flymake-stylelint-enable)
  (company-mode t))

(defun my/html-mode-hook ()
  (setq-local company-backends '(company-web-html company-files company-capf))
  (company-mode t)
  ;; close the current tag upon '</'
  (setq sgml-quick-keys 'close))

(defun my/js-mode-hook ()
  (projectile-mode t)
  (lsp)
  (setq-local js-jsx-syntax t)
  (setq-local company-backends '(company-capf company-web-html company-files))
  (flymake-eslint-enable)
  ;; I want the key command in the xref map (which is LSP-aware) instead of this one
  (define-key js-mode-map (kbd "M-.") nil))

(defun my/json-mode-hook ()
  (projectile-mode t))

(defun my/ts-mode-hook ()
  (projectile-mode)
  (lsp)
  (flymake-eslint-enable))

(defun my/common-lisp-mode-hook ()
  (company-mode t))

(defun my/minibuf-entrance-hook ()
  ;; stolen from Doom
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuf-exit-hook ()
  ;; stolen from Doom
  (run-at-time
   1
   nil
   (lambda ()
     (setq gc-cons-threshold my-gc-cons-threshold))))

(defun my/update-packages ()
  (interactive)
  (straight-fetch-all)
  (straight-pull-all)
  (straight-rebuild-all))

(add-hook
 'after-init-hook
 (lambda ()
   (my/configure-file-associations)
   (my/configure-frames-and-fonts)
   (my/configure-keyboard-shortcuts)
   (my/configure-modeline)
   (my/configure-misc)
   ;; keep these last
   (setq gc-cons-threshold my-gc-cons-threshold)
   (message "startup time: %s" (emacs-init-time))))

(provide 'init.el)
;;; init.el ends here
