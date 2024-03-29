;;; init.el --- personal config file -*- lexical-binding: t; -*-
;;; Summary:
;;; Commentary:
;;; Code:

;;(setq debug-on-error t)

;; try real hard to use UTF-8 everywhere all the time
;; (some of this might be unnecessary and/or deprecated)
(set-language-environment "UTF-8")
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; memory management: perform no GC during startup, then raise the limit from the
;; default value to something more suitable for modern machines
(defvar my/gc-cons-threshold (* 1024 1024 20))
(setq gc-cons-threshold most-positive-fixnum)

;; a directory for any custom scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-hook
 'after-init-hook
 (lambda ()
   (my/configure-file-associations)
   (my/configure-global-keyboard-shortcuts)
   (my/configure-modeline)
   (my/configure-misc)
   ;; keep these last
   (setq gc-cons-threshold my/gc-cons-threshold)
   (message "startup time: %s" (emacs-init-time))))

(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-protocol 'ssh)
(setq straight-vc-git-force-protocol t)
(setq straight-vc-git-default-clone-depth 1)
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

(mapcar
 'straight-use-package
 '((bbjson-mode :type git :protocol ssh :host github :repo "orzechowskid/bbjson-mode.el")
   company
   consult
   coverlay
   (css-in-js-mode :type git :protocol ssh :host github :repo "orzechowskid/css-in-js.el")
   delight
   dockerfile-mode
   dotenv-mode
   eldoc
   '(eslint-disable-rule :type git :protocol ssh :host github :repo "DamienCassou/eslint-disable-rule")
   exec-path-from-shell
   flycheck
   (flymake-stylelint :type git :protocol ssh :host github :repo "orzechowskid/flymake-stylelint")
   lsp-mode
   magit
   marginalia
   prescient
   projectile
   typescript-mode
   scss-mode
   selectrum
   selectrum-prescient
   tree-sitter
   tree-sitter-langs
   (tsi :type git :protocol ssh :host github :repo "orzechowskid/tsi.el")
   (tsx-mode :type git :protocol ssh :host github :repo "orzechowskid/tsx-mode.el")
   vs-light-theme
   yaml-mode))

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "M-/") 'company-complete))

(with-eval-after-load 'flycheck
  ;; try some CSS-in-JS linting magic
  (flycheck-add-mode 'css-stylelint 'typescript-mode)
  (flycheck-add-mode 'css-stylelint 'tsx-mode))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  ;; steal the flycheck error indicator for flymake's use
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

(with-eval-after-load 'magit
  ;; sometimes it's up to us to inform vc-mode of changes to the repo
  (advice-add 'magit-checkout :after
              (lambda (&rest ignored)
                (vc-refresh-state)))
  (advice-add 'magit-branch-and-checkout :after
              (lambda (&rest ignored)
                (vc-refresh-state))))

(with-eval-after-load 'markdown
  (define-key markdown-mode-map (kbd "M-.") 'markdown-follow-link-at-point))

(with-eval-after-load 'projectile
  ;; Ctrl-p -> find file in project
  (define-key projectile-mode-map (kbd "C-p") 'projectile-find-file))

(with-eval-after-load 'rjsx-mode
  ;; prefer the jump function provided by LSP and/or xref-jsx
  (define-key js2-mode-map (kbd "M-.") nil))

(with-eval-after-load 'tree-sitter-langs
  (tree-sitter-require 'json)
  (add-to-list 'tree-sitter-major-mode-language-alist '(bbjson-mode . json))
  (tree-sitter-require 'css)
  (add-to-list 'tree-sitter-major-mode-language-alist '(scss-mode . css)))

(with-eval-after-load 'selectrum-prescient
  (selectrum-mode t)
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))

(mapcar
 'require
 '(consult
   prescient
   selectrum-prescient
   tree-sitter-langs))

(defun my/configure-file-associations ()
  ;; a hook which configures things common to all programming modes
  (add-hook
   'prog-mode-hook
   (lambda ()
     (auto-composition-mode 0)
     (mouse-wheel-mode 0)
     (projectile-mode)
     (subword-mode)
     (show-paren-mode)
     (global-display-fill-column-indicator-mode)
     (tooltip-mode 0)))

  (push '("\\.json\\'" . bbjson-mode) auto-mode-alist)

  ;; mode-specific hooks
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (company-mode)))
  (push '("\\.js[x]?\\'" . javascript-mode) auto-mode-alist)
  (add-hook
   'tsx-mode-hook
   (lambda ()
     (setq-local coverlay:base-path
           (let* ((package-json (locate-dominating-file (buffer-file-name (current-buffer)) "package.json")))
             (file-name-directory (expand-file-name package-json))))
     (coverlay-watch-file (concat coverlay:base-path "coverage/lcov.info"))))
  (push '("\\.ts[x]?\\'" . tsx-mode) auto-mode-alist)
  (add-hook
   'js-mode-hook
   (lambda ()
     (lsp)))
  (push '("\\.mjs\\'" . javascript-mode) auto-mode-alist)
  (add-hook
   'scss-mode-hook
   (lambda ()
     (company-mode)
     (flymake-stylelint-enable)))
  (push '("\\.[s]?css\\'" . scss-mode) auto-mode-alist)
  (add-hook
   'html-mode-hook
   (lambda ()
     (company-mode)
     ;; close the current tag upon '</'
     (setq sgml-quick-keys 'close))))

(defun my/configure-misc ()
  ;; TODO: this doesn't feel great
  ;; ;; show documentation info at point as well as warnings/errors at point
  ;; (advice-add
  ;;  'eldoc-message :after
  ;;  (lambda (oldfn &rest args)
  ;;    "Do some stuff to the Help buffer too"
  ;;    (let ((help-msg
  ;;           (help-at-pt-string))
  ;;          (eldoc-msg
  ;;           eldoc-last-message))
  ;;      ;; write any docs for the current symbol to the Help buffer
  ;;      (when eldoc-msg
  ;;        (save-window-excursion
  ;;          (with-help-window (help-buffer)
  ;;            (princ eldoc-msg))))
  ;;      ;; write any warnings/errors to the echo area
  ;;      (when help-msg
  ;;        (display-local-help)))))
  (advice-add
   'eldoc-message :around
   (lambda (oldfn doc-msg)
     (let ((echo-help-string (help-at-pt-string)))
       (if echo-help-string
           (display-local-help)
         (funcall oldfn doc-msg)))))
  ;; memory management while inside minibuffer
  (add-hook 'minibuffer-setup-hook 'my/minibuf-setup-hook)
  (add-hook 'minibuffer-exit-hook 'my/minibuf-exit-hook)
  ;; apply theme and appearance preferences
  (vs-light-theme)
  (set-face-attribute 'fringe nil :background nil)
  (assoc-delete-all 'continuation fringe-indicator-alist)
  ;; apply some globally-helpful modes
  (global-display-fill-column-indicator-mode t)
  (global-display-line-numbers-mode t)
  (marginalia-mode t)
  (global-subword-mode t)
  ;; I got 99 columns but the fringe ain't one
  (setq-default fill-column 99)
  ;; disable some globally-unhelpful modes
  (blink-cursor-mode 0)
  (auto-composition-mode 0)
  (auto-compression-mode 0)
  (auto-encryption-mode 0)
  (dirtrack-mode 0)
  ;; set `exec-path` to something resonable
  (exec-path-from-shell-initialize)
  ;; store all backup files in a separate directory so we don't pollute our projects
  (defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
  (setq backup-directory-alist (list (cons ".*" (expand-file-name "~/.emacs.d/backup/"))))
  (setq auto-save-list-file-prefix autosave-dir)
  (setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
  ;; if I ever need to pass arguments to git I can add back the pound character myself
  (consult-customize
   consult-git-grep
   :initial nil))

(defun my/configure-modeline ()
  ;; modify some major-mode strings
  (delight '((emacs-lisp-mode "ELisp")))
  ;; remove some minor-mode strings ('C-h m' if I need to see them)
  (delight
   ;; ( <mode> <replacement string> <file which provides <mode>> )
   '((company-mode nil "company")
     (coverlay-minor-mode nil "coverlay")
     (eldoc-mode nil "eldoc")
     (lsp-mode nil "lsp-mode")
     (mmm-mode nil nil)
     (projectile-mode nil "projectile")
     (subword-mode nil "subword")
     (tree-sitter-mode nil "tree-sitter")))
  ;; customize the flymake mode-line string
  (setq flymake-mode-line-title " ✓")
  (advice-add
   'flymake--mode-line-counter :filter-return
   (lambda (return-value)
     (mapcar
      (lambda (list-item)
        (list
         :eval
         (if (length> list-item 1)
             (elt list-item 1)
           "")))
      return-value)))

  (setq-default
   mode-line-format
   '((:eval
      (my/render-modeline
       ;; left
       (list
	(if (and (buffer-modified-p) (buffer-file-name)) "● " "  ")
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
			    ;; the little branch-switcheroo guy
			    (char-to-string 57504)
			    (format-mode-line '(vc-mode vc-mode))))
	  "")))))))

(defun my/configure-org ()
  "Configure org-mode stuff."
  (require 'org-gcal)
  (setq org-gcal-client-id "809538603920-qie1hktpdvmbncr14tcvo1cg0d1roh7l.apps.googleusercontent.com"
        org-gcal-client-secret "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"
        org-gcal-fetch-file-alist '(("orzechod@gmail.com" . "~/calendar-orzechod.org")
                                    ("r1mv4jsief5rfbj385miemh1c4@group.calendar.google.com" . "~/calendar-shared.org"))))

(defun my/find-file-regex (pattern)
  "Better matching for `counsel-find-file'.  Only search for PATTERN at the start of file basenames."
  (concat "^" pattern))

(defun my/magit-create-checkout (&optional branch-name)
  (interactive "Mnew branch name: ")
  ;; TODO: check for presence of git repo
  (message "branch name is %s" branch-name))

(defun my/minibuf-setup-hook ()
  ;; stolen from Doom
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuf-exit-hook ()
  ;; stolen from Doom
  (run-at-time
   1
   nil
   (lambda ()
     (setq gc-cons-threshold my/gc-cons-threshold))))

(defun my/render-modeline (left-content center-content right-content)
  "Return a string containing LEFT-CONTENT and RIGHT-CONTENT appropriately justified."
  (let*
      ((left-str (format-mode-line left-content))
       (center-str (format-mode-line center-content))
       (right-str (format-mode-line right-content))
       (column-width (/ (window-total-width) 3))
       (left-gutter 1)
       (right-gutter 1)
       (left-spacing
	(- column-width
           (+ left-gutter
              (length left-str))))
       (right-spacing
	(- (window-total-width)
           right-gutter
           (length right-str)
           (length center-str)
           left-spacing
           (length left-str)
           left-gutter)))
    
    (format
     "%s%s%s%s%s%s%s"
     (format (format "%%%ds" left-gutter) "")
     left-str
     (format (format "%%%ds" left-spacing) "")
     center-str
     (format (format "%%%ds" right-spacing) "")
     right-str
     (format (format "%%%ds" right-gutter) ""))))

(defun my/rotate-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bbjson-indent-offset 2)
 '(company-backends '(company-capf))
 '(coverlay:tested-line-background-color nil)
 '(coverlay:untested-line-background-color "lavenderblush")
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(cua-mode t)
 '(fill-column 79)
 '(flymake-error-bitmap '(flymake-big-indicator compilation-error))
 '(flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
 '(fringe-mode '(24 . 0) nil (fringe))
 '(indent-tabs-mode nil)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-enabled-frameworks nil)
 '(js-indent-level 4)
 '(lisp-indent-function 'common-lisp-indent-function)
 '(lsp-clients-typescript-init-opts '(:generateReturnInDocTemplate t))
 '(lsp-clients-typescript-preferences '(:generateReturnInDocTemplate t))
 '(lsp-enable-snippet nil)
 '(menu-bar-mode nil)
 '(mmm-submode-decoration-level 0)
 '(projectile-completion-system 'auto)
 '(read-process-output-max (* 1024 1024 5) t)
 '(selectrum-mode t)
 '(tool-bar-mode nil)
 '(tsi-typescript-indent-offset 4)
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
 '(company-scrollbar-bg ((t (:background "gray95"))) t)
 '(company-scrollbar-fg ((t (:background "#808080"))) t)
 '(company-tooltip ((t (:background "white smoke" :foreground "black"))))
 '(company-tooltip-annotation ((t (:foreground "gray45"))))
 '(company-tooltip-common ((t (:foreground "gray14" :weight normal))))
 '(company-tooltip-scrollbar-thumb ((t (:background "gray50"))))
 '(company-tooltip-scrollbar-track ((t (:inherit company-tooltip-common))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(dap-ui-breakpoint-verified-fringe ((t (:background "black" :weight bold))))
 '(dap-ui-pending-breakpoint-face ((t (:background "pale goldenrod"))))
 '(fill-column-indicator ((t (:foreground "#d0d0d0"))))
 '(flymake-warning ((t (:underline (:color "dark orange" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "#647892"))))
 '(header-line ((t (:inherit mode-line :background "grey97" :foreground "grey20" :box nil))))
 '(highlight ((t (:background "light steel blue"))))
 '(hl-line ((t (:inherit highlight))))
 '(js2-error ((t nil)))
 '(js2-external-variable ((t nil)))
 '(js2-function-call ((t nil)))
 '(js2-function-param ((t nil)))
 '(js2-instance-member ((t nil)))
 '(js2-jsdoc-html-tag-delimiter ((t nil)))
 '(js2-jsdoc-html-tag-name ((t nil)))
 '(js2-jsdoc-tag ((t nil)))
 '(js2-jsdoc-type ((t nil)))
 '(js2-jsdoc-value ((t nil)))
 '(js2-object-property ((t nil)))
 '(js2-object-property-access ((t nil)))
 '(js2-private-function-call ((t nil)))
 '(js2-private-member ((t nil)))
 '(js2-warning ((t nil)))
 '(line-number ((t (:background "#ffffff" :foreground "#d0d0d0"))))
 '(lsp-headerline-breadcrumb-path-hint-face ((t nil)))
 '(lsp-headerline-breadcrumb-path-info-face ((t nil)))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face))))
 '(lsp-headerline-breadcrumb-symbols-info-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face))))
 '(markdown-code-face ((t (:inherit default :extend t :background "white" :foreground "#222222"))))
 '(markdown-header-face ((t (:background "#ffffff" :foreground "#444444"))))
 '(markdown-inline-code-face ((t (:inherit (markdown-code-face font-lock-constant-face) :background "cornsilk"))))
 '(markdown-pre-face ((t (:inherit (markdown-code-face font-lock-constant-face) :background "cornsilk"))))
 '(mmm-code-submode-face ((t (:background "LightGray"))))
 '(mmm-default-submode-face ((t nil)))
 '(mmm-delimiter-face ((t nil)) t)
 '(mode-line ((t (:background "steel blue" :foreground "white" :weight normal))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-inactive ((t (:background "grey75" :foreground "white"))))
 '(selectrum-current-candidate ((t (:inherit highlight)))))

(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (my/delete-word (- arg)))

(defun my/configure-global-keyboard-shortcuts ()
  ;; Ctrl-Backspace -> delete a word instead of killing it
  (global-set-key [C-backspace] 'my/backward-delete-word)
  ;; Ctrl-Delete -> delete a word instead of killing it
  (global-set-key [C-delete] 'my/delete-word)
  ;; Ctrl-PgDn -> next window
  (global-set-key [C-next] 'other-window)
  ;; Ctrl-PgUp -> previous window
  (global-set-key [C-prior]
                  (lambda ()
                    (interactive)
                    (other-window -1)))
  ;; Ctrl-a -> select entire buffer
  (global-set-key (kbd "C-a") 'mark-whole-buffer)
  ;; ;; M-x -> counsel extended-command finder
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; ;; Ctrl-x Ctrl-f -> counsel file finder
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; ;; Ctrl-x b -> counsel buffer switcher
  (global-set-key (kbd "C-x b") 'consult-buffer)
  ;; ;; Ctrl-s -> swiper buffer-search
  ;; (global-set-key (kbd "C-s") 'swiper)
  ;; ;; Ctrl-h f -> counsel function-selection
  ;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
  ;; ;; Ctrl-h v -> counsel variable-selection
  ;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  ;; Ctrl-; -> comment/uncomment region
  (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
  ;; C-x 9 -> switch between a horizontal and vertical window split (if 2 windows visible)
  (global-set-key (kbd "C-x 9") 'my/rotate-window-split)

  ;; see also the various `with-eval-after-load' calls for more shortcut assignments

  ;; turn off some other things
  (define-key global-map (kbd "C-/") nil)
  (define-key global-map (kbd "C-x C-k RET") nil)
  ;; `suspend-frame` completely hangs the emacs process which seems cool and good
  (define-key global-map (kbd "C-x C-z") nil))

(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my/update-packages ()
  (interactive)
  (straight-pull-all)
  (straight-rebuild-all))

;; something involving company-mode and lsp attempts to invoke this function
(defun yas-expand-snippet (&rest ignored))

(provide 'init.el)
;;; init.el ends here
