;;(setq debug-on-error t)

(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; memory management: perform no GC during startup, then raise the limit from the
;; default value to something more suitable for modern machines
(defvar my/gc-cons-threshold (* 1024 1024 20))
(setq gc-cons-threshold most-positive-fixnum)

(add-hook
 'after-init-hook
 (lambda ()
   (setq gc-cons-threshold my/gc-cons-threshold)))

;; a directory for any custom scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package company
  :straight t)
(use-package consult
  :straight t)
(use-package eldoc
  :straight t)
(use-package magit
  :straight t)
(use-package marginalia
  :straight t)
(use-package prescient
  :straight t)
(use-package projectile
  :straight t)
(use-package selectrum
  :straight t)
(use-package selectrum-prescient
  :straight t
  :after (selectrum prescient)
  :config
  (selectrum-mode t)
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))
(use-package vs-light-theme
  :straight t)

;; Ctrl-PgDn -> next window
(global-set-key
 (kbd "C-<next>")
 'other-window)
;; Ctrl-Tab -> next window
(global-set-key
 (kbd "C-<tab>")
 'other-window)
;; Ctrl-PgUp -> previous window
(global-set-key
 (kbd "C-<prior>")
 (lambda ()
   (interactive)
   (other-window -1)))

(recentf-mode t)

(defun domacs/universal-argument ()
  "Internal function.  `universal-argument' with some help text."
  (interactive)
  (message "Enter a numeric value followed by a keystroke or other command...")
  (universal-argument))

(defun domacs/file-menu ()
  "Internal function.  Creates the File menu."
  (let ((keymap (make-sparse-keymap)))
    (define-key-after
      keymap [find-file]
      '(menu-item "Visit existing/new file..." find-file))
    (define-key-after
      keymap [visit-directory]
      '(menu-item "Visit existing/new directory..." dired))
    (define-key-after
      keymap [file-sep-1]
      '(menu-item "--"))
    (define-key-after
      keymap [save-buffer]
      '(menu-item "Save file" save-buffer
                  :enable (and (current-buffer)
			       (buffer-modified-p))))
    (define-key-after
      keymap [kill-current-buffer]
      '(menu-item "Close file" kill-current-buffer
                  :enable (current-buffer)
		  :keys "C-x k"))
    (define-key-after
      keymap [file-sep-2]
      '(menu-item "--"))
    (define-key-after
      keymap [save-buffers-kill-terminal]
      '(menu-item "Quit" save-buffers-kill-terminal))
    keymap))


(defun domacs/edit-menu ()
  "Internal function.  Creates the Edit menu."
  ;; TODO: figure out why we can't refer to cua- functions inside our menu items
  (let ((keymap (make-sparse-keymap)))
    (define-key-after
      keymap [undo]
      '(menu-item "Undo last command" undo
                  :enable (and (not buffer-read-only)
                               (not (eq t buffer-undo-list))
                               (if (eq last-command 'undo)
                                   (listp pending-undo-list)
				 (consp buffer-undo-list)))
                  :keys "C-z"))
    (define-key-after
      keymap [edit-sep-1]
      '(menu-item "--"))
    (define-key-after
      keymap [cut]
      '(menu-item "Cut current region" kill-region
                  :enable (and
                           mark-active
                           (not buffer-read-only))
                  :keys "C-x"))
    (define-key-after
      keymap [cut]
      '(menu-item "Copy region" kill-ring-save
                  :enable mark-active
                  :keys "C-c"))
    (define-key-after
      keymap [paste]
      '(menu-item "Paste" yank
                  :enable (and (cdr yank-menu)
                               (not buffer-read-only))
                  :keys "C-v"))
    (define-key-after
      keymap [edit-sep-2]
      '(menu-item "--"))
    (define-key-after
      keymap [select-all]
      '(menu-item "Select all" mark-whole-buffer
                  :enabled (> (- (point-max) (point-min) 0))))
    keymap))


(defun domacs/actions-menu ()
  "Internal function.  Creates the Actions menu."
  (let ((keymap (make-sparse-keymap)))
    (define-key-after
      keymap
      [universal-argument]
      '(menu-item "Repeat a command..." domacs/universal-argument
		  :keys "C-u"))
    (define-key-after
      keymap [actions-sep-1]
      '(menu-item "--"))
    (define-key-after
      keymap [keyboard-quit]
      '(menu-item "Cancel current command" keyboard-quit))
    keymap))


(defun domacs/help-menu ()
  "Internal function.  Creates the Help menu."
  (let ((keymap (make-sparse-keymap "domacs/help-keymap"))
	(describe-keymap (make-sparse-keymap)))
   
    (define-key-after
      describe-keymap [describe-function]
      '(menu-item "Describe a function..." describe-function))
    (define-key-after
      describe-keymap [describe-variable]
      '(menu-item "Describe a variable..." describe-variable))
    (define-key-after
      describe-keymap [describe-key]
      '(menu-item "Describe a keybinding..." describe-key))
    (define-key-after
      describe-keymap [describe-face]
      '(menu-item "Describe a font-face..." describe-face))
    (define-key-after
      describe-keymap [describe-mode]
      '(menu-item "Describe current buffer modes..." describe-mode))

    (define-key-after
      keymap [emacs-tutorial]
      '(menu-item "Emacs Tutorial" help-with-tutorial-spec-language))
    (define-key-after
      keymap [describe-menu-item]
      (cons "Describe" describe-keymap))
    (define-key-after
      keymap [help-sep-1]
      '(menu-item "--"))
    (define-key-after
      keymap [about-emacs]
      '(menu-item "About emacs..." about-emacs
		  :keys "C-h C-a"))
    (define-key-after
      keymap [about-gnu-project]
      '(menu-item "About GNU..." about-gnu-project
		  :keys "C-h g"))
    keymap))
    

(let ((menu-bar-keymap (make-sparse-keymap)))
  (define-key
   global-map [menu-bar]
   menu-bar-keymap)
  (define-key-after
    menu-bar-keymap [file]
    (cons "File" (domacs/file-menu)))
  (define-key-after
    menu-bar-keymap [edit]
    (cons "Edit" (domacs/edit-menu)))
  (define-key-after
    menu-bar-keymap [actions]
    (cons "Actions" (domacs/actions-menu)))
  (define-key-after
    menu-bar-keymap [help]
    (cons "Help" (domacs/help-menu))))

;; HTML codes for the Source Code Pro glyphs to use as fringe indicators
;; (requires fringes to be nil)
(set-display-table-slot standard-display-table 'truncation 8230)
(set-display-table-slot standard-display-table 'wrap 8601)

(add-hook
 'prog-mode-hook
 (lambda ()
   (linum-mode t)
   (subword-mode t)
   (show-paren-mode t)
   (display-fill-column-indicator-mode t)
   (company-mode t)))

;; (mapcar
;;  'require
;;  '(consult
;;    marginalia
;;    prescient
;;    selectrum-prescient))

(vs-light-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode nil)
 '(auto-encryption-mode nil)
 '(blink-cursor-mode nil)
 '(cua-mode t)
 '(dirtrack-mode nil)
 '(fill-column 99)
 '(fringe-mode 0 nil (fringe))
 '(inhibit-startup-screen t)
 '(marginalia-mode t)
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(selectrum-mode t)
 '(show-paren-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(use-dialog-box nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#fafafa" :foreground "#222222" :weight light :height 120 :foundry "ADBO" :family "Source Code Pro"))))
 '(fringe ((t (:background nil)))))
