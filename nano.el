
;;(setq debug-on-error t)

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

(cua-mode t)

(mapcar
 'straight-use-package
 '(company
   consult
   magit
   marginalia
   (nano-emacs :type git :protocol ssh :host github :repo "rougier/nano-emacs")
   prescient
   projectile
   selectrum
   selectrum-prescient
   vs-light-theme))

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

;; File menu

(let ((keymap (make-sparse-keymap "File")))
  (define-key
   global-map [menu-bar file]
   (cons "File" keymap))

  (define-key-after
    global-map [menu-bar file find-file]
    '(menu-item "Visit existing/new file..." find-file))
  (define-key-after
    global-map [menu-bar file visit-directory]
    '(menu-item "Visit existing/new directory..." dired))
  (define-key-after keymap [file-sep-1]
    '(menu-item "--"))
  (define-key-after
    global-map [menu-bar file save-buffer]
    '(menu-item "Save file" save-buffer
                :enable (and (current-buffer)
			                 (buffer-modified-p))))
  (define-key-after
    global-map [menu-bar file kill-current-buffer]
    '(menu-item "Close file" kill-current-buffer
                :enable (current-buffer)))
  (define-key-after keymap [file-sep-2]
    '(menu-item "--"))
  (define-key-after
    global-map [menu-bar file nano--delete-frame-or-kill-emacs]
    '(menu-item "Quit" nano--delete-frame-or-kill-emacs)
    (lookup-key global-map [menu-bar file Open\ Recent])))
  ;; recentf minor mode adds an item to this menu

;; Edit menu

;; TODO: figure out why we can't refer to cua- functions inside our menu items
(let ((keymap (make-sparse-keymap)))
  (define-key
   global-map
   [menu-bar edit]
   (cons "Edit" keymap))

  (define-key-after
    global-map
    [menu-bar edit undo]
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
    global-map
    [menu-bar edit cut]
    '(menu-item "Cut current region" kill-region
                :enable (and
                         mark-active
                         (not buffer-read-only))
                :keys "C-x"))
  (define-key-after
    global-map
    [menu-bar edit cut]
    '(menu-item "Copy region" kill-ring-save
                :enable mark-active
                :keys "C-c"))
  (define-key-after
    global-map
    [menu-bar edit paste]
    '(menu-item "Paste" yank
                :enable (and (cdr yank-menu)
                             (not buffer-read-only))
                :keys "C-v"))
  (define-key-after
    keymap [edit-sep-2]
    '(menu-item "--"))
  (define-key-after
    global-map
    [menu-bar edit select-all]
    '(menu-item "Select all" mark-whole-buffer
                :enabled (> (- (point-max) (point-min) 0)))))

(with-eval-after-load 'nano
  ;; HTML codes for the Source Code Pro glyphs to use as fringe indicators
  ;; (requires fringes to be nil)
  (set-display-table-slot standard-display-table 'truncation 8230)
  (set-display-table-slot standard-display-table 'wrap 8601)
  (add-hook
   'prog-mode-hook
   (lambda ()
     (vs-light-theme)
     (linum-mode t)
     (subword-mode t)
     (company-mode t))))

(with-eval-after-load 'selectrum-prescient
  (selectrum-mode t)
  (selectrum-prescient-mode t)
  (prescient-persist-mode t))

(mapcar
 'require
 '(consult
   marginalia
   prescient
   selectrum-prescient))

(require 'nano)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(cua-mode t)
 '(fringe-mode 0 nil (fringe))
 '(marginalia-mode t)
 '(selectrum-mode t)
 '(windmove-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#fafafa" :foreground "#222222" :weight light :height 120 :foundry "ADBO" :family "Source Code Pro"))))
 '(fringe ((t (:background "transparent")))))
