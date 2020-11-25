;;; .emacs --- Summary:
;;; Commentary:
;;; Code:


(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(require 'package)


;;; mode hooks, utility functions, and advices


(defun my/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."

  (interactive "p")
  (my/delete-word (- arg)))


(defun my/close-buffer ()
  "Closes current buffer without prompting."

  (interactive)
  (kill-buffer (current-buffer)))


(defun my/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."

  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))


(defun my/eslint-fix-buffer ()
  "Run `eslint --fix' on current buffer."

  (interactive)
  (if (executable-find "eslint_d")
      (progn
        (call-process "eslint_d" nil "*ESLint Errors*" nil "--fix" buffer-file-name)
        (revert-buffer t t t))
    (message "eslint_d not found")))


(defun my/find-file-regex (pattern)
  "Better matching for `counsel-find-file'.  Only search for PATTERN at the start of file basenames."

  (concat "^" pattern))


(defun my/ignore-file-p (name path)
  "Tell projectile to ignore some stuff based on the NAME and PATH of a file in the project."

  (or (string= name "node_modules")
      (string= name ".git")
      (string= name ".circleci")
      (string= name "coverage")))


(defun my/noop ()
  "Does nothing."

  (interactive))


(defun my/status-line-render (left-content right-content)
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


(defun my/term-send-cx ()
  "Sends Ctrl-x to a term buffer.  Useful if you accidentally open nano."
  (interactive)
  (term-send-raw-string ""))


(defun my/css-mode-hook ()
  "Do some things when opening [S]CSS files."
  (company-mode t)
  (eldoc-mode t)
  (flymake-stylelint-enable)
  (subword-mode t))


(defun my/flymake-mode-hook ()
  "Do some things when enabling Flymake."
  (help-at-pt-set-timer)
  (setq-local help-at-pt-display-when-idle t))


(defun my/javascript-mode-hook ()
  "Do some things when opening JavaScript files."
;;  ;; run `eslint --fix' upon save
;;  (add-hook 'after-save-hook 'my/eslint-fix-buffer t t)

  ;; assume JSX even if you don't see an import/require of React
  (js-jsx-enable)

  ;; treat this file as part of a larger project (when applicable)
  (projectile-mode)

  ;; code analysis via a language server
  (lsp)

  ;; add an eslint backend to flymake
  (flymake-eslint-enable)
  (if (buffer-file-name)
      (setq flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js")))

  ;; code-completion
  (company-mode t)
  (company-quickhelp-mode t)
  (lsp-completion-mode t)

  ;; camelCase-aware navigation
  (subword-mode t)

  ;; turn on coverage mode if not a test file
  (if (buffer-file-name)
      (unless (string-match-p "test.js[x]?\\'" (buffer-file-name))
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
              "coverage/lcov.info"))))))

  ;; enable multiple major modes (for css-in-js etc)
;  (mmm-mode t)
;  (mmm-add-mode-ext-class 'js-mode nil 'mmm-styled-mode)

  ;; I want the key command in the xref map instead of this one
  (define-key js-mode-map (kbd "M-.") nil))


(defun my/json-mode-hook ()
  "Do some things when opening JSON files."

  (make-local-variable 'js-indent-level)

  (lsp-mode -1)
  (lsp-completion-mode -1)

  ;; enable camelCase-aware navigation
  (subword-mode t))


(defun my/js-json-mode-hook ()
  "Emacs' json major mode descends from its js major mode, so the hook situation is all messed up without something like this."
  (if (string-match-p "json\\'" (or (buffer-file-name) ""))
      (my/json-mode-hook)
    (my/javascript-mode-hook)))


(defun my/common-lisp-mode-hook ()
  "Do some things when entering a Lisp mode."

  ;; enable code-completion mode
  (company-mode t)
  (company-quickhelp-mode t)

  ;; enable documentation in echo area
  (eldoc-mode t)

  ;; linter (most of the time)
  (when (not (string= (buffer-name) "*scratch*"))
    (flymake-mode t)))


(defun my/terminal-mode-hook ()
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
                 (my/status-line-render
                  (format "%%b")
                  (format " "))))))


(defun my/ts-mode-hook ()
  "Do some things when editing a Typescript file."

  ;; treat this file as part of a larger project (when applicable)
  (projectile-mode)

  ;; code analysis via a language server
;  (lsp)
;  (lsp-completion-mode t)

  ;; add an eslint backend to flymake
;;  (flymake-eslint-enable)
;;  (setq flymake-eslint-project-root (locate-dominating-file buffer-file-name ".eslintrc.js"))

  ;; code-completion
  (company-mode t)
  (company-quickhelp-mode t)

  ;; camelCase-aware navigation
  (subword-mode t))


(defun my/minibuf-entrance-hook ()
  "Do some things when activating the minibuffer."

  ;; stolen from Doom
  (setq gc-cons-threshold most-positive-fixnum))


(defun my/minibuf-exit-hook ()
  "Do some things when leaving the minibuffer."

  ;; stolen from Doom
  (run-at-time
   1
   nil
   (lambda ()
     (setq gc-cons-threshold 16777216))))


;;; initialization functions


(defun init/config-file-associations ()
  "Configure what happens when a certain file type is opened."

  (push '("\\.js[x]?\\'" . js-mode) auto-mode-alist)
  (push '("\\.json\\'" . json-mode) auto-mode-alist)
  (push '("\\.[s]?css\\'" . scss-mode) auto-mode-alist)
  (push '("\\.less\\'" . scss-mode) auto-mode-alist)
  (push '("\\.md\\'" . markdown-mode) auto-mode-alist)
  (push '("\\.y[a]?ml\\'" . yaml-mode) auto-mode-alist)
  (push '("\\.ts[x]?\\'" . typescript-mode) auto-mode-alist)

  ;; run custom functions when some major modes are entered
  (add-hook 'scss-mode-hook 'my/css-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'my/common-lisp-mode-hook)
  (add-hook 'js-mode-hook 'my/js-json-mode-hook)
  (add-hook 'json-mode-hook 'my/js-json-mode-hook)
  (add-hook 'lisp-mode-hook 'my/common-lisp-mode-hook)
  (add-hook 'term-mode-hook 'my/terminal-mode-hook)
  (add-hook 'flymake-mode-hook 'my/flymake-mode-hook)
;  (add-hook 'typescript-mode-hook 'my/ts-mode-hook)
  (add-hook 'minibuffer-setup-hook 'my/minibuf-entrance-hook)
  (add-hook 'minibuffer-exit-hook 'my/minibuf-exit-hook))


(defun init/config-fonts ()
  "Configure font, face, and frame preferences."

  ;; set default and fallback fonts
  ;; highest-priority font first
  (let ((my-fonts '("Source Code Pro Light 12"
                    "Symbola 12")))
    (create-fontset-from-fontset-spec standard-fontset-spec)
    (dolist (font (reverse my-fonts))
      (set-fontset-font "fontset-standard" 'unicode font nil 'prepend)))

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
                 '(font . "fontset-standard")))

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
    16 16 'center))


(defun init/config-keyboard-shortcuts ()
  "Config some personal-preference keyboard shortcuts."

  ;; global shortcuts

  ;; Ctrl-Backspace -> delete a word instead of killing it
  (global-set-key [C-backspace] 'my/backward-delete-word)
  ;; Ctrl-Delete -> forward-delete a word instead of killing it
  (global-set-key [C-delete] 'my/delete-word)
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
  ;; I don't use these and their minibuffer messages annoy me
  (global-set-key (kbd "C-b") 'my/noop)
  (global-set-key (kbd "C-f") 'my/noop)
  (global-set-key (kbd "C-n") 'my/noop)
  (global-set-key (kbd "C-p") 'my/noop)
  (global-set-key (kbd "C-x C-k") 'my/noop)
  ;; use a richer file-finder
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; Ctrl-w -> close buffer.  CUA-ish
  (global-set-key (kbd "C-w") 'my/close-buffer)

  ;; mode-specific shortcuts

  ;; use useful Flycheck key bindings in Flymake (when flymake-mode is enabled)
  (define-key flymake-mode-map (kbd "C-c ! n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") 'flymake-goto-prev-error)
  ;; keyboard shortcut to force auto-completion (when company-mode is enabled)
  (define-key company-mode-map (kbd "M-/") 'company-complete)
  ;; keyboard shortcut to find a file in the current project, like VSCode does (when projectile-mode is enabled
  (define-key projectile-mode-map (kbd "C-p") 'projectile-find-file))


(defun init/config-modeline ()
  "Config some mode- and header-line stuff."

  ;; shorten some major-mode modeline strings
  (delight '((emacs-lisp-mode "ELisp")))

  ;; remove some minor-mode modeline strings
  (delight
   '((subword-mode nil "subword")
     (company-mode nil "company")
     (lsp-mode nil "lsp")
     (mmm-mode nil nil)
     (projectile-mode nil t)
     (coverlay-minor-mode nil "coverlay")
     (eldoc-mode nil "eldoc")))

  ;; shorten dynamically-generated Flymake minor-mode string
  (advice-add
   'flymake--mode-line-format :filter-return
   (lambda (&rest return-value)
     (setf
      (seq-elt
       (car return-value)
       0)
      " ✓")
     return-value))

  (advice-add
   'yank
   :after
   (lambda (&rest unused)
     (indent-region (region-beginning) (region-end) nil))))


(defun init/config-packages ()
  "Configure the loading of external packages."

  (package-initialize)

  ;; enable package-loading from MELPA
  (push '("melpa" . "https://melpa.org/packages/") package-archives)

  ;; enable package-loading from a local directory
  (push "~/.emacs.d/elisp" load-path)

  ;; these things are used by multiple major modes and/or get configured before any
  ;; buffers are opened, so they're not good candidates for autoload

  ;; add icons to counsel
  (require 'all-the-icons-ivy)
  ;; code completion
  (require 'company)
  (require 'company-quickhelp)
  ;; a better file-finder
  (require 'counsel)
  ;; mode line cleaner-upper
  (require 'delight)
  ;; support nvm doing weird things to our shell $PATH but not our login session $PATH
  (require 'exec-path-from-shell)
  ;; linting
  (require 'flymake)
  (require 'flymake-stylelint) ;; eyyyy don't forget this is not on MELPA yet and needs to be installed manually
  ;; code analysis
  (require 'lsp-mode)
  (require 'lsp-javascript)
  ;; multiple modes in the same buffer
  (require 'mmm-mode)
  ;; project-centric editing
  (require 'projectile)
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
  (autoload 'yaml-mode "yaml-mode"
    "Use the yaml-mode package to provide 'yaml-mode on-demand."
    t))


(defun init/config-whatever-else ()
  "Kind of a dumping ground tbh."

  ;; use a sane regex for filename matching
  (push '(counsel-find-file . my/find-file-regex) ivy-re-builders-alist)
  ;; prefer contents of help-at-point (e.g., Flymake) over whatever eldoc outputs
  (advice-add 'eldoc-message :around
              (lambda (oldfn doc-msg)
                (let ((echo-help-string (help-at-pt-string)))
                  (if echo-help-string
                      (display-local-help)
                    (funcall oldfn doc-msg))))))


;;; variables and faces set via Customize


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(blink-cursor-mode nil)
 '(company-backends '(company-files company-capf company-semantic))
 '(company-files-exclusions '("~" "*#"))
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-color-background nil)
 '(company-quickhelp-color-foreground nil)
 '(company-quickhelp-delay 1.0)
 '(company-tooltip-align-annotations t)
 '(coverlay:mark-tested-lines nil)
 '(coverlay:untested-line-background-color "#ffe8e8")
 '(css-indent-offset 2)
 '(cua-mode t nil (cua-base))
 '(debug-on-error t)
 '(exec-path-from-shell-check-startup-files nil)
 '(flymake-error-bitmap '(flymake-big-indicator compilation-error))
 '(flymake-eslint-executable-args "\"-f unix\"")
 '(flymake-eslint-executable-name "eslint")
 '(flymake-jslint-args '("--no-color" "--no-ignore" "--stdin"))
 '(flymake-jslint-command "eslint")
 '(flymake-no-changes-timeout 0.5)
 '(flymake-note-bitmap '(flymake-big-indicator compilation-info))
 '(flymake-stylelint-executable-args "-q")
 '(flymake-stylelint-executable-name "stylelint")
 '(flymake-warning-bitmap '(flymake-big-indicator compilation-warning))
 '(font-use-system-font nil)
 '(frame-title-format '("%f") t)
 '(fringe-mode '(24 . 8) nil (fringe))
 '(gc-cons-percentage 0.1)
 '(gc-cons-threshold 16777216)
 '(header-line-format
   '((:eval
      (my/status-line-render
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
 '(ivy-magic-tilde nil)
 '(js-chain-indent nil)
 '(js-enabled-frameworks nil)
 '(js-indent-level 2)
 '(js-jsx-attribute-offset 2)
 '(js-switch-indent-offset 2)
 '(lsp-auto-configure t)
 '(lsp-diagnostics-provider :flymake)
 '(lsp-enable-folding nil)
 '(lsp-enable-snippet nil)
 '(lsp-modeline-code-actions-enable nil)
 '(menu-bar-mode nil)
 '(mmm-submode-decoration-level 2)
 '(mode-line-format
   '((:eval
      (my/status-line-render
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
   '(all-the-icons-ivy-rich all-the-icons-ivy counsel all-the-icons company-prescient mmm-mode typescript-mode auto-dim-other-buffers lsp-mode swiper flymake-json editorconfig dotenv-mode web-mode js-doc projectile treemacs-projectile treemacs exec-path-from-shell 2048-game dockerfile-mode request flymake-stylelint company scss-mode markdown-mode json-mode flymake-eslint delight coverlay company-quickhelp))
 '(projectile-completion-system 'ivy)
 '(read-process-output-max (* 1024 1024) t)
 '(scroll-bar-mode nil)
 '(sgml-basic-offset 2)
 '(tool-bar-mode nil)
 '(treemacs-is-never-other-window t)
 '(treemacs-space-between-root-nodes nil)
 '(typescript-enabled-frameworks '(typescript mochikit exttypescript))
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(widget-image-enable nil)
 '(yas-expand-snippet noop))


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
 '(highlight ((t (:background "light steel blue"))))
 '(mode-line ((t (:background "steel blue" :foreground "white" :weight bold))))
 '(mode-line-highlight ((t (:inherit mode-line))))
 '(mode-line-inactive ((t (:inherit default :foreground "gray33" :weight bold))))
 '(region ((t (:extend t :background "gray90" :distant-foreground "gtk_selection_fg_color"))))
 '(term ((t (:background "black" :foreground "white"))))
 '(term-bold ((t (:background "black" :foreground "white" :weight bold))))
 '(term-color-blue ((t (:background "DeepSkyBlue4" :foreground "DeepSkyBlue4"))))
 '(treemacs-directory-face ((t (:inherit font-lock-function-name-face :weight normal))))
 '(treemacs-git-ignored-face ((t (:inherit font-lock-comment-face))))
 '(treemacs-git-modified-face ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(treemacs-git-untracked-face ((t (:inherit default :foreground "forest green" :weight bold))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :underline t :weight bold)))))


;;; let's go!


(add-hook
 'after-init-hook
 (lambda ()
   (init/config-packages)
   (init/config-fonts)
   (init/config-file-associations)
   (init/config-keyboard-shortcuts)
   (init/config-modeline)
   (init/config-whatever-else)
   (all-the-icons-ivy-setup)
   (auto-compression-mode -1)
   (auto-encryption-mode -1)
   (blink-cursor-mode -1)
   (file-name-shadow-mode -1)
   (global-auto-composition-mode -1)
   (package-refresh-contents t)
   ;; nvm/node.js
   (exec-path-from-shell-initialize)
   ;; keep me last
   (message "startup time: %s" (emacs-init-time))))


(provide 'emacs)
;;; .emacs ends here
