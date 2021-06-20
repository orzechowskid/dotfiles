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


(my/configure-frames-and-fonts)
