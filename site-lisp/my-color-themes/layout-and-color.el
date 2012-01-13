; Modified 

(require 'initial-setup)

; Code start.
(defun layout-and-color ()
  (interactive)
  (global-linum-mode 0)
  (setq-default cursor-type '(bar . 2))
  (blink-cursor-mode 1)
  (show-paren-mode 1)
  (setq inhibit-splash-screen 1)
  (global-hl-line-mode)
  (setq initial-scratch-message nil)
  (setq standard-indent 4)
  (setq resize-mini-windows nil) ; needed to make some of the shell commands to display correctly
  (color-theme-install
   '(layout-and-color
     ((background-color . "#272821")
      (foreground-color . "#F8F8F2")
      (border-color . "black")
      (cursor-color . "#DAD085"))
     (default ((t (nil))))
     (fringe ((t (:background "#272821"))))
     (font-lock-builtin-face ((t (:foreground "#A6E22A"))))
     (font-lock-comment-face ((t (:italic t :foreground "#75715D"))))
     (font-lock-constant-face ((t (:foreground "#A6E22A"))))
     (font-lock-doc-string-face ((t (:foreground "#65B042"))))
     (font-lock-string-face ((t (:foreground "#DFD874"))))
     (font-lock-function-name-face ((t (:foreground "#F1266F" :italic t))))
     (font-lock-keyword-face ((t (:foreground "#66D9EF"))))
     (font-lock-type-face ((t (:underline t :foreground "#89BDFF"))))
     (font-lock-variable-name-face ((t (:foreground "#A6E22A"))))
     (font-lock-warning-face ((t (:bold t :foreground "#FD5FF1"))))
     (highlight-80+ ((t (:background "#D62E00"))))
     (hl-line ((t (:background "#1A1A1A"))))
     (ido-subdir ((t (:foreground "#F1266F"))))
	 ;(mode-line-inactive ((t (:background "black" :foreground "#75715D"))))
     (region ((t (:background "#6DC5F1"))))
     (vertical-border ((t (:foreground "black"))))
    )
  )
)

; Header/Mode line format.  Mainly from here:
;	http://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;(setq-default mode-line-format nil)
(setq-default mode-line-format ;(setq-default header-line-format
	'(	(:propertize "%4l:" face header-line-position-face)
		(:eval (propertize "%3c" 'face
			(if (>= (current-column) 80)
				'header-line-80col-face
				'header-line-position-face)))
		(:eval
			(cond ((buffer-modified-p)
				(propertize " * " 'face 'header-line-modified-face))
				(t "   ")))
		"\t"
		(:propertize "%b" face header-line-filename-face)
		"\t\t"
		(:propertize mode-name face header-line-mode-face)
		"  ("
		(:eval (propertize (format-mode-line minor-mode-alist)
			'face 'header-line-minor-mode-face))
		" )"))
(make-face 'header-line-modified-face)
(make-face 'header-line-filename-face)
(make-face 'header-line-position-face)
(make-face 'header-line-mode-face)
(make-face 'header-line-minor-mode-face)
(make-face 'header-line-80col-face)
(set-face-attribute 'mode-line nil
	:foreground "#75715D" 
    :background "black"
    :box nil)
(set-face-attribute 'mode-line-inactive nil
	:foreground "#75715D" 
    :background "black"
    :box nil)
(set-face-attribute 'header-line-modified-face nil
    :inherit 'header-line-face
    :foreground "white")
(set-face-attribute 'header-line-filename-face nil
    :inherit 'header-line-face
    :foreground "white")
(set-face-attribute 'header-line-position-face nil
    :inherit 'header-line-face
    :family "Menlo" 
    :height 100)
(set-face-attribute 'header-line-mode-face nil
    :inherit 'header-line-face)
(set-face-attribute 'header-line-minor-mode-face nil
    :inherit 'header-line-mode-face
    :height 90)
(set-face-attribute 'header-line-80col-face nil
    :inherit 'header-line-position-face
    :foreground "white")

(provide 'layout-and-color)
