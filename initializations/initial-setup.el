(require 'comint)
(require 'doc-view)

(defvar my-workgroups-default-directory "~/.emacs.d/workgroups/")
(defvar my-bookmarks-default-directory "~/.emacs.d/bookmarks/")

(defun add-subdirs-to-load-path (dir) ; Make it easy to load the files
	  (let ((default-directory (concat dir "/")))
		(normal-top-level-add-subdirs-to-load-path)))

(defun setup-comint ()
	(define-key comint-mode-map (kbd "M-<up>")
	'comint-previous-matching-input-from-input)
	(define-key comint-mode-map (kbd "M-<down>")
		'comint-next-matching-input-from-input)
	(define-key comint-mode-map (kbd "<up>")
		'comint-previous-input)
	(define-key comint-mode-map (kbd "<down>")
		'comint-next-input)
	; Prevents shell-commands from echoing:
	;	http://www.gnu.org/software/emacs/windows/big.html#Sub_002dprocesses
	(defun my-comint-init ()
		(setq comint-process-echoes t))
	(add-hook 'comint-mode-hook 'my-comint-init))

(defun setup-el-get ()
	(unless (require 'el-get nil t)
	  (url-retrieve
	   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
	   (lambda (s)
		 (end-of-buffer)
		 (eval-print-last-sexp))))
	(add-to-list 'el-get-recipe-path "~/.emacs.d/site-lisp/my-recipes")
	(setq my:el-get-packages
	; This doesn't work for bookmark+
	; I need to hand compile the fpllowing:
	;	distel
	;	multi-web-mode (remove eval-after-load line at top first)
	 '(	el-get
		ac-slime
		anything
		auto-complete
		autopair
		buffer-move
		color-theme
		dired+
		distel ; my rcp
		flymake-html-validator ; my rcp
		js-comint
		paredit
		php-mode-improved
		yasnippet ; my rcp
		zencoding-mode))
	(el-get 'sync my:el-get-packages))
	
(defun setup-elpa ()
	; Used this for the following packages:
	;	bookmarks+
	;	flymake-cursor
	;	workgroups 
	; this needs to be recompiled -- first remove the lines:
	;	(wg-mode-line-add-display)
	;	(wg-mode-line-remove-display)
	; at the bottom of the file.
	(add-to-list 'package-archives 
		'("marmalade" . "http://marmalade-repo.org/packages/")))

	
(defun setup-installed-packages ()
	; Comment out:
	;	(wg-mode-line-add-display)
	;	(wg-mode-line-remove-display)
	; at the bottom of workgroups.el.  Recompile.
	(require 'anything-sources)
	(require 'anything-config)
	(require 'bookmark+)
	(require 'layout-and-color)
	(require 'workgroups)
	(setq bmkp-last-as-first-bookmark-file nil) ; This may be overwritten in .emacs -- check it.
	(workgroups-mode 1)
	(layout-and-color)
	(load-anything-sources))

(defun setup-global-keys ()
	(global-set-key (kbd "C-.")			'anything)
	(global-set-key (kbd "C-c C-w")		'backward-kill-word)
	(global-set-key (kbd "C-x C-c") 	'do-before-closing)
	(global-set-key (kbd "M-+")       	'hs-toggle-hiding)
	(global-set-key (kbd "M-=")       	'hs-show-all)
	(global-set-key (kbd "M--")       	'hs-hide-all)
	(global-set-key (kbd "C-`") 		'my-wg-kill-and-load)
	(global-set-key (kbd "RET") 		'newline-and-indent)
	(global-set-key (kbd "C-c s")	 	'slime-selector)
	(global-set-key (kbd "C-<")			'wg-create-workgroup)
	(global-set-key (kbd "C-,")			'wg-switch-to-workgroup)
	(global-set-key (kbd "M-S-<left>")  'windmove-left)
	(global-set-key (kbd "M-S-<right>") 'windmove-right)
	(global-set-key (kbd "M-S-<up>")    'windmove-up)
	(global-set-key (kbd "M-S-<down>")  'windmove-down)
	(global-set-key (kbd "C->") 		'zencoding-expand-line))
	

(defun setup-odds-and-ends ()
	(savehist-mode 1)
	(fset 'yes-or-no-p 'y-or-n-p)
	(add-hook 'text-mode-hook 'turn-on-auto-fill)
	(setq next-line-add-newlines nil)
	(winner-mode 1)
	(setq visible-bell 1)
	(delete-selection-mode 1))

(defun setup-paths ()
	(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")
	(add-subdirs-to-load-path "~/.emacs.d/elpa/")
	(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
	(add-subdirs-to-load-path "~/quicklisp/")
	; With Homebrew you need to do (replace <VER> with, e.g., R14B04):
	;  ln -s /usr/local/Cellar/erlang/<VER>/share/man /usr/local/lib/erlang
	; to get the manuals to work correctly
	(setq erlang-root-dir "/usr/local/lib/erlang")
	(add-to-list 'load-path 
		(car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
	(add-subdirs-to-load-path (car (file-expand-wildcards "/usr/local/lib/erlang/lib/tools-*/emacs")))
	(add-to-list 'exec-path "/usr/local/lib/erlang/bin")
	(add-to-list 'exec-path "/usr/local/bin") ; for csslint, jshint and jslint
	; Look here: http://xahlee.blogspot.com/2009/08/difference-between-emacss-setenv-path.html
	(setenv "PATH" (concat "/usr/local/texlive/2011/bin/x86_64-darwin:" "/usr/texbin:/usr/bin:" 
							"/bin:" "/usr/sbin:" "/sbin:" "/usr/local/bin:" "/usr/X11/bin:" 
							"/usr/texbin"))
	(setq inferior-js-program-command "/usr/local/bin/node")
	(setq inferior-lisp-program "/usr/local/bin/sbcl"))
	
(defun setup-slime ()
	; When quicklisp is installed use the next two lines
	; NOTE: I needed to comment out the 1st line and uncomment the second line.
	; Then I C-u 0 M-x byte-recompile-directory for ~/quicklisp.
	; Otherwise, slime-fancy (and others) couldn't be found.
	; ** DO NOT COMPILE OTHERWISE IT WILL NOT WORK **
	(load (expand-file-name "~/quicklisp/slime-helper.el"))
	(require 'slime)
	(slime-setup '(slime-fancy slime-banner slime-asdf))
	; Stop SLIME's REPL from grabbing DEL,
	(defun override-slime-repl-bindings-with-paredit ()
	   (define-key slime-repl-mode-map
		  (read-kbd-macro paredit-backward-delete-key) nil))
	(add-hook 'slime-repl-mode-hook 
		(lambda () 
			(paredit-mode +1)
			(override-slime-repl-bindings-with-paredit)
			(set-up-slime-ac)
			(add-to-list 'ac-modes 'slime-repl-mode)))
	(add-hook 'slime-mode-hook 'set-up-slime-ac))

(defun setup-windows ()
	(setq pop-up-windows t)
	(setq pop-up-frames nil)
	(setq not-this-window t)
	(defun my-display-buffer-function (buf not-this-window)
		; This is for HTML / CSS / Javascript verifiers where we have one window.
		(cond 
			((string= (buffer-name buf) "*Shell Command Output*")
				(delete-other-windows)
				(split-window-vertically)
				(switch-to-buffer buf)
				; make the new window as small as possible
				(shrink-window-if-larger-than-buffer)
				(other-window -1))))
	(setq special-display-buffer-names '("*Shell Command Output*"))
	(setq special-display-function 'my-display-buffer-function))
	
	
(defun setup-yas-ac-ido-imenu ()
	(setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
	(define-key ac-complete-mode-map "\t" 'ac-complete)
	(define-key ac-complete-mode-map "\r" nil)
	(add-to-list 'ac-user-dictionary "~/.emacs.d/site-lisp/my-ac-dicts")
	(setq yas/trigger-key "TAB")
	(yas/load-directory "~/.emacs.d/site-lisp/my-yas-snippets")
	(setq yas/prompt-functions '(yas/ido-prompt))
	(ido-mode 1))
	
(defun initial-setup ()
	(setup-comint)
	(setup-odds-and-ends)
	(setup-windows)
	(setup-paths) ; ** This must come before the following function calls. **
	(setup-el-get)  
	(setup-elpa)
	(setup-installed-packages)
	(setup-global-keys)
	(setup-yas-ac-ido-imenu)
	(setup-slime))

(provide 'initial-setup)