(require 'comint)
(require 'doc-view)
(require 'yas-and-the-rest)

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
	 '(	el-get
		ac-slime
		anything
		auto-complete
		autopair
		bookmark+ ; my rcp : need to compile by hand
		color-theme
		dired+
		distel ; my rcp
		flymake-csslint ; my rcp
		flymake-cursor ; my rcp
		flymake-html-validator ; my rcp
		js-comint
		lusty-explorer
		paredit
		php-mode ; this is my rcp - using https://github.com/ejmr/php-mode
		; Comment out:
		;	(wg-mode-line-add-display)
		;	(wg-mode-line-remove-display)
		; at the bottom of workgroups.el.  Recompile.
		workgroups
		yasnippet
		zencoding-mode))
	(el-get 'sync my:el-get-packages))
	
(defun setup-elpa ()
	(add-to-list 'load-path "~/.emacs.d/elpa")
	(add-subdirs-to-load-path "~/.emacs.d/elpa/")
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/")))
	
(defun setup-installed-packages ()
	(require 'anything-sources)
	(require 'anything-config)
	(require 'bookmark+)
	(setq bmkp-last-as-first-bookmark-file nil) ; This may be overwritten in .emacs -- check it.
	(workgroups-mode 1)
	;(load-anything-sources)
	)

(defun setup-global-keys ()
	; See anything-sources.el for other global keys
	(global-set-key (kbd "C-x C-c") 	'do-before-closing)
	(global-set-key (kbd "M-+")       	'hs-toggle-hiding)
	(global-set-key (kbd "M-=")       	'hs-show-all)
	(global-set-key (kbd "M--")       	'hs-hide-all)
	(global-set-key (kbd "C-x C-b") 	'ibuffer)
	(global-set-key (kbd "C-x C-f") 	'lusty-file-explorer)
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
	(delete-selection-mode 1)
	(setq max-mini-window-height 1))

(defun setup-paths ()
	(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")
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
							"/bin:" "/usr/sbin:" "/sbin:" "/usr/local/bin:" "/usr/X11/bin:"))
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
	(setq not-this-window t))
	
	
;;;--------------------------------
	
	
(defun initial-setup ()
	(setup-comint)
	(setup-odds-and-ends)
	(setup-windows)
	(setup-paths) ; ** This must come before the following function calls. **
	(setup-el-get)
	;(setup-elpa) ; I'm only using el-get
	(require 'layout-and-color)
	; Don't do the next call if emacs will be run in a terminal.
	; Some lines in .emacs also need to be commented out.
	(layout-and-color)
	(setup-installed-packages)
	(setup-global-keys)
	(setup-yas-and-the-rest)
	(setup-slime))

(provide 'initial-setup)