; csslint, jslint, jshint and uglifyjs installed with npm.
; I needed to ln -s in /usr/local/bin from
;	/usr/local/Cellar/node/0.6.6/lib/node_modules/csslint/cli.js
;	/usr/local/Cellar/node/0.6.6/lib/node_modules/jshint/bin/hint
;	/usr/local/Cellar/node/0.6.6/lib/node_modules/jslint/bin/jslint.js
;	/usr/local/Cellar/node/0.6.6/lib/node_modules/uglify-js/bin/uglifyjs
; Also need to make sure that
;	(add-to-list 'exec-path "/usr/local/bin")
(require 'flymake-csslint)
(require 'flymake-cursor)
(require 'flymake-jshint)
(require 'js-beautify)
(require 'my-yas-funs)

(defun css-hooks ()
	(autopair-mode 1)
	(hs-minor-mode 1)
	(add-to-list 'ac-modes 'css-mode)
	(auto-complete-mode 1)
	(yas/minor-mode-on)
	(add-to-list 'ac-sources 'ac-new-yas-source))

(defun erlang-hooks ()
	(imenu-add-menubar-index)
	(hs-minor-mode 1)
	(autopair-mode 1)
	(add-to-list 'ac-modes 'erlang-mode)
	(yas/minor-mode-on)
	(add-to-list 'ac-sources 'ac-new-yas-source))

(defun html-hooks ()
	(imenu-add-menubar-index)
	(autopair-mode 1)
	(zencoding-mode 1)
	(hs-minor-mode 1)
	(add-to-list 'ac-modes 'html-mode)
	(auto-complete-mode 1)
	(yas/minor-mode-on)
	(add-to-list 'ac-sources 'ac-new-yas-source))
			
; Installed uglify-js with npm.
(defun uglify-js ()
  	(interactive)
  	(save-buffer)
  	(shell-command (format "uglifyjs  -o %s.min.js %s" 
  		(file-name-sans-extension buffer-file-name)
  		(buffer-file-name))))	
(defun beautify-js ()
  	(interactive)
  	(save-buffer)
  	(shell-command (format "uglifyjs  -b --overwrite %s" (buffer-file-name)))
  	(revert-buffer  1 1 1))
(defun js-hooks ()
	(local-set-key "\C-jj" 	'js-send-last-sexp)
	(local-set-key "\C-js" 	'js-send-buffer)
	(local-set-key "\C-jl" 	'js-load-file)
	(local-set-key "\C-ju" 	'uglify-js)
	(local-set-key "\C-jb" 	'beautify-js) ; with uglifyjs
	(local-set-key "\C-jt" 	'js-beautify)
	(imenu-add-menubar-index)
	(autopair-mode 1)
	(hs-minor-mode 1)
	(yas/minor-mode-on)
	(add-to-list 'ac-sources 'ac-new-yas-source))
	
(defun lisp-hooks ()
	(imenu-add-menubar-index)
	(hs-minor-mode 1)
	(paredit-mode 1))

(defun php-hooks ()
	(imenu-add-menubar-index)
	(autopair-mode 1)
	(hs-minor-mode 1)
	(add-to-list 'ac-modes 'php-mode)
	(auto-complete-mode 1)
	(yas/minor-mode-on)
	(add-to-list 'ac-sources 'ac-new-yas-source)
	(flymake-mode 1))

;;;--------------------------

(defun setup-css ()
	(add-hook 'css-mode-hook 
		(lambda () 
			(css-hooks))))
			
(defun setup-erlang ()
	(add-hook 'erlang-mode-hook 
		(lambda ()
			(erlang-hooks)))
	(require 'erlang-flymake)
	(add-hook 'erlang-shell-mode-hook 
		(lambda () 
			(autopair-mode 1))))
			
(defun setup-html ()
	(add-hook 'html-mode-hook 
		(lambda ()
			(html-hooks))))
			
(defun setup-javascript ()
	(add-hook 'js-mode-hook 
		(lambda ()
			(js-hooks)))
	; Have a Javascript REPL
	(add-hook 'inferior-js-mode-hook
	  (lambda ()
		(autopair-mode 1)
		;; We like nice colors
		(ansi-color-for-comint-mode-on)
		;; Deal with some prompt nonsense
		(add-to-list 'comint-preoutput-filter-functions
					 (lambda (output)
					   (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
					   (replace-regexp-in-string ".*1G.*3G" "node > " output)))))))

(defun setup-lisp ()
	(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
	(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 1)))
	(add-hook 'inferior-lisp-mode-hook (lambda () (paredit-mode 1)))
	(add-hook 'lisp-mode-hook 
		(lambda ()
			(lisp-hooks))))
			
(defun setup-php ()
	(add-hook 'php-mode-hook 
		(lambda () 
			(php-hooks))))

;;;--------------------------

(defun setup-languages ()
	(setup-css)
	(setup-erlang)
	(setup-html)
	(setup-javascript)
	(setup-lisp)
	(setup-php))
	
(provide 'language-hooks)