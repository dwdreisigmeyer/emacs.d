(defun setup-ac ()
	(setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
	(define-key ac-complete-mode-map "\t" 'ac-complete)
	(define-key ac-complete-mode-map "\r" nil)
	(add-to-list 'ac-user-dictionary "~/.emacs.d/site-lisp/my-ac-dicts"))

(defun setup-ibuffer ()
	(setq ibuffer-saved-filter-groups
          (quote (("default"
                   ("dired" (mode . dired-mode))
                   ("php" (mode . php-mode))
                   ("html" (mode . html-mode))
                   ("css" (mode . css-mode))
                   ("js" (mode . js-mode))
                   ("erlang" (mode . erlang-mode))))))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(defun setup-ido ()
	(ido-mode 1))

(defun setup-yas ()
	(yas/load-directory "~/.emacs.d/site-lisp/my-yas-snippets")
	(setq yas/prompt-functions '(yas/dropdown-prompt)))

(defun setup-yas-and-the-rest ()
		(setup-ac)
		(setup-ibuffer)
		;(setup-ido)
		(setup-yas))
	
(provide 'yas-and-the-rest)