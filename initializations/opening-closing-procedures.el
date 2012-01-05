(require 'initial-setup)
(require 'my-wg-functions)
(require 'my-bmkp-functions)


(defun do-before-closing ()
	(interactive)
	(wg-query-for-save)
	(if (y-or-n-p (format "Really exit Emacs? "))
		(save-buffers-kill-emacs)
		(message "Canceled exit")))

(defun do-when-opening ()
	(if (y-or-n-p (format "Load a bookmark file? "))
		(my-bmkp-load))
	(if (y-or-n-p (format "Load a workgroup? "))
		(my-wg-load)))
		
(provide 'opening-closing-procedures)