(require 'workgroups)
(require 'my-bmkp-functions)

(defun my-wg-load ()
	(interactive)
	(wg-load (read-file-name "Enter workgroup file : "  my-workgroups-default-directory nil 1)))
	
(defun my-wg-kill-and-load ()
	(interactive)
	(wg-kill-workgroup-and-buffers (wg-current-workgroup))
	(wg-load (read-file-name "Enter workgroup file : "  my-workgroups-default-directory nil 1))
	(if (y-or-n-p (format "Load a new bookmark file? "))
		(my-bmkp-load)))
	
(provide 'my-wg-functions)