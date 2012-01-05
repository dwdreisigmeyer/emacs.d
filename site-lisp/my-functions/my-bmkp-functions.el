(require 'bookmark+)

(defun my-bmkp-load ()
	(interactive)
	(bmkp-switch-bookmark-file-create (read-file-name "Enter bookmark file : "  
		my-bookmarks-default-directory nil 1)))

(provide 'my-bmkp-functions)