(require 'initial-setup)
(require 'anything-traverse)
; Need to replace the following in anything-c-yasnippet.el:
;	yas/snippets/table-hash -> yas/table-hash
;	yas/snippets/table-templates -> yas/table-templates
(require 'anything-c-yasnippet)
	
;-------------------------------------------------------------------------------------------
; These are from:
;	http://www.emacswiki.org/emacs/AnythingSources
;-------------------------------------------------------------------------------------------
	
(defun load-anything-sources ()
	(interactive)
	(setq anything-sources
		'(	anything-c-source-bookmarks ; install bookmarks+ from marmalade
			anything-c-source-bookmark-set
			anything-c-source-browse-code
			anything-c-source-emacs-functions
			anything-c-source-etags-select
			anything-c-source-extended-command-history
			anything-c-source-files-in-current-dir+
			anything-c-source-imenu
			anything-c-source-traverse-occur
			anything-c-source-yasnippet)))

                            
(provide 'anything-sources)