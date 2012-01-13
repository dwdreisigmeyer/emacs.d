(require 'initial-setup)
(require 'anything-traverse)
; Need to replace the following in anything-c-yasnippet.el:
;	yas/snippets/table-hash -> yas/table-hash
;	yas/snippets/table-templates -> yas/table-templates
(require 'anything-c-yasnippet)


(global-set-key (kbd "C-x b")
  (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :sources
     '( anything-c-source-buffers))))
     	
(global-set-key (kbd "C-x f")
  (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :sources
     '( anything-c-source-recentf
        anything-c-source-find-files
        anything-c-source-write-file
        anything-c-source-insert-file
        anything-c-source-copy-files)))) 
               
     	
(global-set-key (kbd "M-? c")
  (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :sources
     '( anything-c-source-emacs-functions
     	anything-c-source-extended-command-history))))
             
(global-set-key (kbd "M-? m")
  (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :sources
     '( anything-c-source-bookmarks
     	anything-c-source-bookmark-set
     	anything-c-source-etags-select
     	anything-c-source-traverse-occur))))
     
(global-set-key (kbd "M-? y")
  (lambda() (interactive)
    (anything
     :prompt "Switch to: "
     :sources
     '( anything-c-source-yasnippet))))
                        
(provide 'anything-sources)