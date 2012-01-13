;;
;; Put this in your .emacs:
;; 		(require 'my-yas-funs)
;;
;; To use in a given major-mode, e.g., js-mode, use:
;;		(add-hook 'js-mode-hook (lambda () (yas/minor-mode-on)))
;;		(add-hook 'js-mode-hook (lambda () (add-to-list 'ac-sources `ac-new-yas-source)))
;;
;; Works best with the following:
;;		(define-key ac-complete-mode-map "\t" 'ac-complete)
;;		(define-key ac-complete-mode-map "\r" nil)
;; Then "\r" will be the yas trigger after "\t" is used to select an ac candidate.
;; The yasnippet candidates are marked with a "y" in the candidate list.

(require 'yasnippet)

(defvar yas-candidates nil)

(defun init-yas-candidates ()
	(let ((table (yas/get-snippet-tables major-mode)))
		(if table
		  (let (candidates (list))
				(mapcar (lambda (mode)          
				  (maphash (lambda (key value)    
					(push key candidates))          
				  (yas/table-hash mode))) 
				table)				
			(setq yas-candidates candidates)))))
			
			
(defvar ac-new-yas-source
	'(	(init . init-yas-candidates)
		(candidates . yas-candidates)
		(action . yas/expand)
		(symbol . "y")))

(provide 'my-yas-funs)