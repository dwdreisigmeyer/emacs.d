;;;		.emacs
;;; 	4 JAN 2012

; I put these here so they don't appear and then disappear.
(scroll-bar-mode 0)
(tool-bar-mode 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATIONS
(add-to-list 'load-path "~/.emacs.d/initializations")
; These need to be loaded and run in this order
(require 'initial-setup)
(initial-setup)
(require 'language-hooks)
(setup-languages)
(require 'opening-closing-procedures)
(do-when-opening)