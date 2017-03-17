(setq-default dotspacemacs-configuration-layers '((ess :variables
	ess-enable-smart-equals t)))

(add-hook 'ess-mode-hook
	(lambda ()
	(ess-toggle-underscore nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default dotspacemacs-configuration-layers '(csv))

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;dotspacemacs-configuration-layers '(
;  (latex :variables latex-build-command "LaTeX"))

;dotspacemacs-configuration-layers '(
;  (latex :variables latex-enable-auto-fill t))

;dotspacemacs-configuration-layers '(
;  (latex :variables latex-enable-folding t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/user-config ()
	(setq inferior-lisp-program "/home/jerzy/bin/ccl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)
