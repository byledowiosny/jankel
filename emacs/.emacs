
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (distinguished)))
 '(custom-safe-themes
   (quote
    ("b3f148ee61ffa98a390f785f049450f6ab877cbe7d3e078bc41920d345b2bb90" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" default)))
 '(package-selected-packages
   (quote
    (auctex-lua auctex-latexmk lua-mode slime pkg-info ess distinguished-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'ess-site)
(autoload 'forth-mode "~/.gforth.el")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
			    auto-mode-alist))
(autoload 'forth-block-mode "~/.gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
			    auto-mode-alist))
(add-hook 'forth-mode-hook (function (lambda ()
				       ;; customize variables here:
				       (setq forth-indent-level 4)
				       (setq forth-minor-indent-level 2)
				       (setq forth-hilight-level 3)
				       ;;; ...
				       )))
(setq inferior-lisp-program "/usr/local/bin/sbcl")
(require 'auctex-latexmk)
(auctex-latexmk-setup)
