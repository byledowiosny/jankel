;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs                                                                    ;;
;; Author: Pavel Penev                                                       ;;
;; Licence: MIT                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file describes a basic Emacs setup. Feel free to modify it to
;; your liking.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section I: Generic settings                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following 3 lines disable unnecessary GUI elements, in this case the
;; menu bar, the tool bar and the scroll bar. If you wish, you can comment out
;; the menu-bar and keep it, but eventually I recommend you disable it.

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Sometimes the mini-buffer becomes multi-line, and it can be a bit annoying as
;; you type in it. This makes it stay one line.

(setq resize-mini-windows nil)

;; We don't need the Emacs splash screen. You can keep it on if you're into
;; that sort of thing

(setq inhibit-splash-screen t)

;;; global-linum-mode adds line numbers to all open windows, In my opinion
;;; there is no reason not to have this enabled.

(global-linum-mode)

;;; Emacs comes with a built-in text based browser. Since I use the browse-url
;;; function most often to browse documentation, I've set it to eww, the Emacs
;;; Web Browser. It works well for that purpose. If you would prefer to use a
;;; graphical browser, you can change this line.

(setq browse-url-browser-function 'eww-browse-url)

;;; I prefer to make the default font slightly smaller.

(set-face-attribute 'default nil :height 130)

;; Show matching parentecies globaly.

(show-paren-mode 1)

;; Use UTF-8 by default

(set-language-environment "UTF-8")

;; Don't ring the bell. It saves some annoyance

(setq ring-bell-function 'ignore)

;; If you're one of the heathens who prefers tabs over spaces, you should
;; remove the following line. It makes indentation use spaces.

(setq-default indent-tabs-mode nil)

;;; A simple backup setup. Makes sure I don't foo~ and #.foo files in
;;; directories with files you edit.

(setq
 backup-by-copying t      ; don't clobber symlinks-
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups


;; Set up emacs server. This allows you to run emacs in the background and
;; connect to it with emacs client. It reduces startup time significantly. If
;; the server is not running, it starts it.

(load "server")

(unless (server-running-p)
  (server-start))

;;; ido-mode, or Interactively DO mode, adds lots of improvements when working
;;; with buffers of files. You can read more about it at:
;;; https://www.emacswiki.org/emacs-test/InteractivelyDoThings

(require 'ido)
(ido-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section II: Packages                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elpa, the default package repository for emacs is fairly conservative, so
;; we'll add the melpa and marbaraie repositories

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; You can install packages by typing M-x package-install <package-name>. I
;; recomend you install the following packages: smex, which adds an improved
;; version of M-x. I highly recomend this. You can read more about smex at:
;; https://github.com/nonsequitur/smex/

;; Another ofthen used mode is magit, which is an interface to git, allowing
;; you to manage your repos through emacs. You can read more about it at:
;; http://magit.vc/ It is one of the most useful modes available for emacs

;; The following repo contains org-mode, since I use a features of org-mode not
;; present in the built-in version. Run M-x package-install org-plus-contrib

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section III: Global Key Bindings                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; By default C-x o is bound to 'other window, but I find I use it much more
;; ofther than open-line, which is bound to C-o, so I swap their definitions

(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-x o") 'open-line)

;; M-0..3 are bound to 'digit-argument. To be used with C-u. I don't use them
;; ofthen, so I prefer to rebind them to the window commands, since M-1 is
;; easyer to type than C-x 1. 

(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)

;; Set the enter key to newline-and-indent which inserts a new line and then
;; indents according to the major mode. This is very convenient. 

(global-set-key (kbd "<RET>") 'newline-and-indent)


;; If you have installed smex, you can uncomment the following lines to
;; activate it:

;(require 'smex)

(global-set-key (kbd "M-x") 'smex) 
(global-set-key (kbd "M-X") 'smex-major-mode-commands) 
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section IV: Misc settings                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load solarized theme

(package-initialize)
(load-theme 'solarized-dark t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section V: Common Lisp mode                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following code sets up Emacs to work with Common Lisp. I'll describe
;; what the code does and what you need to do outside of emacs in order to
;; uncomment it safely.

;; This code assumes that you've installed both SBCL and CCL and slime, the
;; Emacs IDE for Common Lisp. The way you do that is by running the following
;; code in your terminal:

;; # ros install sbcl
;; # ros install ccl-bin
;; # ros -Q -e '(ql:quickload :quicklisp-slime-helper)' -q

;; Now you can safely uncomment the following code


;; Load slime, which was installed in your .roswell directory

(load (expand-file-name "~/.roswell/impls/ALL/ALL/quicklisp/slime-helper.el"))

;; This tells slime how to start a lisp repl. We define two implementations:
;; sbcl, and ccl and we tell slime that we want sbcl to be used by default when
;; we start slime with M-x slime. If you want to start it with CCL, you can do
;; C-u ccl M-x slime

(setf slime-lisp-implementations
      `((sbcl    ("ros" "-Q" "-l" "~/.sbclrc" "run"))
        (ccl     ("ros" "-Q" "-l" "~/.ccl-init.lisp" "-L" "ccl-bin" "run"))))

(setf slime-default-lisp 'sbcl)

;; We make sure slime is using utf-8 to communicate with the running lisp image

(setq slime-net-coding-system 'utf-8-unix)

;; The following function will be executed when we enter lisp-mode, which is
;; active in buffers with Common Lisp code:

(defun lisp-hook-fn ()
  (interactive)
  ;; Start slime mode
  (slime-mode)
  ;; Some useful key-bindings
  (local-set-key [tab] 'slime-complete-symbol)
  (local-set-key (kbd "M-q") 'slime-reindent-defun)
  ;; We set the indent function. common-lisp-indent-function will indent our
  ;; code the right way
  (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
  ;; We tell slime to not load failed compiled code
  (setq slime-load-failed-fasl 'never)
  ;; paredit-mode is a mode that helps deal with lisp parenthesies. Some people
  ;; love it, others hate it and prefer alternatives. I personally love it and
  ;; recomend you use it when editing lisp code. You must install it first with
  ;; M-x package-install paredit-mode. If you chose not to, leave the following
  ;; line commented or delete it.
  (paredit-mode +1)
)

;; Finally we tell lisp-mode to run our function on startup

(add-hook 'lisp-mode-hook 'lisp-hook-fn)

;; The Common Lisp Hyperspec is an online HTML version of the Lisp
;; standard. Slime has a built-in function
;; called hyperspec-lookup which will sholl you the page for a symbol in the
;; spec. You're likely to refer to it very ofthen, so it is useful to have a
;; local copy. To install it on a Debian-like system run:
;; # sudo apt-get install hyperspec
;; Then uncomment the following line. If you use hyperspec-lookup on a lisp
;; symbol it will open a browser to your local copy of the spec.

(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section VI: Org mode setup                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tell org where your agenda files are

(setq org-agenda-files (list (expand-file-name "~/org/agenda/")))

;; Some useful keybindings

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Prompt for a note on TODO task getting marked as DONE

(setq org-log-done t)

;; org-bullets makes outlines pretties. Install it with
;; M-x package install org-bullets

(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section VII: markdown-mode setup                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install markdown-mode with M-x package-install markdown-mode

;; longlines-mode has been marked as obsolete, but it is still very useful.

(require 'longlines)

;; make sure that files with .md and .markdown extentions are opened in markdown-mode

(push '("\\.markdown" . markdown-mode) auto-mode-alist)
(push '("\\.md" . markdown-mode) auto-mode-alist)

;; A hook that starts flyspell-mode for spell checking and longlines mode for
;; soft-line wraping.

(add-hook 'markdown-mode-hook
          (lambda ()
            (interactive)
            (flyspell-mode)
            (longlines-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section VIII: Forth mode                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'forth-mode "~/gforth/gforth.el")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
			    auto-mode-alist))
(autoload 'forth-block-mode "~/gforth/gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
			    auto-mode-alist))
(add-hook 'forth-mode-hook (function (lambda ()
				       ;; customize variables here:
				       (setq forth-indent-level 4)
				       (setq forth-minor-indent-level 2)
				       (setq forth-hilight-level 3)
				       ;;; ...
				       )))