;;; package --- Summary

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section IX: ESS and Julia
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is ESS Debian package.
;(require 'ess-site)

;;; This is my local ESS package from github.
;(add-to-list 'load-path "~/scribus/code/ESS/lisp/")
;(load "ess-site")

(add-to-list 'load-path "~/scribus/code/julia-emacs")
(require 'julia-mode)

;;; This is my local Julia package in ~/bin.
;(setq inferior-julia-program-name "~/bin/j05")

;;; This is the source folder from github.
;(add-to-list 'ess-tracebug-search-path "~/.julia/release-0.5")

;;; local package from github
;(add-to-list 'load-path "~/scribus/code/julia-shell-mode")
;(defun my-julia-mode-hooks ()
;  (require 'julia-shell-mode))
;(add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;(define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;(define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section I: Generic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set up emacs server in the background.
;(load "server")
;(unless (server-running-p)
; (server-start))

;;; solarized-theme
;(package-initialize)
;(load-theme 'solarized-dark t)

;;; Makes the mini-buffer stay one line.
;(setq resize-mini-windows nil)

;;; Adds line numbers.
(global-linum-mode)

;;; Web Browser.
;(setq browse-url-browser-function 'eww-browse-url)

;;; Shows matching parentesies globaly.
;(show-paren-mode 1)

;;; Uses UTF-8 by default.
;(set-language-environment "UTF-8")

;;; Don't ring the bell.
(setq ring-bell-function 'ignore)

;;; Don't pollute files.
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/emacs.d/.saves"))
 auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;; https://www.emacswiki.org/emacs-test/InteractivelyDoThings
;(require 'ido)
;(ido-mode t)
;(setq ido-enable-flex-matching t)
;(setq ido-everywhere t)
;(global-set-key (kbd "C-o") 'other-window)
;(global-set-key (kbd "C-x o") 'open-line)
;(global-set-key (kbd "M-1") 'delete-other-windows)
;(global-set-key (kbd "M-2") 'split-window-vertically)
;(global-set-key (kbd "M-3") 'split-window-horizontally)
;(global-set-key (kbd "M-0") 'delete-window)
;(global-set-key (kbd "<RET>") 'newline-and-indent)

;;; https://github.com/nonsequitur/smex/
;(global-set-key (kbd "M-x") 'smex) 
;(global-set-key (kbd "M-X") 'smex-major-mode-commands) 
;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(unless (package-installed-p 'org-bullets)
  (package-refresh-contents)
  (package-install 'org-bullets))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section VIII: Forth
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'forth-mode "~/.local/lib/gforth/gforth.el")
(setq auto-mode-alist (cons '("\\.fs\\'" . forth-mode)
			    auto-mode-alist))
(autoload 'forth-block-mode "~/.local/lib/gforth/gforth.el")
(setq auto-mode-alist (cons '("\\.fb\\'" . forth-block-mode)
			    auto-mode-alist))
(add-hook 'forth-mode-hook (function (lambda ()
				       ;;; customize variables here:
				       (setq forth-indent-level 4)
				       (setq forth-minor-indent-level 2)
				       (setq forth-hilight-level 3)
				       ;;; ...
				       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section V: Common Lisp
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Install CMUCL (x86 only), manually in /usr/local/ (lisp) or by package
;;; (cmucl).
;;; Install SBCL, manually in /usr/local/ or by package (sbcl both).
;;; Install quicklisp by REPL in both:
;;; * (load "quicklisp.lisp")
;;; * (quicklisp-quickstart:install)
;;; * (ql:add-to-init-file)

;;; M-x package-install
;;; ===================
;;; sly
;;; sly-mode
;;; sly-company
;;; sly-hello-world
;;; sly-macrostep
;;; sly-named-read-tables
;;; sly-quicklisp
;;; sly-repl-ansi-colors
;;; ====================

;;; I want cmucl to be used by default when I start:
;;; M-x sly
;;; =======

(add-to-list 'load-path "~/common-lisp/sly")
(require 'sly-autoloads)
(setq inferior-lisp-program "~/.local/bin/ccl")

;;; It makes sure sly is using utf-8.

(setq sly-net-coding-system 'utf-8-unix)

;;; The following function will be executed when we enter lisp-mode,
;;; which is active in buffers with Common Lisp code:

;;; Help to deal with lisp parenthesies.
;;; M-x package-install
;;; =================== 
;;; paredit
;;; ============

(defun lisp-hook-fn ()
  (interactive)
  (sly-mode)
  (local-set-key [tab] 'sly-complete-symbol)
  (local-set-key (kbd "M-q") 'sly-reindent-defun)
  (set (make-local-variable lisp-indent-function)
       'common-lisp-indent-function)
  (setq sly-load-failed-fasl 'never)
  (paredit-mode +1))

;;; It tells lisp-mode to run my function on startup.

(add-hook 'lisp-mode-hook 'lisp-hook-fn)

;;; The Common Lisp Hyperspec is an online HTML version of the Lisp
;;; standard. Sly has a built-in function called
;;; hyperspec-lookup
;;; ================
;;; which will show the page for a symbol in the spec.
;;; To install it on a Debian-like system run:
;;; # apt-get install hyperspec
;;; ===========================

(setq common-lisp-hyperspec-root "file:///usr/share/doc/hyperspec/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Section XI: TeX
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Useful AUCTeX setup for ConTeXt by Sanjoy Mahajan.
;;; The AUCTeX manual recommends these settings.

(setq TeX-parse-self t)			; Enable parse on load.
(setq TeX-auto-save t)			; Enable parse on save.

;;; for outline views (hide/show sections, chapters, etc.)

(add-hook 'TeX-mode-hook '(lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (outline-minor-mode 1)))

;;; make PDF by default (can toggle with C-c C-t C-p

(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))

;;; these math abbrevs (` as prefix char) are also useful in
;;; TeX/ConTeXt files

(require 'latex)			; defines LaTeX-math-mode
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

;;; Emacs help for \label, \ref, \cite.  Normally used only with
;;; LaTeX-mode but also useful with plain TeX + eplain and with ConTeXt

(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'reftex-mode)
(defun insert-balanced (left right)
  "Insert a left, right delmiter pair and be poised to type inside them."
  (interactive)
  (insert left)
  (save-excursion
    (insert right)))
;;; When start-context-math() is bound to $:
;;; Typing one $ gets you $$ with the insertion point between them.
;;; Typing a second $ turns the $$ into ConTeXt's form for displayed math:
;;;   \placeformula\startformula
;;;   [blank line with insertion point at beginning]
;;;   \stopformula
;;; Delete the \placeformula if you don't want equations numbered
;;; automatically.

(defun start-context-math ()
  (interactive)
  (let* ((start (max (point-min) (- (point) 1)))
         (stop  (min (point-max) (+ (point) 1))))
    
;;; if in the middle of a $$, turn inline math into context display math
    
(if (equal "$$" (buffer-substring-no-properties start stop))
    (progn
      (delete-region start stop)	;get rid of the $$
; delete preceding spaces, if any
(while (and (< (point-min) (point))
            (equal (buffer-substring-no-properties (- (point) 1)
                                                   (point))
                   " "))
  (backward-delete-char 1))
; delete a preceding newline, if any
(if (equal (buffer-substring-no-properties (- (point) 1)
                                           (point))
           "\n")
    (backward-delete-char 1))
; ConTeXt's display math with automatic equation numbering
(insert "\n\\placeformula\\startformula\n")
(save-excursion (insert "\n\\stopformula")))
; else: just doing inline math
(insert-balanced ?\$ ?\$))))
; automatically insert right delimiter for $, {, [, and ( and be
; poised to type inside them.
(add-hook 'TeX-mode-hook
          '(lambda ()
             (local-set-key "$"
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\$ ?\$)))
             (local-set-key "{"
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\{ ?\})))
             (local-set-key "["
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\[ ?\])))
             (local-set-key "("
                            '(lambda ()
                               (interactive)
                               (insert-balanced ?\( ?\))))))
;;; For ConTeXt mode, inserting two $ signs needs to behave specially
(add-hook 'ConTeXt-mode-hook
          '(lambda ()
             (local-set-key "$" 'start-context-math)))
;;; The TeX-format-list from AUCTeX's tex.el (v11.82) with a few more
;;; ConTeXt-specific patterns.  I've submitted it to the AUCTeX lists,
;;; so later versions should have them automatically and you won't need
;;; this regexp mess in your .emacs
(setq TeX-format-list
      '(("JLATEX" japanese-latex-mode
         "\\\\\\(documentstyle\\|documentclass\\)[^%\n]*{\\(j[s-]?\\|t\\)\\(article\\|report\\|book\\|slides\\)")
        ("JTEX" japanese-plain-tex-mode
         "-- string likely in Japanese TeX --")
        ("AMSTEX" ams-tex-mode
         "\\\\document\\b")
        ("CONTEXT" context-mode
         "\\(\\\\\\(start\\(text\\|project\\|environment\\|product\\|typing\\|component\\|tekst\\)\\)\\|%.*?interface=\\)")
        ("LATEX" latex-mode
         "\\\\\\(begin\\|section\\|chapter\\|documentstyle\\|documentclass\\)\\b")
        ("TEX" plain-tex-mode ".")))
(defun context-insert-nattab (rows columns)
;;; Johan Sandblom 2006-01-28
"Insert a TABLE skeleton"
(interactive "nNumber of rows: \nnNumber of columns: \n")
(newline)
(insert "\\bTABLE\n\\setupTABLE\[\]\n")
;;; First a TABLE header
(insert "\\bTABLEhead\n\\bTR\\bTH \\eTH\n")
(let ((column 1))
  (while (< column (- columns 1))
    (insert "    \\bTH \\eTH\n")
    (setq column (1+ column))))
(insert "    \\bTH \\eTH\\eTR\n\\eTABLEhead\n\\bTABLEbody\n")
;; The rows and columns
(let ((row 1))
  (while (<= row rows)
    (insert "\\bTR\\bTD \\eTD\n")
;; The let expression makes sure that each loop starts at the
;; right place
(let ((column 1))
  (while (< column (- columns 1))
    (insert "    \\bTD \\eTD\n")
    (setq column (1+ column)))
  (insert "    \\bTD \\eTD\\eTR\n")
  (setq row (1+ row))))
(insert "\\eTABLEbody\n\\eTABLE\n")))
(defun context-insert-nattab-row (columns)
   "Insert a row in a TABLE"
    (interactive "nNumber of columns: \n")
     (newline)
      (insert "\\bTR\\bTD \\eTD\n")
       (let ((column 1))
	    (while (< column (- columns 1))
		        (insert "    \\bTD \\eTD\n")
			     (setq column (1+ column)))
	       (insert "    \\bTD \\eTD\\eTR\n")))
(defun context-insert-nattab-column (&optional arg)
   "Insert a column in a TABLE"
    (interactive "P")
     (insert "\\bTD \\eTD")
      (indent-for-tab-command)
       (newline)
        (backward-char 5))
(add-hook 'ConTeXt-mode-hook
          '(lambda ()
             (local-set-key "\C-cnr" 'context-insert-nattab-row)
             (local-set-key "\C-cnc" 'context-insert-nattab-column)
             (local-set-key "\C-cnn" 'context-insert-nattab)))
;; Johan Sandblom. No copyright.
(defun context-FLOW-shift-cells (x y beg end)
  "Shifts FLOW cells in region right and down"
  (interactive "nRight: \nnDown: \nr")
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp
            "{\\([0-9]+\\),\\([0-9]+\\)}" end t)
      (replace-match 
       (concat "{" (number-to-string 
                    (+ (string-to-number (match-string 1)) x))
               ","  (number-to-string 
                     (+ (string-to-number (match-string 2)) y)) "}")
       nil nil))))
(defun context-FLOW-insert-cells (%optional n)
  "Insert a FLOWchart cell"
  (interactive "P")
  (if (not (bolp)) (newline))
  (let ((x 1))
    (while (<= x (if n n 1))
      (insert "\\startFLOWcell\n")
      (insert "  \\name          {}\n")
      (insert "  \\location      {}\n")
      (insert "  \\shape         {action}\n")
      (insert "  \\text          {}\n")
      (insert "  \\connection[rl]{}\n")
      (insert "\\stopFLOWcell\n")
      (setq x (1+ x)))))
(add-hook 'ConTeXt-mode-hook
          '(lambda ()
             (define-key (current-local-map) "\C-cnF" 'context-FLOW-insert-cells)
             (define-key (current-local-map) "\C-cnS" 'context-FLOW-shift-cells)))
;; Johan Sandblom. No copyright.
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(mmm-add-group 'context-plus
               '((context-R
                  :submode r-mode
                  :face mmm-comment-submode-face
                  :front ".*\\\\startR\\w*\\({\\w*}\\|\\[\\w*\\]\\|\\)\\W*"
                  :back  ".*\\\\stopR")
                 (context-MP
                  :submode metapost-mode
                  :face mmm-code-submode-face
                  :front ".*\\\\start\\w*MP\\w*\\({\\w*}\\|\\[\\w*\\]\\|\\)\\W*"
                  :back  ".*\\\\stop\\w*MP")
                 ))
(add-to-list 'mmm-mode-ext-classes-alist '(context-mode nil context-plus))



(provide 'jankiel)
;;; akatom-others.el ends here
