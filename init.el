(add-to-list 'load-path "~/.emacs.d/elisp") ;; personal stuff
(require 'generic-x) ;; generic syntax highlighting for files of unspecified type
(autoload 'disk "disk" "Do The Right Thing(TM) with files/buffers." t)

(setq inhibit-startup-message t) ;; cf. ~/.Xresources for additional settings
(setq use-dialog-box nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; By default, 'case-fold-search is t, which makes basic C-s searches
;; case-insensitive. In incremental search, this can be handy. It
;; destroys regexp searches, though. So first we draw a modeline
;; indicator if its value is non-nil:
(add-to-list 'minor-mode-alist '(case-fold-search " CFS"))

;; Now we add a hook to isearch so we can move in/out of regexp search
;; and case-insensitive isearch. 'isearch-edit-string allows us to
;; edit the search string in the minibuffer.
(add-hook 'isearch-mode-hook
	  (function
	   (lambda ()
	     (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
	     (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
	     (define-key isearch-mode-map "\C-j" 'isearch-edit-string))))

(define-key global-map (kbd "C-w") 'backward-kill-word) ;; match GNU Readline
(define-key global-map (kbd "C-c k") 'kill-region) ;; used to be "C-w"; find a suitable home
(define-key global-map (kbd "M-g *") 'ffap) ;; find-file-at-point, a la Vim
(define-key global-map (kbd "M-p") 'upcase-previous-word)

(defun upcase-previous-word ()
  (interactive)
  (progn
    (backward-word)
    (upcase-word 1)
    (backward-word)))

(define-key global-map (kbd "<f5>") 'disk)
(define-key global-map (kbd "<f7>") 'eval-buffer)

(defvar *background-color* (if (equal (window-system) nil) 'dark 'light)
  "Default frame background color value; one of 'dark or 'light.")

(defun toggle-colors ()
  "Switch to/from dark background, sans color theme tomfoolery."
  (interactive)
  (if (equal *background-color* 'light)
      (progn
	(set-foreground-color "gray88")
	(set-background-color "#0c101c")  ;; #0c101c is a nice "inkpot" hue
	(setq *background-color* 'dark))
    (progn
      (set-foreground-color "black")
      (set-background-color "ivory")
      (setq *background-color* 'light))))

(global-set-key (kbd "<f9>") 'toggle-colors)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elisp/sage-mode-0.6/emacs"))
(require 'sage "sage")
(setq sage-command "/usr/bin/sage")

;; If you want sage-view to typeset all your output and have plot()
;; commands inline, uncomment the following line and configure sage-view:
;; (require 'sage-view "sage-view")
;; (add-hook 'sage-startup-hook 'sage-view)
;; You can use commands like
;; (add-hook 'sage-startup-hook 'sage-view
;; 'sage-view-disable-inline-output 'sage-view-disable-inline-plots)
;; to have some combination of features.  In future, the customize interface
;; will make this simpler... hint, hint!

;; taken from http://www.cs.caltech.edu/courses/cs11/material/python/misc/python_style_guide.html#TABS
(add-hook 'sage-mode-hook
      '(lambda ()
         (set-variable 'py-indent-offset 4)
         (set-variable 'py-smart-indentation nil)
         (set-variable 'indent-tabs-mode nil) ))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(font-lock-maximum-decoration nil)
 '(fringe-mode (quote (0)) nil (fringe))
 '(indicate-buffer-boundaries (quote right))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify))))
