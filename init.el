(add-to-list 'load-path "~/.emacs.d/elisp") ;; personal stuff

;; cf. ~/.Xresources for additional settings
(setq inhibit-startup-message t)

(fset 'yes-or-no-p 'y-or-n-p)

(define-key global-map (kbd "C-w") 'backward-kill-word) ;; match GNU Readline
(define-key global-map (kbd "C-c k") 'kill-region) ;; used to be "C-w"; find a suitable home
(define-key global-map (kbd "M-g *") 'ffap) ;; bit of consistency w/Vim
(define-key global-map (kbd "<f5>") 'save-buffer)
(define-key global-map (kbd "<f7>") 'eval-buffer)

(defvar *background-color* 'dark
  "One of 'light or 'dark; corresponds to frame background value.")

(defun toggle-colors ()
  "Switch to/from dark background, sans color themes tomfoolery."
  (interactive)
  (if (equal *background-color* 'light)
      (progn
	(set-foreground-color "gray88")
	(set-background-color "black")
	(setq *background-color* 'dark))
    (progn
      (set-foreground-color "black")
      (set-background-color "white")
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
