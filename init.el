;; tips are now filed in emacs-tips.el
(prefer-coding-system 'utf-8)

(let ((default-directory  "~/Source/Repos/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(defvar my-packages
  '(better-defaults deft parinfer dash-functional rdf-prefix sparql-mode ttl-mode magit yasnippet which-key schrute csv-mode helpful helm helm-ag helm-slime helm-systemd helm-wordnet helm-xref))
(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(autoload 'sparql-mode "sparql-mode.el"
  "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
(add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode))

(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
(add-hook 'ttl-mode-hook    ; Turn on font lock when in ttl mode
          'turn-on-font-lock)
(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . ttl-mode)
        '("\\.ttl" . ttl-mode))
       auto-mode-alist))

(load-library "dash")
(load-library "dash-functional")
(eval-after-load 'dash '(dash-enable-font-lock))

(require 'helm-config)
(helm-mode)
(require 'which-key)
(which-key-mode)
(require 'deft)

;; helm-ify All The Things(TM)
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;; was 'ido-find-file
(global-set-key (kbd "C-x b") 'helm-buffers-list) ;; was 'ido-switch-buffer
(global-set-key (kbd "M-x") 'helm-M-x) ;; was 'execute-extended-command
(global-set-key (kbd "C-h a") 'helm-apropos) ;; was 'apropos-command
(global-set-key (kbd "M-o" ) 'helm-occur)

(setf helm-M-x-fuzzy-match t
      helm-apropos-fuzzy-match t
      helm-autoresize-mode t
      helm-buffers-fuzzy-matching t
      helm-completion-in-region-fuzzy-match t
      helm-etags-fuzzy-match t
      helm-ff-fuzzy-matching t
      helm-locate-fuzzy-match t
      helm-locate-library-fuzzy-match t
      helm-mode-fuzzy-match t
      helm-move-to-line-cycle-in-source t
      helm-split-window-in-side-p t)

(setf save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))

(setq frame-title-format
      '(buffer-file-name "%f" ; File buffer
			 (dired-directory dired-directory ; Dired buffer
					  (revert-buffer-function "%b" ; Buffer Menu
								  ("%b - Dir: " default-directory))))) ; Plain buffer

(setq org-fontify-done-headline t
      org-fontify-whole-heading-line t
      org-hide-leading-stars t
      org-highlight-sparse-tree-matches nil
      org-n-level-faces 8)

(setq inhibit-startup-message t
      tool-bar-mode nil
      show-paren-mode t
      scroll-bar-mode nil
      size-indication-mode t
      column-number-mode t
      ring-bell-function 'ignore)

(add-hook 'outline-mode-hook
	  (lambda () (local-set-key '[left]
				    outline-hide-entry)))
(add-hook 'outline-mode-hook
	  (lambda () (local-set-key '[right]
				    outline-show-entry)))
(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key '[left]
				    outline-hide-entry)))
(add-hook 'outline-minor-mode-hook
	  (lambda () (local-set-key '[right]
				    outline-show-entry)))

(if (eq window-system 'ns)
    (setq ns-mwheel-line-height 1
	  ns-use-mwheel-acceleration nil
	  ns-use-mwheel-momentum 1
	  ns-command-modifier 'control
	  ns-option-modifier 'meta)
  (setq mouse-wheel-scroll-amount '(5 ((shift) . 1))
	mouse-wheel-progressive-speed nil
	mouse-wheel-follow-mouse 't
	scroll-step 1))

(defun unicode-insert (char)
  "Read a unicode code point and insert said character. Input uses
`read-quoted-char-radix'. If you want to copy the values from
the Unicode charts, you should set it to 16."
  (interactive (list (read-quoted-char "Char: ")))
  (insert-char char))

(global-set-key (kbd "<f5>") 'set-selective-display-dlw)

(defun set-selective-display-dlw (&optional level)
  "Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL. If
'selective-display' is already set to LEVEL, clicking F5 again
will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(cond
 ((eq window-system 'ns) (setq inferior-lisp-program "/opt/bin/ccl"))
 ((eq window-system 'ms-dos) (setq inferior-lisp-program "C:/apps/clisp"))
 (t (setq inferior-lisp-program "/usr/bin/sbcl")))

(setq slime-contribs '(slime-fancy))
(add-to-list 'slime-contribs 'slime-repl)

(setq prettify-symbols-alist
      ;; other chars are super buggy - fix someday...
      ;; use M-x describe-char, M-x insert-char to explore
      '(("lambda" . 955)))
(prettify-symbols-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helpful csv-mode schrute which-key yasnippet magit better-defaults ttl-mode sparql-mode rdf-prefix parinfer helm-xref helm-wordnet helm-systemd helm-slime helm-ag deft dash-functional))))
