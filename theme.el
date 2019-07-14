(deftheme minimal
  "Created 2019-05-14.")

(custom-theme-set-variables
 'minimal
 ;;'(fringe-mode (2 . 2)))
)
(custom-theme-set-faces
 'minimal
 '(custom-group-tag ((t (:inherit variable-pitch :weight bold))))
 '(custom-group-tag-1 ((t (:inherit variable-pitch))))
 '(default ((t (:foreground "black" :background "white"))))
 '(error ((t (:foreground "orangered3" :weight bold))))
 '(fixed-pitch-serif ((t (:foreground "firebrick4"))))
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "gray67"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t (:slant italic))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-negation-char-face ((t (:underline "red"))))
 '(font-lock-string-face ((t nil)))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(helm-M-x-key ((t (:foreground "seagreen" :box (:line-width 1 :color "honeydew2")))))
 '(helm-action ((t (:underline "seagreen" :slant italic))))
 '(helm-buffer-archive ((t (:foreground "Gold4"))))
 '(helm-buffer-directory ((t (:foreground "blue4"))))
 '(helm-candidate-number ((t (:weight bold))))
 '(helm-ff-directory ((t (:foreground "blue4" :weight bold))))
 '(helm-ff-dotted-directory ((t (:foreground "dimgrey"))))
 '(helm-ff-executable ((t (:foreground "seagreen"))))
 '(helm-ff-invalid-symlink ((t (:background "tomato" :foreground "maroon4"))))
 '(helm-ff-prefix ((t (:background "lightyellow" :foreground "goldenrod4"))))
 '(helm-ff-suid ((t (:background "tomato" :foreground "linen"))))
 '(helm-grep-finish ((t (:foreground "seaGreen"))))
 '(helm-grep-lineno ((t (:inherit font-lock-comment-face))))
 '(helm-lisp-show-completion ((t (:background "slategray" :foreground "slategray1"))))
 '(helm-locate-finish ((t (:foreground "seagreen"))))
 '(helm-selection ((t (:inherit highlight))))
 '(helm-source-header ((t (:slant italic :weight bold))))
 '(highlight ((t (:background "lightyellow2"))))
 '(info-index-match ((t (:inherit match))))
 '(isearch ((t (:background "lightyellow1" :foreground "orange4"))))
 '(lazy-highlight ((t (:background "lemonchiffon2" :foreground "lemonchiffon4"))))
 '(link ((t (:foreground "blue" :underline t))))
 '(match ((t (:background "lightyellow"))))
 '(mode-line ((t (:background "grey90" :box (:line-width 1 :color "grey70")))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-highlight ((t (:background "slateblue" :foreground "white"))))
 '(mode-line-inactive ((t (:background "grey95" :foreground "grey75" :box (:line-width 1 :color "grey80")))))
 '(region ((t (:background "grey92" :foreground "darkgreen"))))
 '(show-paren-match ((t (:inherit mode-line-highlight))))
 '(show-paren-mismatch ((t (:inherit error)))))

(provide-theme 'minimal)
