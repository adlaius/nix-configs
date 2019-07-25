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
 '(Info-quoted ((t (:weight bold))))
 '(cursor ((t (:background "midnightblue"))))
 '(error ((t (:foreground "indianred"))))
 '(escape-glyph ((t (:distant-foreground "black" :foreground "white"))))
 '(fixed-pitch ((t (:family "Go Mono"))))
 '(font-lock-builtin-face ((t (:inherit default))))
 '(font-lock-comment-face ((t (:foreground "darkgray" :slant italic))))
 '(font-lock-constant-face ((t (:inherit default))))
 '(font-lock-function-name-face ((t (:weight black))))
 '(font-lock-keyword-face ((t (:inherit default))))
 '(font-lock-string-face ((t (: inherit default))))
 '(font-lock-type-face ((t (:inherit default))))
 '(font-lock-variable-name-face ((t (:inherit default))))
 '(fringe ((t (:background "gainsboro"))))
 '(header-line ((t (:inherit mode-line :inverse-video t :box nil))))
 '(helm-M-x-key ((t (:inherit default))))
 '(helm-buffer-directory ((t (:inherit link))))
 '(helm-buffer-process ((t (:inherit default))))
 '(helm-buffer-size ((t (:inherit default))))
 '(helm-candidate-number ((t (:inverse-video t))))
 '(helm-ff-directory ((t (:inherit link))))
 '(helm-ff-dotted-directory ((t (:background "lightgray"))))
 '(helm-ff-executable ((t (:inherit success))))
 '(helm-ff-prefix ((t (:inherit warning))))
 '(helm-grep-file ((t (:inherit link))))
 '(helm-grep-finish ((t (:inherit success))))
 '(helm-grep-lineno ((t (:inherit font-lock-comment-face))))
 '(helm-grep-match ((t (:inherit success))))
 '(helm-header-line-left-margin ((t (:inherit warning))))
 '(helm-match ((t (:inherit match))))
 '(helm-prefarg ((t (:inherit success))))
 '(helm-selection ((t (:inherit region))))
 '(helm-separator ((t (:inherit default))))
 '(helm-source-header ((t (:inherit (variable-pitch fringe) :slant italic :weight bold))))
 '(highlight ((t (:background "lightsteelblue1"))))
 '(info-node ((t (:weight bold))))
 '(isearch ((t (:inherit success :weight bold))))
 '(lazy-highlight ((t (:inherit match))))
 '(link ((t (:foreground "blue3" :underline t))))
 '(match ((t (:background "lightyellow"))))
 '(minibuffer-prompt ((t (:foreground "blue3"))))
 '(mode-line ((t (:background "gainsboro" :box nil))))
; '(mode-line-highlight ((t (:foreground "dodgerblue3"))))
 '(mode-line-inactive ((t (:background "gray95" :foreground "gray60"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-highlight ((t (:background "slateblue" :foreground "white"))))
 '(region ((t (:background "lavender"))))
 '(show-paren-match ((t (:background "seagreen" :foreground "white"))))
 '(show-paren-mismatch ((t (:inherit trailing-whitespace))))
 '(success ((t (:foreground "seagreen" :weight bold))))
 '(trailing-whitespace ((t (:background "indianred"))))
 '(fixed-pitch ((t (:family "Consolas"))))
 '(fixed-pitch-serif ((t (:foreground "firebrick4"))))
 '(variable-pitch ((t (:family "Merriweather Sans"))))
 '(warning ((t (:foreground "indianred" :weight bold))))
 


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
 '(highlight ((t (:background "lightsteelblue1"))))
 '(info-index-match ((t (:inherit match))))
 '(isearch ((t (:background "lightyellow1" :foreground "orange4"))))
 '(lazy-highlight ((t (:background "lemonchiffon2" :foreground "lemonchiffon4"))))
 '(link ((t (:foreground "blue" :underline t))))
 '(match ((t (:background "lightyellow"))))

)

(provide-theme 'minimal)
