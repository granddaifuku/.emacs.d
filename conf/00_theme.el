;; Theme
(add-to-list 'custom-theme-load-path
			 (file-name-as-directory "~/.emacs.d/replace-colorthemes/clarity-theme.el"))
(load-theme 'clarity t t)
(enable-theme 'clarity)
(set-face-foreground 'minibuffer-prompt "brown")
