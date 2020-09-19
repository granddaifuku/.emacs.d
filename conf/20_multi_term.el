;; multi-term
(require 'use-package)

(use-package multi-term
  :ensure t
  :defer t
  :init
  (defun open-shell-sub (new)
	(split-window-below)
	(enlarge-window 12)
	(other-window 1)
	(let ((term) (res))
	  (if (or new (null (setq term (dolist (buf (buffer-list) res)
									 (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
										 (setq res buf))))))
		  (multi-term)
		(switch-to-buffer term))))
  (defun open-shell-sub-right (new)
	(split-window-right)
	(enlarge-window-horizontally 40)
	(other-window 1)
	(let ((term) (res))
	  (if (or new (null (setq term (dolist (buf (buffer-list) res)
									 (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
										 (setq res buf))))))
		  (multi-term)
		(switch-to-buffer term))))
  (defun open-shell ()
	(interactive)
	(open-shell-sub t))
  (defun open-shell-r ()
	(interactive)
	(open-shell-sub-right t))
  (defun to-shell ()
	(interactive)
	(open-shell-sub nil))
  :bind
  ("C-c m" . open-shell)
  ("C-c n" . open-shell-r)
  (:map term-mode-map
		("C-c C-p" . multi-term-prev)
		("C-c C-n" . multi-term-next))
  :config
  (setq scroll-conservatively 1)
  (setq next-screen-context-lines 5)
  (setenv "SHELL" shell-file-name)
  (setq multi-term-program shell-file-name)
  (setq system-uses-terminfo t)
  (defadvice term-interrupt-subjob
	  (around ad-term-interrupt-subjob activate)
	(term-send-raw-string "\C-c")))
