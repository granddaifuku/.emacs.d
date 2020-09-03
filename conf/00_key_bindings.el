;; Key Bindings

(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(setq kill-whole-line t)

(defun rename-file-and-buffer (new-name)
  "Rename both current buffer and file."
  (interactive "New Name:")
  (let ((name (buffer-name))
		(filename (buffer-filename)))
	(if (not filename)
		(message "Buffer '%s' is not visiting a file!" name)
	  (if (get-buffer new-name)
		  (message "Buffer named '%s' already exists!" new-name)
		(progn
		  (rename-file name new-name 1)
		  (rename-buffer new-name)
		  (set-visited-file-name new-name)
		  (set-buffer-modified-p nil))))))
  
(global-set-key (kbd "C-c C-n") 'rename-file-and-buffer)

(defun copy-whole-line (&optional arg)
  "Copy current line."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
	  (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
	  (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'copy-region-as-kill)
	(kill-new "")
	(setq last-command 'copy-region-as-kill))
  (cond ((zerop arg)
		 (save-excursion
		   (copy-region-as-kill (point) (progn (forward-visible-line 0) (point)))
		   (copy-region-as-kill (point) (progn (end-of-visible-line) (point)))))
		 ((< arg 0)
		  (save-excursion
			(copy-region-as-kill (point) (progn (end-of-visible-line) (point)))
			(copy-region-as-kill (point)
								 (progn (forward-visible-line (1+ arg))
										(unless (bobp) (backward-char))
										(point)))))
		(t
		 (save-excursion
		   (copy-region-as-kill (point) (progn (forward-visible-line 0) (point)))
		   (copy-region-as-kill (point)
								(progn (forward-visible-line arg) (point))))))
  (message (substring (car kill-ring-yank-pointer) 0 -1)))

(global-set-key (kbd "M-k") 'copy-whole-line)
