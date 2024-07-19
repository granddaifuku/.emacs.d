;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

(setq gc-cons-threshold 100000000)

(push '(fullscreen . maximized) default-frame-alist)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
