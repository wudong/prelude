;;; customize.el --- Personal customizaton.
;;; Commentary:
;; This file will be loaded by prelude when Emacs starts.

;;; Code:
;; Disable scroll bar
(scroll-bar-mode -1)

;; expand region
(prelude-require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; goto-last-change
(prelude-require-package 'goto-last-change)
(global-set-key (kbd "C-<") 'goto-last-change)

;; auto-fill mode to automatic wrap line in text mode.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; enable global line-number on the left mode for the given mode.
(add-hook 'text-mode-hook (lambda() (linum-mode t)))

;;; customize.el ends here
