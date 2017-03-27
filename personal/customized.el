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

;; require environment from shell, which is from the .bashrc
(prelude-require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; php-mode
(prelude-require-package 'php-mode)

;; required for neotree's icon theme;
;; also need to download and install the fonts.
(prelude-require-package 'all-the-icons)

;; magit
(prelude-require-package 'magit)

;; neotree
(prelude-require-package 'neotree)

;; neotree configurations

;; every time when the neotree window is opened, let it find
;; current file and jump to node.
(setq neo-smart-open t)


(message "current display is: %s" (if (display-graphic-p) "graphical" "terminal"))
;; set the theme to use icon while in graphical and arrow
;; while in console
(setq neo-theme 'icons)

;; change root automatically when running projectile-switch-project
(setq projectile-switch-project-action 'neotree-projectile-action)

;; open neotree at projectile project root
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (if project-dir
          (if (neotree-toggle)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
(global-set-key [f8] 'neotree-project-dir)

;; auto-fill mode to automatic wrap line in text mode.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; enable global line-number on the left mode for the given mode.
(add-hook 'text-mode-hook (lambda() (linum-mode t)))

;; syntax hightlight in orgmode
;; http://stackoverflow.com/questions/10642888/syntax-highlighting-within-begin-src-block-in-emacs-orgmode-not-working
(setq org-src-fontify-natively t)

;;; customize.el ends here
