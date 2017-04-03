;;; customize.el --- Personal customizaton.
;;; Commentary:
;; This file will be loaded by prelude when Emacs starts.

;;; Code:
;; Disable scroll bar
(scroll-bar-mode -1)

;; install material theme
(prelude-require-package 'material-theme)

;; expand region
(prelude-require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; goto-last-change
(prelude-require-package 'goto-last-change)
(global-set-key (kbd "C-<") 'goto-last-change)

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

;; set the theme to use icon while in graphical and arrow
;; while in console
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; loading ag.el for silver search.
(prelude-require-package 'ag)

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

;; ace-window configuration
(global-set-key (kbd "M-p") 'ace-window)

;;; customize.el ends here
