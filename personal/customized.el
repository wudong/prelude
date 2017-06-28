;;; customize.el --- Personal customizaton.
;;; Commentary:
;; This file will be loaded by prelude when Emacs starts.

;;; this starts the server mode. which allow the client to connect to.
(server-start)

;;; OSX specific settings
;; reverse the key that defined by the prelude mac module.
;; disable the right alternate-modifier
;; it will disable the use the right alt key is the meta key.
;; mainly for the purpose of mac keyboard with the #.
(when (eq system-type 'darwin)
  (setq ns-right-alternate-modifier (quote none))
  (define-key prelude-mode-map (kbd "C-c w") nil))

;;; enable the desktop mode to save emacs sessions.
;; so buffer/history etc will be restored upon restart
(desktop-save-mode 1)

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

;;
(prelude-require-package 'ibuffer-projectile)
;; neotree configurations

;; every time when the neotree window is opened, let it find
;; current file and jump to node.
(setq neo-smart-open t)


(message "current display is: %s" (if (display-graphic-p) "graphical" "terminal"))
;; set the theme to use icon while in graphical and arrow
;; while in console
(setq neo-theme 'icons)

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

;; ibuffer-hook for ibuffer-projectile
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; syntax hightlight in orgmode
;; http://stackoverflow.com/questions/10642888/syntax-highlighting-within-begin-src-block-in-emacs-orgmode-not-working
(setq org-src-fontify-natively t)
;; set agenda files
(setq org-agenda-files (quote ("~/Repository/org")))


;; ace-window configuration
(global-set-key (kbd "M-p") 'ace-window)

;; openwith files in external
(prelude-require-package 'openwith)
(openwith-mode t)
(setq pdf-opener (if (eq system-type 'darwin) "open" "evince") )
(setq openwith-associations '(("\\.pdf\\'" pdf-opener (file))))

;;; customize.el ends here
