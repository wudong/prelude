;;; customize.el --- Personal customizaton.
;;; Commentary:
;; This file will be loaded by prelude when Emacs starts.

;;; adding the archive for orgmode.
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;; setting to turn on debug mode.
;;(setq debug-on-error t)

;;; this starts the server mode. which allow the client to connect to.
;;; (server-start)

;; eyebrowse to workspace.
(prelude-require-package 'eyebrowse)
(eyebrowse-mode t)
(setq eyebrowse-new-workspace t)

;;; yasnippet
(prelude-require-package 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)

;;; enable the desktop mode to save emacs sessions.
;; so buffer/history etc will be restored upon restart
(desktop-save-mode 1)

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

;; typescript-mode
(prelude-require-package 'typescript-mode)

;; add hook for tsx type.
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

;; required for neotree's icon theme;
;; also need to download and install the fonts.
(prelude-require-package 'all-the-icons)

;; magit
(prelude-require-package 'magit)

;; neotree
(prelude-require-package 'neotree)


;; loading ag.el for silver search.
(prelude-require-package 'ag)

;;; projectile configuration
;; open the project root dir in dired
(setq projectile-switch-project-action #'projectile-dired)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; neotree configurations

;; every time when the neotree window is opened, let it find
;; current file and jump to node.
(setq neo-smart-open t)

;; set the theme to use icon while in graphical and arrow
;; while in console
(setq neo-theme 'icons)

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
(global-set-key [f9] 'neotree-project-dir)

;; auto-fill mode to automatic wrap line in text mode.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; enable global line-number on the left mode for the given mode.
(add-hook 'text-mode-hook (lambda() (linum-mode t)))

;; ibuffer-hook for ibuffer-projectile
(prelude-require-package 'ibuffer-projectile)
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

(prelude-require-package 'ace-window)
;; ace-window configuration
(global-set-key (kbd "M-p") 'ace-window)

(setq vc-follow-symlinks t)

;; anki works on wsl2
(setq anki-editor-anki-connect-listening-address "192.168.10.29")


;;; desktop-override-stale-locks.el begins here
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (search-forward "emacs" nil t)
          pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here


(global-set-key "\M-N"  (lambda () (interactive) (scroll-up   1)) )
(global-set-key "\M-P"  (lambda () (interactive) (scroll-down 1)) )

;;; customize.el ends here
