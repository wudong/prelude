;;; orgmode-customize.el -- OrgMode customization.
;;; Commentary:
;; this file will be loaded by prelude when Emacs starts.

;;htmlize to convert buffer
(prelude-require-package 'htmlize)

;; main org-directory
(setq org-directory "~/Dropbox/org")


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
              )))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "light green" :weight bold)
              )))

;; syntax hightlight in orgmode
;; http://stackoverflow.com/questions/10642888/syntax-highlighting-within-begin-src-block-in-emacs-orgmode-not-working
(setq org-src-fontify-natively t)
;; set agenda files
(setq org-agenda-files (quote ("~/Repository/org")))
;; set execution on source code block
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (java . t)
   (js . t)
   ))

;; add the org-timer
;; http://orgmode.org/worg/org-gtd-etc.html
;; (add-to-list 'org-modules 'org-timer)
;; (require 'org-timer)
;; set a default value for the timer (for promodora)
;; (setq org-timer-default-timer 25)
;; add a hook when org-clock-in so that a timer is started
;; with the default value, except if a timer is already started.
;; (add-hook 'org-clock-in-hook (
;; lambda()(if (not 'org-timer-current-timer)
;;      (org-timer-set-timer '(16)))))

;; capture is used to quickly store notes with little interruption of
;; the workflow
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes,

(setq org-capture-templates
      '(("t" "Todo Item" entry (file+headline (concat org-directory "/gtd.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Quick Note" entry (file (concat org-directory "/notes.org"))
         "* %?\nEntered on %U\n  %i\n")
        ("s" "Scribble" entry (file (concat org-directory "/scribble.org"))
         "* %?\nEntered on %U\n  %i\n")
        ))

(setq org-clock-into-drawer "CLOCKING")

;; setting up sync with google calendar.
;; (prelude-require-package 'org-gcal)
;; (require'org-gcal)

;; (setq org-gcal-client-id "447777984749-qn6b8d232jheo97t0tneqknojqgdiam5.apps.googleusercontent.com"
;;       org-gcal-client-secret "VLMI7ebbYxPyZG1zWP_aQCuO"
;;       org-gcal-file-alist '(("wudong.liu@gmail.com" .  "~/Repository/org/schedule.org")
;; ))
;; call the gcal sync when necessary.
;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))

;; blogging with orgmode, publishing to wordpress.
(setq org2blog/wp-blog-alist
      '(("graceliu"
         :url "https://www.graceliu.uk/xmlrpc.php"
         :username "wudong"
         :password "yuepan2008"
         :wp-code t
         :wp-latex t
         :default-title "title"
         )))

(setq org2blog/wp-default-categories '("编程") )
