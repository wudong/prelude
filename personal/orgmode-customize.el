;;; orgmode-customize.el -- OrgMode customization.
;;; Commentary:

;; this file will be loaded by prelude when Emacs starts.

;;htmlize to convert buffer
(prelude-require-package 'htmlize)
(prelude-require-package 'ox-gfm)

;; load the markdown exporter
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; main org-directory
(setq org-directory "~/repo/org")
(setq org-todo-capture-file "~/repo/org/gtd.org")

(setq org-refile-targets '(("~/repo/org/gtd.org" :maxlevel . 1)
                           ("~/repo/org/anki.org" :maxlevel . 1)))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")
              )))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "light yellow" :weight bold)
              )))

;; syntax hightlight in orgmode
;;  http://stackoverflow.com/questions/10642888/syntax-highlighting-within-begin-src-block-in-emacs-orgmode-not-working
(setq org-src-fontify-natively t)
;; set agenda files
(setq org-agenda-files (quote (concat org-directory "/gtd.org")))
(setq org-agenda-ndays 7)
(setq org-deadline-warning-days 14)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-reverse-note-order t)

;; https://orgmode.org/manual/Breaking-down-tasks.html
;;(defun org-summary-todo (n-done n-not-done)
;;  "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;  (let (org-log-done org-log-states)   ; turn off logging
;;    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; set execution on source code block
                                        ;(org-babel-do-load-languages
;; 'org-babel-load-languages
                                        ;'((python . t)
;;   (shell . t)
;;   (java . t)
;;   (js . t)
;;   ))

;; capture is used to quickly store notes with little interruption of
;; the workflow
(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks,
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/repo/org/gtd.org" "Collect")
         "* TODO %? %^g \n  %U" :empty-lines 1)
        ("s" "Scheduled TODO" entry (file+headline "~/repo/org/gtd.org" "Collect")
         "* TODO %? %^g \nSCHEDULED: %^t\n  %U" :empty-lines 1)
        ("d" "Deadline" entry (file+headline "~/repo/org/gtd.org" "Collect")
         "* TODO %? %^g \n  DEADLINE: %^t" :empty-lines 1)
        ("p" "Priority" entry (file+headline "~/repo/org/gtd.org" "Collect")
         "* TODO [#A] %? %^g \n  SCHEDULED: %^t")
        ("a" "Appointment" entry (file+headline "~/repo/org/gtd.org" "Collect")
         "* %? %^g \n  %^t")
        ("l" "Link" entry (file+headline "~/repo/org/gtd.org" "Links")
         "* TODO %a %? %^g\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("n" "Note" entry (file+headline "~/repo/org/anki.org" "NoteCollect")
         "** %? %^g \n%U" :empty-lines 1)
        ))

;; anki-editor, connect to WSL window
(setq anki-editor-anki-connect-listening-address "192.168.10.29")
