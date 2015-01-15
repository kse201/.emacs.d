;;; org
(when (require 'org-install nil t)
  (require 'org-capture nil t)
  (require 'org-habit nil t)
                                        ;(org-remember-insinuate)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)

;;; hide extra '*'
  (setq org-hide-leading-stars t)
  (setq system-time-locale "C")
;;; org key-bind
  (define-many-keys global-map
    '(
      ("C-c l" . org-store-link)
      ("C-c a" . org-agenda)
      ("C-c r" . org-capture)
      ("C-c c" . org-capture)
      ("C-c b" . org-iswitchb)
      ))
;;; org dir/files
  (setq org-directory "~/org/")
  (setq org-default-notes-file "note.org")
  (setq org-agenda-files (list org-directory))
;;; underline in agenda
  (add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
  (setq hl-line-face 'underline)
;;; ignroe DONE
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
;;; 
  (setq org-deadline-warning-days 7)
;;; TODO
  (setq org-todo-keywords
        '((sequence "New(n!)" "InProgress(i!)" "Review(r!)" "Pending(p!)" "|" "Closed(c!)" "Rejected(R!)")))
  ;; log DONE time
  (setq org-log-done 'time)
  ;;; Tags
  (setq org-tag-alist
        '(
          ("@OFFICE" . ?o)
          ("@HOME" . ?h)
          ("SHOPPING" . ?s)
          ("MAIL" . ?m)
          ("PROJECT" . ?p)
          ("BOOKMARK" .?b)
          ))
;;; capture
  ;; template
  (setq org-capture-templates
        '(
          ("t" "Todo"       entry( file+headline "~/org/inbox.org" "Todo Backlog") "%[~/.emacs.d/org_templates/todo.txt]")
          ("p" "Project"    entry( file+headline "~/org/inbox.org" "Todo Backlog") "%[~/.emacs.d/org_templates/project.txt]")
          ("d" "Daily Todo" entry( file+headline "~/org/daily.org" "Daily Todo")   "%[~/.emacs.d/org_templates/todo.txt]")
          ("n" "Note"       entry( file+headline nil               "Note")         "%[~/.emacs.d/org_templates/note.txt]")
          ("b" "Bookmark"   entry( file+headline nil               "Bookmark")     "%[~/.emacs.d/org_templates/bookmark.txt]")
          ))
  ;; refile destination
  (setq org-refile-targets
        (quote (("daily.org" :level . 1)
                ("inbox.org" :level . 1)
                )))
  ;; #+ARCHIVE: todo_archive::
;;; time grid
  (setq org-agenda-time-grid
        '((daily today require-timed)
          "----------------"
          (900 1000 1100 1200 1300 1400 1500 1600 1700)))
;;; custom commands
  (setq org-agenda-custom-commands
        '(
          ("x" "Unscheduled TODO" tags-todo "-SCHEDULED>=\"<now>\"" nil)
          ("D" "Daily Action List"
           ((agenda "" ((org-agenda-ndays 1)
                        (org-agenda-sorting-strategy
                         (quote ((agenda time-up priority-down tag-up) )))
                        (org-deadline-warning-days 0)
                        ))))
          ))
  (setq org-stuck-projects
        '("+PROJECT/-Closed-Rejected" ("New" "Pending")))
  ;; review
  (defun gtd ()
    (interactive)
    (find-file "~/org/inbox.org"))
  (global-set-key (kbd "C-c g") 'gtd)
  )
