;;; org
(when (require 'org nil t)
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
;;; TODO
  (setq org-todo-keywords
        '((sequence "New(n!)" "InProgress(i!)" "Review(r!)" "Pending(p!)" "|" "Closed(c!)" "Rejected(R!)")))
  ;; log DONE time
  (setq org-log-done 'time)
;;; tags
  (setq org-tag-alist
        '(("@OFFICE" . ?o)
          ("@HOME" . ?h)
          ("SHOPPING" . ?s)
          ("MAIL" . ?m)
          ("PROJECT" . ?p)))
;;; capture templates
  (setq org-capture-templates
        '(("n" "Note" entry( file+headline nil "Note") "* %?\n  %u%i")
          ("t" "Todo" entry( file+headline "~/org/todo.org" "Todo") "* New %?\n  %t%i")
          ("p" "Project" entry ( file+headline "~/org/todo.org" "Todo") "* New [/] %?\n  %t%i\n** New sub todo1\n** New sub todo2")
          ))

;;; time grid
  (setq org-agenda-time-grid
        '((daily today require-timed)
          "----------------"
          (900 1000 1100 1200 1300 1400 1500 1600 1700)))
;;; custom commands
  (setq org-agenda-custom-commands
        '(("x" "Unscheduled TODO" tags-todo "-SCHEDULED>=\"<now>\"" nil)))
  (setq org-stuck-projects
        '("+PROJECT/-Closed-Rejected" ("TODO" "WAIT")))

  )
