(defvar my-config-dir (concat user-emacs-directory "conf.org.d"))
(org-babel-load-file (expand-file-name "init.org" my-config-dir))
