;;; 30_el-get.el ---                                 -*- lexical-binding: t; -*-

;;; el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; define el-get repo
(setq el-get-sources
      '(
        (:name open-github-from-here
               :type github
               :description "open github from here"
               :pkgname "shibayu36/emacs-open-github-from-here"
               :branch "development")
        (:name anything-git-files
               :type github
               :pkgname "tarao/anything-git-files-el"
               :depends anything)
        (:name popup ;; <- 追加
               :type github
               :website "https://github.com/auto-complete/popup-el"
               :description "[My Recipes] This section describes the basic data structures and operations of popups."
               :pkgname "auto-complete/popup-el")
        (:name auto-complete ;; <- 追加
               :type github
               :website "https://github.com/auto-complete/auto-complete"
               :description "[My Recipes] Auto Complete Mode renews an old completion interface and provides an environment that makes users could be more concentrate on their own works."
               :pkgname "auto-complete/auto-complete")
        (:name powerline
               :website "https://github.com/milkypostman/powerline"
               :depends (cl-lib)
               :description "Powerline for Emacs"
               :type github
               :pkgname "milkypostman/powerline"
               :load-path "."
               :features powerline)
        ))


;; auto install el-get.el
(defvar my/el-get-packages
  '(
    open-github-from-here
    )
  "A list of packages to install from el-get at launch.")

(el-get 'sync)
