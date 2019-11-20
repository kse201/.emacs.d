
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defvar my-config-dir (concat user-emacs-directory "conf"))
(org-babel-load-file (expand-file-name "init.org" my-config-dir))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-comphist-file
   "c:/Users/se.keisuke.G01/AppData/Roaming/.emacs.d/var/ac-comphist.dat")
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
 '(beacon-color "yellow")
 '(conda-anaconda-home "~/miniconda3")
 '(custom-safe-themes
   (quote
    ("332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" default)))
 '(dashboard-items
   (quote
    ((recents . 15)
     (projects . 5)
     (bookmarks . 5)
     (agenda))) t)
 '(dashboard-startup-banner 3 t)
 '(git-gutte+r:modified-sign "~" t)
 '(git-gutter+:added-sign "+" t)
 '(git-gutter+:deleted-sign "-" t)
 '(helm-delete-minibuffer-contents-from-point t)
 '(highlight-indent-guides-auto-enabled t)
 '(highlight-indent-guides-method (quote character))
 '(highlight-indent-guides-responsive t)
 '(imenu-list-auto-resize nil t)
 '(imenu-list-focus-after-activation t t)
 '(org-journal-date-format "%Y/%m/%d")
 '(org-journal-dir "c:/Users/se.keisuke.G01/AppData/Roaming/org/jounal")
 '(org-journal-file-format "%Y/%m/%Y-%m-%d-diary.org")
 '(package-selected-packages
   (quote
    (all-the-icons-ivy all-the-icons migemo json-mode use-package-ensure ox-gfm skk ssk ac-emoji org yaml-mode which-key use-package-ensure-system-package telephone-line tabbar rainbow-delimiters python-mode plantuml-mode org-pomodoro org-bullets neotree molokai-theme magit lsp-ui init-loader imenu-list hydra hlinum hl-todo highlight-indent-guides hide-mode-line helm-projectile git-gutter+ fill-column-indicator exec-path-from-shell editorconfig dracula-theme ddskk dashboard conda company-lsp color-identifiers-mode beacon auto-save-buffers-enhanced auto-complete)))
 '(projectile-completion-system (quote helm))
 '(projectile-git-submodule-command nil)
 '(show-paren-style (quote mixed))
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
 '(markdown-code-face ((t (:inherit default :background "#6e8fa1"))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face)))))
