
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
 '(conda-anaconda-home "~/miniconda3")
 '(package-selected-packages
   (quote
    (ox-gfm skk ssk ac-emoji org yaml-mode which-key use-package-ensure-system-package telephone-line tabbar rainbow-delimiters python-mode plantuml-mode org-pomodoro org-bullets neotree molokai-theme magit lsp-ui init-loader imenu-list hydra hlinum hl-todo highlight-indent-guides hide-mode-line helm-projectile git-gutter+ fill-column-indicator exec-path-from-shell editorconfig dracula-theme ddskk dashboard conda company-lsp color-identifiers-mode beacon auto-save-buffers-enhanced auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c")))))
