;;; 追加elisp関連
(eval-when-compile (require 'cl nil t))
(require 'cl nil t)
;; @ package
(when
    (if (string-match "^23\." emacs-version)
        (require 'package-23 nil t)
      (require 'package-24 nil t))

  ;; バッケージリポジトリにMarmaladeと開発者運営のELPAを追加
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))
(require 'melpa)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; 起動時にEmacsWikiのページ名を補完候補に加える
            ;;;; (auto-install-update-emacswiki-package-name t)
            (auto-install-compatibility-setup)))

                                        ;(add-to-list 'load-path "~/src/emacswikipages/" t)

(when (require 'auto-async-byte-compile nil t)
  ;; 自動バイトコンパイルを無効にするファイル名の正規表現
;;;;   (auto-async-byte-compile-exclude-files-regexp "^#.+#")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; --------------------------------------------------
;;; @ emacs-lisp

;;; @ autoinsert
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/templete")
(define-auto-insert "\\.rb$" "template.rb")

;; org-mode
(require 'org-install nil t)

 ;; @ rainbow-delimiters
(when (require 'rainbow-delimiters nil t)
  (global-rainbow-delimiters-mode t)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode))

;; @ smartchr.el
;; http://tech.kayac.com/archive/emacs-tips-smartchr.html
(when(require 'smartchr nil t)
     (global-set-key (kbd "=") (smartchr '(" = "  "== " "="))))

(when (require 'git-gutter nil t))

;;; @ stripe-buffer
(require 'stripe-buffer)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
