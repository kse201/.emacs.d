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

;;; @ minor-mode-hack
;;; マイナーモード衝突問題を解決する
(require 'minor-mode-hack nil t)

;;; http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;; org-mode
;; Emacsでメモ・TODO管理
(when  (require 'org-install nil t)
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cr" 'org-remember)
  (setq org-startup-truncated nil)
  (setq org-return-follows-link t)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-directory "~/work/memo/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-agenda-files (concat org-directory "notes.org"))
  (setq org-remember-templates
        '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
          ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
          ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas"))))

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

;; @ popwin
(setq pop-up-windows t)
(when (require 'popwin nil t)
  (setq anything-samewindow nil)
  (setq display-buffer-function 'popwin:display-buffer)
  (setq popwin:popup-window-height 0.2)
  (setq popwin:popup-window-position 'bottom)
  ;; フレームのサイズに応じてpopwinの出現位置を決める
  ;; http://qiita.com/items/7f9fe4abac5044025e0f
(defun popwin-auto-set-popup-window-position ()
  (interactive)
  (let ((w (frame-width))
        (h (frame-height)))
    (setq popwin:popup-window-position
          (if (and (< 200 w)         ; フレームの幅が200桁より大きくて
                   (< h w))          ; 横長の時に
              'right                 ; 右へ出す
            'bottom))))              ; そうじゃないときは下へ出す
;; popwin表示時にフレームサイズに応じた表示位置にする
(defadvice  popwin:display-buffer (before popwin-auto-window-position activate)
  (popwin-auto-set-popup-window-position))
  (push '("anything" :regexp t :height 40) popwin:special-display-config ) ;anything
  (push '("*Completions*" ) popwin:special-display-config )
  (push '("*complilation*" :noselect t :stick t ) popwin:special-display-config )
  (push '(dired-mode :position top) popwin:special-display-config ) ;dired
  (push '("*Backtrace*" :noselect t) popwin:special-display-config )
  (push '(fundamental-mode  :noselect t) popwin:special-display-config )
  (push '(typeset-mode :noselect t) popwin:special-display-config )
  (push '(" *auto-async-byte-compile*"  :position bottom :noselect t :height 0.3 :stick nil) popwin:special-display-config )
  (push '("*YaTeX-typesetting*" :position bottom :noselect t) popwin:special-display-config )
  (push '("*VC-log*" :position bottom) popwin:special-display-config )
  (push '("\\*.*\\.po\\*"        :regexp t        :position bottom        :height 20)      popwin:special-display-config)
  )

;; @ popwin:select-popup-window
(when (require 'popup nil t )
  (require 'popup-select-window nil t)
  (global-set-key "\C-xo" 'popup-select-window)
  (setq popup-select-window-popup-windows 2))

(when (require 'git-gutter nil t))

;;; @ stripe-buffer
(add-hook 'dired-mode-hook 'stripe-listify-buffer)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
