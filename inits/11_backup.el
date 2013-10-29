;;; @ backup

;; backup autosave
;; 変更ファイルのバックアップ
;(require 'auto-save-buffers)
;(run-with-idle-timer 5 t 'auto-save-buffers)
;; http://d.hatena.ne.jp/tomoya/20110217/1297928222
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  (require 'recentf-ext))

(setq make-backup-files nil)
(setq auto-save-default nil)
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" , (expand-file-name "~/.emacs.d/backups/") t)))
;; 変更ファイルの番号つきバックアップ
(setq version-control t)
;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; バックアップ世代数
(setq kept-old-versions 1)
(setq kept-new-versions 2)
;;; 確認せず古いものを消す
(setq delete-old-versions t)
;; ブックマーク
(setq bookmark-save-flag 1)
(setq bookmark-sort-flag nil)
(defun bookmark-arrange-latest-top ()
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (cons latest (delq latest bookmark-alist))))
  (bookmark-save))
(add-hook 'bookmark-after-jump-hook 'bookmark-arrange-latest-top)
