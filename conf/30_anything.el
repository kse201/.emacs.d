;; @ anything
(when (require 'anything nil t)
  ;; tuning
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 10000
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)
  
  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo")
    ;; anything関連キーバインド
    (define-many-keys global-map
      '(( "M-y" . anything-show-kill-ring)
        ( "C-x C-:" . anything-M-x)
        ( "C-x b" . anything-for-files)
        ( "C-x C-f" . anything-find-files)
        ("C-x g" . anything-imenu)
        ;("TAB" . anything-lisp-completion-at-point-or-indent)
        ("C-M-z" . anything-resume)
        ("C-x C-o" . other-window)))
    (define-many-keys anything-map
      '(("C-z" . nil)
        ("C-w" . anything-execute-persistent-action)
        ("C-o" . nil)
        ("C-M-n" . anything-next-source)
        ("C-M-p" . anything-previous-source)))

    (require 'anything-match-plugin nil t)
    (defun anything-custom-filelist ()
      (interactive)
      (anything-other-buffer
       (append
        '(anything-c-source-ffap-line
          anything-c-source-ffap-guesser
          anything-c-source-buffers+
          )
        (anything-c-sources-git-project-for)
        '(anything-c-source-recentf
          anything-c-source-bookmarks
          anything-c-source-file-cache
          anything-c-source-filelist
          ))
       "*anything file list*"))
    )

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))
  (require 'anything-show-completion nil t)
  ;; ajc-java-complete.el
  (when (require 'ajc-java-complete-config nil t)
    (add-hook 'java-mode-hook 'ajc-java-complete-mode))

  (when (require 'anything-c-moccur nil t)
    (setq
     anything-c-moccur-anything-idle-delay 0.1
     anything-c-moccur-higligt-info-line-flag t ; バッファの情報をハイライトする
     anything-c-moccur-enable-auto-look-flag t  ;選択中の候補の位置を他のwindowに表示する
     anything-c-moccur-enable-initial-pattern nil) ;起動時にポイントの位置の単語を初期パターンにしない
    ;; C-M-o にanything-c-moccur-occur-by-moccurを割り当てる
    (global-set-key (kbd "C-M-o") 'anything-c-moccur-occur-by-moccur)

    ;; http://d.hatena.ne.jp/kitokitoki/20100626/p1
    (defvar anything-cycle-task-count 0)

    ;; fixme-mode.el と ik:anything-cycle-pattern を参考にした
    (defun anything-cycle-task ()
          ;; 全バッファからTODO等を一覧表示する
      (interactive)
      (let ((los '("\\_<todo\\_>"
                   "\\_<kludge\\_>"
                   "\\_<fixme\\_>"
                   "\\_<bug\\_>"
                   "\\_<todo\\_>\\|\\_<fixme\\_>\\|\\_<bug\\_>\\|\\_<kludge\\_>")))
        (if (eq this-command real-last-command)
            (incf anything-cycle-task-count)
          (setq anything-cycle-task-count 0))
        (when (>= anything-cycle-task-count (length los))
          (setq anything-cycle-task-count 0))
        (delete-minibuffer-contents)
        (let ((sep (nth anything-cycle-task-count los)))
          (insert sep))))
    ;; anythign-c-moccur-buffer-list 中に T で切り替え
    (define-key anything-c-moccur-anything-map (kbd "T") 'anything-cycle-task)
    )
  ;; @ anything-auto-install
  (require 'anything-auto-install nil t)
  
  ;; @ anything-include
  (when (require 'anything-include nil t)
    (setq anything-sources
          (list anything-c-source-buffers
                anything-c-source-files-in-current-dir
                anything-c-source-recentf
                anything-c-source-emacs-commands
                anything-c-source-info-pages
                anything-c-source-include))
    (setq anything-include-save-file "~/.anything-include")
    (setq anything-include-max-saved-items 100))
  
  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 300)
    (anything-read-string-mode 1)
    )

  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install))
  
  ;; man パスを設定
  (setq woman-manpath '("/usr/share/man"
                        "/usr/loca/share/man"
                        "/usr/local/share/man/ja"
                        "/usr/local/Cellar/"))
  (setq woman-cache-filename "~/.emacs.d/.wmncach.el") ;キャッシュ生成
  )

(add-to-list 'anything-sources 'anything-c-source-emacs-commands)

