;;; @ 各種言語別設定
(add-hook
 'c-mode-common-hook
 (lambda ()
   ;; BSDスタイルをベースにする
   (c-set-style "bsd")
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)
   (setq c-basic-offset 4)
   ;; 自動改行（auto-new-line）と
   ;; 連続する空白の一括削除（hungry-delete）を
   ;; 有効にする
   (c-toggle-auto-hungry-state 1)
   ;; CamelCaseの語でも単語単位に分解して編集する
   ;; GtkWindow         => Gtk Window
   ;; EmacsFrameClass   => Emacs Frame Class
   ;; NSGraphicsContext => NS Graphics Context
   (subword-mode 1)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   ;; スペースでインデントをする
   (setq indent-tabs-mode nil)))
;; http://qiita.com/items/b836e7792be0a7c65fd4
;; C系統,Pythonにて1行80文字を超えるとハイライト
(add-hook 'c-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
;; @ C/C++
(add-hook 'c-mode-common-hook
          '(lambda ()
             ;; センテンスの終了である ';' を入力したら、自動改行+インデント
             (c-toggle-auto-hungry-state 1)
             ;; RET キーで自動改行+インデント
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

(add-hook 'c++-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;; Javaで1行100文字を超えるとハイライト
(add-hook 'java-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

;; スクリプト保存時、自動的にchmod+x
;;; ファイル先頭に#!が含まれているとき
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; emacs-lisp mode
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq indent-tabs-mode nil)))

;;  markdown
;; from http://support.markedapp.com/kb/how-to-tips-and-tricks/marked-bonus-pack-scripts-commands-and-bundles
 
(defun markdown-preview-file ()
  "run Marked on the current file and revert the buffer"
  (interactive)
  (shell-command
   (format "open -a /Applications/Marked.app %s"
       (shell-quote-argument (buffer-file-name))))
)
(add-hook 'markdown-mode
          (lambda ()
            (global-set-key "\C-cc" 'markdown-preview-file)))

;; Ruby
;; http://shibayu36.hatenablog.com/entry/2013/03/18/192651
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq ruby-indent-level tab-width)
             (setq ruby-deep-indent-paren-style nil)
             (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)))
(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))
(when (require 'ruby-electric nil t)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil))
(when (require 'rcodetools nil t)
  (setq rct-find-tag-if-available nil)
  (defun ruby-mode-hook-rcodetools ()
    (define-key ruby-mode-map "\M-\C-i" 'rct-complete-symbol)
  (define-key ruby-mode-map "\C-c\C-t" 'ruby-toggle-buffer)
  (define-key ruby-mode-map "\C-c\C-d" 'xmp)
  (define-key ruby-mode-map "\C-c\C-f" 'rct-ri))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-rcodetools)
(require 'anything-rcodetools)
(setq rct-get-all-methods-command "PAGER=cat fri -l")
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
;; See docs
;(define-key anything-map [(control ?;)] 'anything-execute-persistent-action)
  )
;; http://tcnksm.sakura.ne.jp/blog/2012/05/07/emacs-%E3%81%A7-ruby-%E3%81%AE%E5%85%A5%E5%8A%9B%E8%87%AA%E5%8B%95%E8%A3%9C%E5%AE%8C%E3%81%A8%E3%83%AA%E3%83%95%E3%82%A1%E3%83%AC%E3%83%B3%E3%82%B9%E3%81%AE%E8%A1%A8%E7%A4%BA/
(when (require 'rsense nil t)
 (setq rsense-home "~/.emacs.d/opt/rsense-0.3")
 (add-to-list 'load-path (concat rsense-home "/etc"))
 (add-hook 'ruby-mode-hook
           '(lambda ()
              ;; .や::を入力直後から補完開始
              (add-to-list 'ac-sources 'ac-source-rsense-method)
              (add-to-list 'ac-sources 'ac-source-rsense-constant)
              ;; C-x .で補完出来るようキーを設定
              (define-key ruby-mode-map (kbd "C-x .") 'ac-complete-rsense)))

 ;; ruby-mode-hook用の関数を定義
 (defun ruby-mode-hooks ()
   (inf-ruby-keys)
   (ruby-electric-mode t)
   (ruby-block-mode t))
 (add-hook 'ruby-mode-hook 'ruby-mode-hooks) ; ruby-mode-hookに追加
)
(when (require 'smart-complie nil t)
  (define-key ruby-mode-map (kbd "C-c c") 'smart-complie)
  (define-key ruby-mode-map (kbd "C-c C-c") '( kbd "C-c C-m"))
  )

