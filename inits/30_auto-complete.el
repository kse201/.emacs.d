;;; @ auto-complete
(when (require 'auto-complete nil t)
  (require 'popup nil t)
  (require 'fuzzy nil t)
  (require 'auto-complete-config nil t)
  (setq popup-use-optimized-column-computation nil)
  (setq ac-ignore-case 'smart)               ; 大文字が含まれている場合のみ、大文字/小文字を区別する
  ;;(ac-set-trigger-key "TAB")
  (global-set-key (kbd "TAB") 'auto-complete)
  (setq ac-use-menu-map t)
  (setq ac-auto-start t) 
  (global-auto-complete-mode t)
  (setq ac-modes (cons 'js-mode ac-modes))
  ;; emacs-lisp-modeでEmacs Lispシンボルも補完してくれるようにする
  (defun emacs-lisp-ac-setup ()
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-symbols)))
  (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-ac-setup)
      (add-to-list 'ac-dictionary-directories "~/.emacd.d/ac-dict")
    (setq ac-ignore-case t)
    (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
    (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
    (auto-complete-mode 1)
                                        ; Auto Complete でコメントや文字列リテラルでも補完を行う
                                        ; http://qiita.com/items/87514346bf1c008f1ef6
  (setq ac-disable-faces nil)
                                        ; Auto Complete でファイル名の入力が上手くいかない問題を解決する
                                        ; http://qiita.com/items/c905dbaa8b2ffa56b9e0
  ;;; パス入力用の情報源を追加
  (defun ac-common-setup ()
    (add-to-list 'ac-sources 'ac-source-filename))

  ;;; ac-source-filename は ac-sources の先頭でないと "~/" が上手く補完できないため，
  ;;; ac-sources の後ろに mode-hook 用情報源を付け足すよう変更
  (defun ac-emacs-lisp-mode-setup ()
    (setq ac-sources (append ac-sources '(ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols))))

  (defun ac-cc-mode-setup ()
    (setq ac-sources (append ac-sources '(ac-source-yasnippet ac-source-gtags))))

  (defun ac-css-mode-setup ()
    (setq ac-sources (append ac-sources '(ac-source-css-property))))
  
  ;; http://d.hatena.ne.jp/syohex/20121225/1356449561
;; look command with auto-complete
(defun my/ac-look ()
  "`look' command with auto-completelook"
  (interactive)
  (unless (executable-find "look")
    (error "Please install `look' command"))
  (let ((word (thing-at-point 'word)))
    (unless word
      (error "not found word"))
    (let ((cmd (format "look %s" word)))
      (with-temp-buffer
        (call-process-shell-command cmd nil t)
        (split-string-and-unquote (buffer-string) "\n")))))

(defun ac-look ()
  (interactive)
  (let ((ac-menu-height 50)
        (ac-candidate-limit t))
    (auto-complete '(ac-source-look))))

(defvar ac-source-look
  '((candidates . my/ac-look)
    (requires . 2)))

(global-set-key (kbd "C-M-l") 'ac-look) ;; 好きなキーにしてください


  )
