;;; additional elisp
(eval-when-compile (require 'cl nil t))

;; @ package
(fset 'package-desc-vers 'package--ac-desc-version)
(when
    (require 'package nil t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))
(require 'melpa nil t)
(add-hook 'emacs-startup-hook
          (lambda ()
  ;;;; (auto-install-update-emacswiki-package-name t)
            (auto-install-compatibility-setup)))

                                        ;(add-to-list 'load-path "~/src/emacswikipages/" t)

(when (require 'saveplace nil t)
  (savehist-mode t))


(when (require 'auto-async-byte-compile nil t)
;;;;   (auto-async-byte-compile-exclude-files-regexp "^#.+#")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; --------------------------------------------------
;;; @ emacs-lisp

;;; @ autoinsert
(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/templete")
(define-auto-insert "\\.rb$" "template.rb")

;;; @ minor-mode-hack
(require 'minor-mode-hack nil t)

;;; http://e-arrows.sakura.ne.jp/2010/02/vim-to-emacs.html
;; org-mode
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

;;; @ color-moccur
(when (require 'color-moccur nil t)
  (global-set-key (kbd "M-s") 'occur-by-moccur)
  (setq moccur-split-word t)
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$")
  (when (and (executable-find "cmigemo")
             (require 'migemo nil t ))
    (setq cmigemo-command "cmigemo") ; cmigemoを使う
    (setq moccur-use-migemo t)))

;;; @ color-grep
(when (require 'color-grep nil t)
  (setq color-grep-sync-kill-buffer t)
  (setq grep-find-command "ack --nocolor --nogroup "))

;;; @ undohist
(when (require 'undohist nil t)
  (undohist-initialize))

;;; @ undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;; @ screen-lines
(when (require 'screen-lines nil t)
  (add-hook 'text-mode-hook 'turn-on-screen-lines-mode))

;;; @ text-adjust
(require 'text-adjust nil t)

;;; @ multi-shell
(when (require 'multi-shell nil t)
  (setq multi-shell-command "/bin/zsh"))

;; @ redo+
(when (require 'redo+ nil t)
  (global-set-key (kbd  "C-.") 'redo)
  (setq undo-no-redo t)
  (setq undo-limit 60000)
  (setq undo-strong-limit 90000))

(when (require 'eldoc nil t)
  (require 'eldoc-extension nil t)
  (defun elisp-mode-hooks ()
    ;;  "lisp-mode-hooks"
    (setq eldoc-idle-delay 0.5)
    (setq eldoc-area-use-multiline-p t)
    (turn-on-eldoc-mode))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.5)
  (setq eldoc-minor-mode-string "")
  (add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks))

;;;  @ C-eldoc
(when (require 'c-eldoc nil t)
  (add-hook 'c-mode-hook
            (lambda ()
              (set (make-local-variable 'eldoc-idle-delay) 0.5)
              (set (make-local-variable 'eldoc-minor-mode-string) "")
              (c-turn-on-eldoc-mode))))

(require 'hideshow nil t)

;; @ e2wm
(when (require 'e2wm nil t)
  (global-set-key (kbd "M-+") 'e2wm:start-management)
  (setq e2wm:c-code-recipe
        '(| (:left-max-size 12)
            (- (:upper-size-ratio 0.7)
               files history)
            (- (:upper-size-ratio 0.7)
               (| (:right-max-size 12)
                  main imenu)
               sub)))
  (setq e2wm:c-code-winfo
        '((:name main)
          (:name files :plugin files)
          (:name history :plugin history-list)
          (:name sub :buffer "*info*" :default-hide t)
          (:name imenu :plugin imenu :default-hide nil))))

;; @ w3m
(when (and (executable-find "w3m")
           (require 'w3m nil t))
  (require 'w3m-load nil t)
  (setq w3m-use-cookies t)
  (setq browse-url-browser-function 'w3m-browse-url)
  (setq w3m-key-binding 'info)
                                        ;(global-set-key (kbd "C-x C-b") 'bs-show)
  (global-set-key "\C-xm" 'browse-url-at-point)
  (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)
  (setq w3m-search-default-engine "google")
  (global-set-key "\C-cs" 'w3m-search)
  (autoload 'w3m-weather "w3m-weather" "Display weather report." t)
  (autoload 'w3m-antenna "w3m-antenna" "Report chenge of WEB sites." t)
  (autoload 'w3m-namazu "w3m-namazu" "Search files with Namazu." t)
                                        ;formに入力可能とする。今は不要かもしれない
  (setq w3m-use-form t)
                                        ;うまく起動しない場合以下を設定してみるとよい
                                        ;(setq w3m-command "/usr/local/bin/w3m")
                                        ;初期起動時に表示する画面
  (setq w3m-home-page "http://www.google.co.jp")
  (setq w3m-default-display-inline-images t)       ;画像を表示する

  ;; http://mugijiru.seesaa.net/article/258382587.html
  (defun w3m-play-movie ()
    (interactive)
    (let ((url (w3m-anchor)))
      (cond ((string-match "^http:\\/\\/www\\.youtube\\.com" url)
             (message (concat "loading from youtube... " url))
             (call-process "play-youtube" nil nil nil url))
            ((string-match (concat "^http.*" (regexp-opt '(".mpg" ".wmv" ".avi" ".flv")) "$") url)
             (call-process "mplayer" nil nil nil "-fs" url))
            (t
             (message "not movie."))))))

;;; ruby
(require 'ruby-electric nil t)          ; 括弧の自動挿入
(when (require 'ruby-block nil t)       ; end に対応する行のハイライト
  (setq ruby-block-highlight nil))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")


;; http://tcnksm.sakura.ne.jp/blog/2012/05/07/emacs-%E3%81%A7-ruby-%E3%81%AE%E5%85%A5%E5%8A%9B%E8%87%AA%E5%8B%95%E8%A3%9C%E5%AE%8C%E3%81%A8%E3%83%AA%E3%83%95%E3%82%A1%E3%83%AC%E3%83%B3%E3%82%B9%E3%81%AE%E8%A1%A8%E7%A4%BA/
(when (require 'rsense nil t)
 (setq rsense-home "~/.emacs.d/opt/rsense-0.3")
 (add-to-list 'load-path (concat rsense-home "/etc"))
 (add-hook 'ruby-mode-hook
           '(lambda ()
              (add-to-list 'ac-sources 'ac-source-rsense-method)
              (add-to-list 'ac-sources 'ac-source-rsense-constant)
              (define-key ruby-mode-map (kbd "C-x .") 'ac-complete-rsense)))
 
 (defun ruby-mode-hooks ()
   (inf-ruby-keys)
   (ruby-electric-mode t)
   (ruby-block-mode t))
 (add-hook 'ruby-mode-hook 'ruby-mode-hooks) ; ruby-mode-hookに追加
)
;; recursive grep
(when (require 'grep nil t)
  (setq grep-command-before-query "grep -nH -r -e ")
  (defun grep-default-command ()
    (if current-prefix-arg
        (let ((grep-command-before-target
               (concat grep-command-before-query
                       (shell-quote-argument (grep-tag-default)))))
          (cons (if buffer-file-name
                    (concat grep-command-before-target
                            " *."
                            (file-name-extension buffer-file-name))
                  (concat grep-command-before-target " ."))
                (+ (length grep-command-before-target) 1)))
      (car grep-command)))
  (setq grep-command (cons (concat grep-command-before-query " .")
                           (+ (length grep-command-before-query) 1)))
)

;; unique filename
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

(when (require 'navi2ch nil t)
  (setq navi2ch-article-exist-message-range '(1 . 1000))
  (setq navi2ch-article-new-message-range '(1000 . 1))
  (setq  navi-board-insert-subject-with-diff t)
  (setq navi2ch-board-insert-subject-with-unread t)
  (setq navi2ch-list-init-open-category nil)
  (setq navi2ch-board-expire-date nil)
  (setq navi2ch-history-max-line nil))

;; @ paredit.el
(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'c-mode-hook 'enable-paredit-mode))

;; @ newsticker
(when (require 'newsticker nil t)
  (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
  (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t))

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
(when (require 'stripe-buffer nil t)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode))
;; @ C/C++
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-toggle-auto-hungry-state 1)
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

(defadvice kill-line (before kill-line-and-fixup activate)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

;; @ egg
(when (executable-find "git")
  (require 'egg nil t))

;; @ time-stamp
(when (require 'time-stamp nil t)
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t)
  (setq time-stamp-start "Last Change: " )
  (setq time-stamp-format "%02d-%3b-%04y.")
  (setq time-stamp-end " \\|$"))

;; ブロックの折畳みと展開
;; http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el
(when (require 'fold-dwim nil t)
  (require 'hideshow nil t)
  ;; 機能を利用するメジャーモード一覧
  (let ((hook))
    (dolist (hook
             '(emacs-lisp-mode-hook
               c-mode-common-hook
               python-mode-hook
               php-mode-hook
               ruby-mode-hook
               js2-mode-hook
               css-mode-hook
               apples-mode-hook))
      (add-hook hook 'hs-minor-mode))))

;; @Emacsのcalendarで日本の祝日を表示する
;; http://qiita.com/aprikip@github/items/db350720bb32e244daea
(when (require 'solar nil t)
(setq holiday-general-holidays nil
      holiday-local-holidays t
      holiday-solar-holidays nil
      holiday-bahai-holidays nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-other-holidays nil
      mark-holidays-in-calendar t)
(setq holiday-local-holidays
      '((holiday-fixed 1 1 "元日")
    (holiday-float 1 1 2 "成人の日")
    (holiday-fixed 2 11 "建国記念の日")
    (holiday-sexp '(map 'list 'truncate (solar-equinoxes/solstices 0 year)) "春分の日")
    (holiday-fixed 4 29 "昭和の日")
    (holiday-fixed 5 3 "憲法記念日")
    (holiday-fixed 5 4 "みどりの日")
    (holiday-fixed 5 5 "こどもの日")
    (holiday-float 7 1 3 "海の日")
    (holiday-float 7 1 3 "敬老の日")
    (holiday-sexp '(map 'list 'truncate (solar-equinoxes/solstices 2 year)) "秋分の日")
    (holiday-float 10 1 2 "体育の日")
    (holiday-fixed 11 3 "文化の日")
    (holiday-fixed 11 23 "勤労感謝の日")
    (holiday-fixed 12 23 "天皇誕生日")
)))

(when (require 'git-gutter nil t))

(when (require 'saveplace nil t)
 (setq-default save-place t)
 (setq save-place-file "~/.saved-places") )

;;; @SKK
(when (require 'skk nil t)
  (define-many-keys global-map
    '(("C-x C-j" . skk-mode)
      ("C-x j" . skk-auto-fill-mode)
      ("C-x t" . skk-tutorial)
      ))
  (setq skk-use-azik t)
  ;; input Hankaku-Kana
  (setq skk-use-jisx0201-input-method t)
  ;; dont CR in Enter
  (setq skk-egg-like-newline t)
  ;; complete "「"
  (setq skk-auto-insert-paren t)
  ;; dot rule
  (setq skk-kuten-touten-alist
        '(
          (jp . ("。" . "、" ))
          (en . ("." . ","))
          ))

  (setq skk-henkan-strict-okuri-precedence t)
  (setq skk-share-private-jisyo t)
  )
;;; @ ido
(ido-mode 1)
(ido-everywhere 1)

;;; @ bs-show
(global-set-key (kbd "C-x C-b") 'bs-show)

;;; @ open-junk-file
(when (require 'open-junk-file nil t)
  (setq open-junk-file-format "~/howm/%Y/%m/%Y-%m-%d-%H%M%S.txt")
  (global-set-key (kbd "C-x j") 'open-junk-file)
  )

;;; @ multiple-cursors

(when (require 'multiple-cursors nil t)
  
  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
  
  (define-many-keys global-map
    '(("C-t C-t"      . mc/mark-next-like-this)
      ("C-t n"        . mc/mark-next-like-this)
      ("C-t p"        . mc/mark-previous-like-this)
      ("C-t m"        . mc/mark-more-like-this-extended)
      ("C-t u"        . mc/unmark-next-like-this)
      ("C-t U"        . mc/unmark-previous-like-this)
      ("C-t s"        . mc/skip-to-next-like-this)
      ("C-t S"        . mc/skip-to-previous-like-this)
      ("C-t *"        . mc/mark-all-like-this)
      ("C-t d"        . mc/mark-all-like-this-dwim)
      ("C-t i"        . mc/insert-numbers)
      ("C-t o"        . mc/sort-regions)
      ("C-t O"        . mc/reverse-regions))))

(when (require 'real-auto-save nil t)
  (setq real-auto-save-interval 5)        ;3秒後に自動保存
  ;; (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (add-hook 'find-file-hook 'real-auto-save-mode))

(when (require 'persp-mode nil t)
  (setq persp-keymap-prefix (kbd "C-c p")) ;prefix
  (setq persp-add-on-switch-or-display t) ;バッファを切り替えたら見えるようにする
  (persp-mode 1)
;  (defun persp-register-buffers-on-create ()
;    (interactive)
;    (dolist (bufname (condition-case _
;                       (helm-comp-read
;                        "Buffers: "
;                        (mapcar 'buffer-name (buffer-list))
;                        :must-match t
;                        :marked-candidates t)
;                     (quit nil)))
;    (persp-add-buffer (get-buffer bufname))))
;  (add-hook 'persp-activated-hook 'persp-register-buffers-on-create)
  )
