;;; @ surface
;;; OS
(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-frame-position (selected-frame) 0 0)
         (setq term-setup-hook 'jbr-init)
         (setq window-setup-hook 'jbr-init))
        ((eq ws 'ns)
         ;; for MacBook Air(Late2010) 11inch display
         (set-frame-position (selected-frame) 0 0)
         (set-frame-size (selected-frame) 95 30))))
(setq scroll-preserve-screen-position nil)
(setq next-screen-context-lines 1)
;; Scroll window on a line-by-line basis
(setq scroll-conservatively 1000)
(setq scroll-step 1)
(setq scroll-margin 0) ; default=0
(setq yank-excluded-properties t)
(transient-mark-mode 1)

(setq frame-title-format "%f")
(global-linum-mode 0)
(set-face-attribute 'linum nil :foreground "red" :height 1)
(setq linum-format "%2d")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(show-paren-mode nil)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "red")
(setq show-paren-delay 0.125) ; 表示までの秒数 emacs24では0だと重い 1x http://suzukima.hatenablog.com/entry/2012/08/16/232210

(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines,%d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

(size-indication-mode t)

(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)

(display-time)
(column-number-mode t)
(line-number-mode t)
(setq inhibit-startup-echo-area-message -1)
(blink-cursor-mode 1)
(transient-mark-mode 1)
(set-scroll-bar-mode 'right); GUI emacs
(which-function-mode 1)

(auto-image-file-mode t)
(require 'whitespace nil t)
(setq whitespace-line-column 80)
(setq whitespace-style '(face
                         trailing
                         space-before-tab
                         space-after-tab))
(global-whitespace-mode 0)

(setq cursor-in-non-selected-windows nil)
(setq-default indicate-empty-lines t)
(setq isearch-lazy-highlight-initial-delay 0)
(setq initial-scratch-message "Scratch\n========\n\n")

(require 'paren nil t)
(setq show-paren-style 'mixed)
(make-face 'paren-mismatch)
(set-face-foreground 'paren-mismatch "white")
(set-face-background 'paren-mismatch "lightcoral")
;; 正規表現見やすく
(set-face-foreground 'font-lock-regexp-grouping-backslash "#66CC99")
(set-face-foreground 'font-lock-regexp-grouping-construct "#9966CC")
(setq show-paren-face  'paren-match)
(setq show-paren-mismatch-face 'paren-mismatch)

;; line-space
(setq-default line-spacing 1)
(global-set-key [f12] 'speedbar)

(defface my-hl-line-face
  '((((class clolor) (background dark))
     (:background "NavyBlue" t))
    (((class color) (background light))
     (:background "LightGoldenrodYellow" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; @ color-theme
(when (require 'color-theme nil t)
  (setq custom-theme-load-path "~/.emacs.d/themes"))

(when (require 'color-theme-molokai nil t)
  (color-theme-molokai))

;;; @ powerline
(when (require 'powerline nil t)
  (powerline-default-theme)
  )
