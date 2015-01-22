;;; @ keybind
(when (eq system-type 'darwin)
  ;;(setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  ;;  (setq mac-command-modifier 'super)
  (setq mac-pass-control-to-system t))
(define-many-keys global-map
  '(("C-h" . delete-backward-char)
    ("<f1>" . help-for-help)
    ("C-c i" . indent-region )
    ("C-c C-i" . dabbrev-expand )
    ("C-c );" . comment-region )
    ("C-c :" . uncomment-region )
    ("C-\\" . nil )
    ("C-m" . newline-and-indent)
    ("C-c l" . toggle-truncate-lines)
    ("C-t" . nil)
    ;;    ("C-x C-o" . my-other-window)
    ("M-y" . backward-kill-word )
    ("C-x o" . browse-url-at-point )
    ("C-x C-g" . goto-line )
    ;; window-move
    ("C-x w h" . windmove-left)
    ("C-x w j" . windmove-down)
    ("C-x w k" . windmove-up)
    ("C-x w l" . windmove-right)
    ;; window-split
    ("C-x SPC" . good-split-window)
    ("C-x -" . split-window-vertically)
    ("C-x |" . split-window-horizontally)
    ("C-c C-@" . move-to-mark)
    ("C-c C-e" . edit-init)
    ("C-x C-z" . nil)
    ("C-a" . my-beginning-of-indented-line)
    ("M-p" . scroll-down)
    ("M-n" . scroll-up)
    ("M-g" . goto-line)
    ("C-M-h" . delete-horizontal-space)
    ("M-f" . forward-word)
    ))

; http://ainame.hateblo.jp/entry/2013/11/04/015107
(defun newline-or-open-line ()
  "newline-or-openline is a new command for merging C-m and C-o"
  (interactive)
  (let ((string-exists-before-cursor (string-match "[^\\\s\\\n\\\t]" (buffer-substring (point-at-bol) (point))))
        (string-exists-after-cursor (string-match "[^\\\s\\\n\\\t]" (buffer-substring (point) (point-at-eol)))))
    (cond ((or (eolp)
               (not string-exists-after-cursor)
               (and string-exists-before-cursor string-exists-after-cursor))
	   (progn (newline) (indent-according-to-mode)))
          (t (progn (open-line 1) (indent-according-to-mode))))))
 
(define-key global-map (kbd "C-m") 'newline-or-open-line)

(windmove-default-keybindings)
(define-key mode-specific-map "c" 'compile)
(defalias 'exit 'save-buffers-kill-emacs)

;;http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part16
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))

;; minibuffer
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

(setq kill-whole-line t)
(iswitchb-mode 1)
(if (string-match "^23\." emacs-version)
    (iswitchb-default-keybindings))
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)
;;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)
