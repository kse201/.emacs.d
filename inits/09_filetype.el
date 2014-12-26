;;; @ Lang
(add-hook
 'c-mode-common-hook
 (lambda ()
   (c-set-style "bsd")
   (setq indent-tabs-mode nil)
   (setq c-basic-offset 4)
   (c-toggle-auto-hungry-state 1)
   (subword-mode 1)))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   
   (setq indent-tabs-mode nil)))
(add-hook 'c-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'c++-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))
(add-hook 'python-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(add-hook 'java-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))))

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq indent-tabs-mode nil)))

;;  markdown
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
;; See docs
;(define-key anything-map [(control ?;)] 'anything-execute-persistent-action)
  )
(when (require 'smart-complie nil t)
  (define-key ruby-mode-map (kbd "C-c c") 'smart-complie)
  (define-key ruby-mode-map (kbd "C-c C-c") '( kbd "C-c C-m"))
  )

