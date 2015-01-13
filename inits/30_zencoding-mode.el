;; zencoding-mode
(when (require 'zencoding-mode nil t)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-i") 'zencoding-expand-line))
