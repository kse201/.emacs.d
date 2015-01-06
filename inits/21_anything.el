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
    (anything-completion-mode)
    ;; key-bind
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
        ("C-M-p" . anything-previous-source)))))
