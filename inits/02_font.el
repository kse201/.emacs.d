(let ((ws window-system))
  (cond ((eq ws 'w32)
         (set-face-attribute 'default nil
                             :family "Ricty Diminished"  ;; 英数
                             :height 100)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))  ;; 日本語
        ((eq ws 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty Diminished"  ;; 英数
                             :height 160)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))))  ;; 日本語
