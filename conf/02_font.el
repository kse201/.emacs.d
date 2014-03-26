(let ((ws window-system))
  (cond ((eq system-type 'windows-nt)
         (set-face-attribute 'default nil
                             :family "Ricty Diminished"  ;; 英数
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))  ;; 日本語
        ((eq system-type 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty Diminished"  ;; 英数
                             :height 160)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))))  ;; 日本語
