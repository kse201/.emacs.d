(let ((ws window-system))
  (cond ((eq system-type 'windows-nt)
         (set-face-attribute 'default nil
                             :family "Ricty Diminished"
                             :height 140)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))
        ((eq system-type 'ns)
         (set-face-attribute 'default nil
                             :family "Ricty Diminished"
                             :height 160)
         (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished")))))
