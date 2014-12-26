(when (require 'google-translate nil t)

  ;; キーバインドの設定（お好みで）
  (global-set-key [(C x) (C t)] 'google-translate-at-point)

  ;; 翻訳のデフォルト値を設定（en -> ja）
  (custom-set-variables
   '(google-translate-default-source-language "en")
   '(google-translate-default-target-language "ja"))
  )
