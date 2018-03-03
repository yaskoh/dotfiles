;; ================================================
;; Macでのキーボード配置を変更
;; ================================================
(when (eq system-type 'darwin)
  ;(setq mac-control-modifier 'control)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  )

;; ================================================
;; Macでのフォント設定を変更
;; 2014/4/13
;; ================================================
(when (eq system-type 'darwin)
  ;English
  (set-face-attribute 'default nil
                      :family "Inconsolata")
  ;日本語
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "TakaoExGothic"))
  )
