;; 注：読み込みを後半にすると、カーソルなどの設定が適切に行われない。

;; ================================================
;; カラーテーマを設定
;; ================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes/zenburn")
(load-theme 'zenburn t)


;; ;; カーソルの色
;; (add-to-list 'default-frame-alist '(cursor-color . "white"))
;; ;; マウスポインタの色
;; (add-to-list 'default-frame-alist '(mouse-color . "whit"))
;; ;; 選択中のリージョンの色を設定します。
;; (set-face-background 'region "LightSteelBlue1")

;; ;; モードラインの文字色
;; (set-face-foreground 'mode-line "white")
;; ;; モードラインの背景色
;; (set-face-background 'mode-line "MediumPurple2")
;; ;; モードラインの文字色(inactive)
;; (set-face-foreground 'mode-line-inactive "gray30")
;; ;; モードラインの背景色(inactive)
;; (set-face-background 'mode-line-inactive "gray85")
