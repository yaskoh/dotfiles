;; use-packageが読み込まれていれば読み込まているはず。
(require 'bind-key)

;; ================================================
;; Key設定
;; ================================================
(bind-keys* 
           ("C-o" . other-window)
           ("C-x o" . other-frame)
           )

(bind-keys :map global-map
           ("C-c s" . scheme-other-window)
           ("C-c C-c" . px-toggle)
           ("C-c C-x l" . px-preview)
           ("C-h" . delete-backward-char)
           ("C-j" . newline)
           ("C-l" . (lambda () (interactive) (other-window -1)))
           ("C-m" . newline-and-indent)
           ("C-t" . forward-char)
           ("C-x d" . dired-toggle)
           ("C-x ?" . help-command)
           ("C-z" . undo)
           ("C-/" . transpose-chars)
           ("M-t" . forward-word)
           ("M-r" . replace-string)
           ("M-z" . transpose-words)
           )

; help-modeにevilでmodeが移り変われれば良いのでは。
; evil周りの設定がまだよくわからない。
;(bind-keys* :map help-mode-map
;           ("C-o" . other-window)
;           ("C-x o" . other-frame)
;           )
;(bind-key "C-c s" 'scheme-other-window)
