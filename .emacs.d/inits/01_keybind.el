;;; 01_keybind.el -- set keybinds

;;; Commentary:

;;; Code:

;; ================================================
;; Key設定
;; ================================================
(bind-keys*
           ("C-o" . other-window)
           ("C-x o" . other-frame)
           ("C-z" . undo)
           ("C-M-z" . redo)
           ("C-/" . transpose-chars)
           )

(bind-keys :map global-map
           ("C-c s" . scheme-other-window)
           ("C-c C-x l" . px-toggle)
           ("C-h" . delete-backward-char)
           ("C-j" . newline)
           ("C-l" . (lambda () (interactive) (other-window -1)))
           ("C-m" . newline-and-indent)
           ("C-t" . forward-char)
           ("C-s" . swiper)
           ("C-x d" . dired-toggle)
           ("C-x ?" . help-command)
           ("C-x -" . split-window-below)
           ("C-x |" . split-window-right)
           ("C-x C-f" . counsel-find-file)
           ("M-t" . forward-word)
           ("M-r" . replace-string)
           ("M-x" . counsel-M-x)
           ("M-z" . transpose-words)
           )


; help-modeにevilでmodeが移り変われれば良いのでは。
; evil周りの設定がまだよくわからない。
;(bind-keys* :map help-mode-map
;           ("C-o" . other-window)
;           ("C-x o" . other-frame)
;           )
;(bind-key "C-c s" 'scheme-other-window)

;;; 01_keybind.el ends here
