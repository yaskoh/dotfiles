;; use-packageが読み込まれていれば読み込まているはず。
(require 'bind-key)

;; ================================================
;; Key設定
;; ================================================
(bind-key* "C-o" 'other-window)
(bind-key* "C-x o" 'other-frame)

(bind-key "C-c s" 'scheme-other-window)
(bind-key "C-c C-c" 'px-toggle)
(bind-key "C-c C-x l" 'px-preview)
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-j" 'newline)
(bind-key "C-l" 'evil-normal-state)
(bind-key "C-m" 'newline-and-indent)
(bind-key "C-t" 'forward-char)
(bind-key "C-x d" 'dired-toggle)
(bind-key "C-x ?" 'help-command)
(bind-key "C-z" 'undo)
(bind-key "C-/" 'transpose-chars)

(bind-key "M-t" 'forward-word)
(bind-key "M-r" 'replace-string)
(bind-key "M-z" 'transpose-words)
