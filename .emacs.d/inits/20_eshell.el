;; prompt
(setq eshell-prompt-function
      (lambda ()
        (concat (user-login-name) "@" (system-name)
                " [" (abbreviate-file-name (eshell/pwd)) "]\n"
                (if (= (user-uid) 0) "#" "$") " ")))

;; alias
(setq eshell-command-aliases-list
      (append
       (list
        (list "emacs" "find-file $1")
        (list "d" "dired .")
        (list "ll" "ls -ltr")
        (list "la" "ls -a")
        )))

;; prompt regexp
(setq eshell-prompt-regexp "^[^#$]*[$#] ")
;; igrone duplicate in history
(setq eshell-hist-ignoredups t)
