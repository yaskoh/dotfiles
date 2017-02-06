(custom-set-variables
 ;; prompt
 '(eshell-prompt-function
   (lambda ()
     (concat (user-login-name) "@" (system-name)
             " [" (abbreviate-file-name (eshell/pwd)) "]\n"
             (if (= (user-uid) 0) "#" "$") " ")))
 ;; prompt regexp
 '(eshell-prompt-regexp "^\\([^#$]+\\|[$#] \\)")
 )

;; regexp setting
(defvar eshell-prompt-regexp-lastline "^[#$] "
  "regexp for last line of prompt")

;; overwrite "eshell-skip-prompt" at em-prompt for multi-line prompt
;;
;; obsolete(defadvice)
;(defadvice eshell-skip-prompt (around ehell-skip-prompt-ext active)
;  (if (looking-at eshell-prompt-regexp)
;      (re-search-forward eshell-prompt-regexp-lastline nil t)))
;;
;; new
(defun eshell-skip-prompt-modified (orig-func &rest args)
  (if (looking-at eshell-prompt-regexp)
      (re-search-forward eshell-prompt-regexp-lastline nil t)))
(advice-add 'eshell-skip-prompt :around 'eshell-skip-prompt-modified)

;; alias
(setq eshell-command-aliases-list
      (append
       (list
        (list "emacs" "find-file $1")
        (list "d" "dired .")
        (list "ll" "ls -ltr")
        (list "la" "ls -a")
        )))

;; igrone duplicate in history
(setq eshell-hist-ignoredups t)

;; Start eshell when emacs starting
(add-hook 'after-init-hook (lambda () (eshell)))
