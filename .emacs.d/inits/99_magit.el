;; ================================================
;; magit
;; 2017/01/16
;; https://github.com/magit/magit
;; ================================================
(when (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
  (setq exec-path (append '("C:\\msys64\\usr\\bin") exec-path))
  (setq magit-git-executable "C:\\msys64\\usr\\bin\\git.exe")
  (require 'magit))
(bind-key "C-x g" 'magit-status)
