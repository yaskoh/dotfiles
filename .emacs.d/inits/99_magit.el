;; ================================================
;; magit
;; 2017/01/16
;; https://github.com/magit/magit
;; ================================================
(when (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
  (require 'magit))
(bind-key "C-x g" 'magit-status)
