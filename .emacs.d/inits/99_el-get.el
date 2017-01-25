;; ================================================
;; El-Get
;; 2017/1/17
;; https://github.com/dimitri/el-get

;; Winはうまくいかないので現時点保留中。
;; ================================================
(when (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
  (unless (require 'el-get nil 'noerror)
    (package-refresh-contents)
    (package-initialize)
    (package-install 'el-get)
    (require 'el-get))

  (when (eq system-type 'windows-nt)
    (setq el-get-install-info (concat default-directory "bin/windows-nt/install-info.exe")))

  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

  (el-get 'sync)
)
