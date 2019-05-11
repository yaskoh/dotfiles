;;; init.el --- emacs run-command file. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; ================================================
;; Load-Path
;; ================================================
;; load pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 下記ディレクトリおよびサブディレクトリをサブディレクトリに追加
(add-to-load-path "elisp")


;; ================================================
;; straight
;; 2019/05/08
;; https://github.com/raxod502/straight.el
;; ================================================
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; ================================================
;; use-package
;; 2017/01/17
;; (including bind-key)
;; ================================================
(straight-use-package 'use-package)
; オプションなしで、use-packageをstraight.elにフォールバック(":straigt t" が不要)
(setq straight-use-package-by-default t)


;; ================================================
;; init-loader
;; 2017/01/17
;; https://github.com/emacs-jp/init-loader
;; ================================================
(straight-use-package 'init-loader)
(init-loader-load "~/.emacs.d/inits/")

;;; init.el ends here
