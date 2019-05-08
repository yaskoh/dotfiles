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
;; init-loader
;; 2017/01/17
;; https://github.com/emacs-jp/init-loader
;; ================================================
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits/")

;;; init.el ends here
