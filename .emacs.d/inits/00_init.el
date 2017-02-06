;;; 00_init.el --- emacs run-command file.

;;; Commentary:

;;; Code:

;; ================================================
;; Package settings (Default package)
;; 2017/01/16
;; ================================================
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; ================================================
;; use-package
;; 2017/01/17
;; (including bind-key)
;; ================================================
(require 'use-package)


;;; 00_init.el ends here
