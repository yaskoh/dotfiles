;; ================================================
;; LaTeX設定
;; 2016/11/14
;; ================================================
(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:\\texlive\\2016\\bin\\win32" ":" (getenv "PATH")))
  (setq exec-path (append '("C:\\texlive\\2016\\bin\\win32") exec-path)))

(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/Library/TeX/texbin" ":" (getenv "PATH")))
  (setq exec-path (append '("/Library/TeX/texbin") exec-path)))


;; ================================================
;; yasnippet
;; 2014/3/21
;; https://github.com/capitaomorte/yasnippet
;; 
;; 2017/1/16 : yas-minor-modeのkeybindを変更、C-'に。
;; 2017/1/17 : yasnippetを0.11.0へupdate。bindも修正。
;; ================================================
(use-package yasnippet)
(yas-global-mode 1)


;; ================================================
;; emacs-flymake
;; 2014/3/21
;; https://github.com/illusori/emacs-flymake
;; ================================================
(use-package flymake)


;; ================================================
;; emmet
;; 2014/3/21
;; https://github.com/smihica/emmet-mode
;;
;; 2017/1/16 : emmet-expand-lineのキーを変更、C-,に。
;; ================================================
(use-package emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'nxml-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))


;; ================================================
;; jaspace
;; 2014/3/29
;; http://homepage3.nifty.com/satomii/software/elisp.ja.html
;; ================================================
(require 'jaspace)
;改行
(setq jaspace-alternate-eol-string "\xab\n")
;タブハイライト
(setq jaspace-highlight-tabs t)
;起動時設定
(add-hook 'text-mode-hook 'jaspace-mode-on)
(add-hook 'org-mode-hook 'jaspace-mode-on)
(jaspace-mode-on)


;; ================================================
;; flycheck
;; 2014/6/4
;; ================================================
(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ================================================
;; dired-hide-details-mode
;; 2016/04/19
;; ================================================
(require 'dired)
(bind-key "(" 'dired-hide-details-mode dired-mode-map)

;; ================================================
;; wdired settings
;; 2016/04/18
;; ================================================
(use-package wdired)
(bind-key "f" 'wdired-change-to-wdired-mode dired-mode-map)

;; ================================================
;; dired-toggle
;; 2016/04/19
;; https://github.com/fasheng/dired-toggle
;; ================================================
(use-package dired-toggle)
(bind-key "q" 'delete-window dired-toggle-mode-map)

;; ================================================
;; px
;; 2016/11/14
;; https://github.com/emacsmirror/px
;; ================================================
(use-package px)

;; ================================================
;; magit
;; 2017/01/16
;; https://github.com/magit/magit
;; ================================================
(use-package magit
  :config
  (when (or (eq system-type 'darwin)
            (eq system-type 'gnu/linux))
    (require 'magit))
  (bind-key "C-x g" 'magit-status)
  )
;; ================================================
;; openwith
;; 2017/07/22
;; 外部プログラムを使ってファイルを開く
;; ================================================
(use-package openwith)
(openwith-mode t)

;; ================================================
;; redo+
;; 2019/05/12
;; ================================================
(use-package redo+)

;; ================================================
;; dumb-jump
;; 2019/05/12
;; ================================================
(use-package dumb-jump
  :config
  (setq dumb-jump-mode t)
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-use-visible-window nil)
  (define-key global-map [(super d)] 'dumb-jump-go)
  (define-key global-map [(super shift d)] 'dumb-jump-back)
  )
