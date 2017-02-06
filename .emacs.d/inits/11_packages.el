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
(require 'yasnippet)
(yas-global-mode 1)


;; ================================================
;; emacs-flymake
;; 2014/3/21
;; https://github.com/illusori/emacs-flymake
;; ================================================
(require 'flymake)


;; ================================================
;; auto-complete
;; 2014/3/23
;; https://github.com/auto-complete/auto-complete/
;; ================================================
(require 'auto-complete-config)
(ac-config-default)
(auto-complete-mode 1)


;; ================================================
;; helm
;; 2014/3/21
;; https://github.com/emacs-helm/helm
;; ================================================
(require 'helm-config)
(helm-mode 1)
;; キーバインド
(bind-key "C-h" 'delete-backward-char helm-map)
(bind-key "C-t" 'forward-char helm-map)
(bind-key "C-k" 'kill-line helm-map)
; TABキーで補完(C-zと同様の動き)
(bind-key "<tab>" 'helm-execute-persistent-action helm-read-file-map)


;; ================================================
;; emmet
;; 2014/3/21
;; https://github.com/smihica/emmet-mode
;;
;; 2017/1/16 : emmet-expand-lineのキーを変更、C-,に。
;; ================================================
(require 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
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
(require 'flycheck)
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
(require 'wdired)
(bind-key "f" 'wdired-change-to-wdired-mode dired-mode-map)

;; ================================================
;; dired-toggle
;; 2016/04/19
;; https://github.com/fasheng/dired-toggle
;; ================================================
(require 'dired-toggle)
(bind-key "q" 'delete-window dired-toggle-mode-map)

;; ================================================
;; px
;; 2016/11/14
;; https://github.com/emacsmirror/px
;; ================================================
(require 'px)
