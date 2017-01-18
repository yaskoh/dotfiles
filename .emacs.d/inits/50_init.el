;;; 50_init.el --- emacs run-command file.

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

;; ================================================
;; magit
;; 2017/01/16
;; https://github.com/magit/magit
;; ================================================
(when (eq system-type 'windows-nt)
  (setq exec-path (append '("C:\\msys64\\usr\\bin") exec-path))
  (setq magit-git-executable "C:\\msys64\\usr\\bin\\git.exe"))
(require 'magit)
(bind-key "C-x g" 'magit-status)

;; ================================================
;; El-Get
;; 2017/1/17
;; https://github.com/dimitri/el-get

;; うまくいかないので現時点保留中。
;; ================================================
(unless (require 'el-get nil 'noerror)
  (package-refresh-contents)
  (package-initialize)
  (package-install 'el-get)
  (require 'el-get))

(when (eq system-type 'windows-nt)
  (setq el-get-install-info (concat default-directory "bin/windows-nt/install-info.exe")))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;(el-get 'sync)


;; ================================================
;; Macでのキーボード配置を変更
;; ================================================
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'meta))

;; ================================================
;; Macでのフォント設定を変更
;; 2014/4/13
;; ================================================
(when (eq system-type 'darwin)
  ;English
  (set-face-attribute 'default nil
                      :family "Inconsolata")
  ;日本語
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "TakaoExGothic"))
  )

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
;; Gaucheの設定（コピペ）
;; ================================================
;; Gaucheのデフォルトエンコーディングに合わせます。
;; Gaucheのデフォルトエンコーディングがeuc-jpの時はutf-8をeuc-jpに
;; してください。
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
;; goshインタプリタのパスに合わせます。-iは対話モードを意味します。
(setq gosh-program-name "/usr/local/bin/gosh -i")
;; schemeモードとrun-schemeモードにcmuscheme.elを使用します。
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; ウィンドウを2つに分け、一方でgoshインタプリタを実行するコマンドを定義します。
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme gosh-program-name))
;; そのコマンドをCtrl-csで呼び出します。

;; 直前/直後の括弧に対応する括弧を光らせます。
(show-paren-mode)

;; 以下はインデントの定義です。
(put 'and-let* 'scheme-indent-function 1)
(put 'begin0 'scheme-indent-function 0)
(put 'call-with-client-socket 'scheme-indent-function 1)
(put 'call-with-input-conversion 'scheme-indent-function 1)
(put 'call-with-input-file 'scheme-indent-function 1)
(put 'call-with-input-process 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-iterator 'scheme-indent-function 1)
(put 'call-with-output-conversion 'scheme-indent-function 1)
(put 'call-with-output-file 'scheme-indent-function 1)
(put 'call-with-output-string 'scheme-indent-function 0)
(put 'call-with-temporary-file 'scheme-indent-function 1)
(put 'call-with-values 'scheme-indent-function 1)
(put 'dolist 'scheme-indent-function 1)
(put 'dotimes 'scheme-indent-function 1)
(put 'if-match 'scheme-indent-function 2)
(put 'let*-values 'scheme-indent-function 1)
(put 'let-args 'scheme-indent-function 2)
(put 'let-keywords* 'scheme-indent-function 2)
(put 'let-match 'scheme-indent-function 2)
(put 'let-optionals* 'scheme-indent-function 2)
(put 'let-syntax 'scheme-indent-function 1)
(put 'let-values 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)
(put 'let1 'scheme-indent-function 2)
(put 'letrec-syntax 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'multiple-value-bind 'scheme-indent-function 2)
(put 'match 'scheme-indent-function 1)
(put 'parameterize 'scheme-indent-function 1)
(put 'parse-options 'scheme-indent-function 1)
(put 'receive 'scheme-indent-function 2)
(put 'rxmatch-case 'scheme-indent-function 1)
(put 'rxmatch-cond 'scheme-indent-function 0)
(put 'rxmatch-if  'scheme-indent-function 2)
(put 'rxmatch-let 'scheme-indent-function 2)
(put 'syntax-rules 'scheme-indent-function 1)
(put 'unless 'scheme-indent-function 1)
(put 'until 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'while 'scheme-indent-function 1)
(put 'with-builder 'scheme-indent-function 1)
(put 'with-error-handler 'scheme-indent-function 0)
(put 'with-error-to-port 'scheme-indent-function 1)
(put 'with-input-conversion 'scheme-indent-function 1)
(put 'with-input-from-port 'scheme-indent-function 1)
(put 'with-input-from-process 'scheme-indent-function 1)
(put 'with-input-from-string 'scheme-indent-function 1)
(put 'with-iterator 'scheme-indent-function 1)
(put 'with-module 'scheme-indent-function 1)
(put 'with-output-conversion 'scheme-indent-function 1)
(put 'with-output-to-port 'scheme-indent-function 1)
(put 'with-output-to-process 'scheme-indent-function 1)
(put 'with-output-to-string 'scheme-indent-function 1)
(put 'with-port-locking 'scheme-indent-function 1)
(put 'with-string-io 'scheme-indent-function 1)
(put 'with-time-counter 'scheme-indent-function 1)
(put 'with-signal-handlers 'scheme-indent-function 1)
(put 'with-locking-mutex 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)


;; ================================================
;; tabbar.el
;; 2014/1/16 追加
;; http://www.emacswiki.org/emacs/download/tabbar.el
;; ================================================
(require 'tabbar)
(tabbar-mode 1)


;; ================================================
;; evil.el
;; 2014/1/16 追加
;; http://www.emacswiki.org/emacs/download/tabbar.el
;; ================================================
(require 'evil)
(evil-mode 1)

(setcdr evil-insert-state-map nil)
(bind-key [escape] 'evil-normal-state evil-insert-state-map)


;; ================================================
;; evil-mode-line.el
;; 2014/1/17 追加
;; https://raw.github.com/tarao/evil-plugins/master/evil-mode-line.el
;; (mode-line-colorも追加)
;; ================================================
(require 'evil-mode-line)


;; ================================================
;; cua-mode（矩形編集）の使用
;; ================================================
(cua-mode t)
(setq cua-enable-cua-keys nil)



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
;; slime
;; 2016/05/12
;; https://common-lisp.net/project/slime/
;; ================================================
(require 'slime-autoloads)
(when (eq system-type 'windows-nt)
  (setq inferior-lisp-program "wx86cl64"))
(when (eq system-type 'darwin)
  (setq inferior-lisp-program "/usr/local/bin/clisp"))
(setq slime-contribs '(slime-fancy))

;; ================================================
;; px
;; 2016/11/14
;; https://github.com/emacsmirror/px
;; ================================================
(require 'px)

;;; 50_init.el ends here
