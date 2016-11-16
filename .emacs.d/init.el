;;; init.el --- emacs run-command file.

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
;; Serverをスタートしておく
;; ※windowsではserverフォルダの所有者権限が必要な模様。
;; ================================================
(when (eq system-type 'windows-nt)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; ================================================
;; Windowsでのフォント設定を変更
;; 2016/4/15
;; ================================================
(when (eq system-type 'windows-nt)
  (custom-set-faces
   '(default ((t (:family "Ricty Diminished" :foundry "outline" :slant normal :weight normal :height 115 :width normal))))))

;; ================================================
;; Windowsでのgrep/find利用のためパスを変更
;; 2016/4/15
;; ================================================
(when (eq system-type 'windows-nt)
  (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
  (setq find-program "C:\\msys64\\usr\\bin\\find.exe"
        grep-program "C:\\msys64\\usr\\bin\\grep.exe"))

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
;; カラーテーマを設定
;; ================================================
(add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes/zenburn")
(load-theme 'zenburn t)

;; ================================================
;; Key設定
;; ================================================
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-j" 'newline)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-o" 'other-window)
(global-set-key "\C-t" 'forward-char)
(global-set-key "\C-xo" 'other-frame)
(global-set-key "\C-x?" 'help-command)
(global-set-key "\C-z" 'undo)
(global-set-key (kbd "C-/") 'transpose-chars)

(global-set-key "\M-t" 'forward-word)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-z" 'transpose-words)


;; ================================================
;; Settings
;; ================================================
;; モードラインに列番号を表示
(column-number-mode t)
;; タブを使わない
(setq-default indent-tabs-mode nil)
;; タブ幅を4に設定
(setq-default tab-width 4)
(setq default-tab-width 4)
;; タブ幅の倍数を設定
(setq tab-stop-list
  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
;; バックアップとオートセーブ先をbackupsに設定
(add-to-list `backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
;; 行番号を表示させる
(global-linum-mode t)
;; 行ハイライト
(global-hl-line-mode)
;; ビープ音を消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; ================================================
;; Utilities
;; ================================================

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
(define-key global-map
  "\C-cs" 'scheme-other-window)

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
;; csharp-modeの設定
;; ================================================
;; (require 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; ================================================
;; html-helper-mode
;; 2013/1/3 追加
;; ================================================
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.phtml$" . html-helper-mode) auto-mode-alist))


;; ================================================
;; web-mode
;; ================================================
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;; ================================================
;; python-mode.el
;; 2013/1/13 追加, 2014/8/4 修正(org-modeにあたりがあった)
;; https://launchpad.net/python-mode/
;; ================================================
(require 'python-mode)
(setq py-load-pymacs-p t)


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
(define-key evil-insert-state-map [escape] 'evil-normal-state)


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
;; sql-mode
;; 2014/1/20
;; http://www.emacswiki.org/cgi-bin/wiki?SqlMode
;; ================================================
;; C-c C-c : 'sql-send-paragraph
;; C-c C-r : 'sql-send-region
;; C-c C-s : 'sql-send-string
;; C-c C-b : 'sql-send-buffer
(require 'sql)

(add-hook 'sql-interactive-mode-hook
          #'(lambda ()
              (interactive)
              (set-buffer-process-coding-system 'sjis-unix 'sjis-unix )
              (setq show-trailing-whitespace nil)))

;; starting SQL mode loading sql-indent / sql-complete
(eval-after-load "sql"
  '(progn
     (load-library "sql-indent")
     (load-library "sql-complete")
     (load-library "sql-transform")))

(setq auto-mode-alist
      (cons '("\\.sql$" . sql-mode) auto-mode-alist))

(sql-set-product-feature
 'ms :font-lock 'sql-mode-ms-font-lock-keywords)

(defcustom sql-ms-program "sqlcmd"
  "Command to start sqlcmd by SQL Server."
  :type 'file
  :group 'SQL)

(sql-set-product-feature
 'ms :sql-program 'sql-ms-program)
(sql-set-product-feature
 'ms :sqli-prompt-regexp "^[0-9]*>")
(sql-set-product-feature
 'ms :sqli-prompt-length 5)

(defcustom sql-ms-login-params
  '(user password server database)
  "Login parameters to needed to connect to mssql."
  :type '(repeat (choice
                  (const user)
                  (const password)
                  (const server)
                  (const database)))
  :group 'SQL)

(defcustom sql-ms-options '("-U" "-P" "-S" "-d")
  "List of additional options for `sql-ms-program'."
  :type '(repeat string)
  :group 'SQL)

(defun sql-connect-ms ()
  "Connect ti SQL Server DB in a comint buffer."
  ;; Do something with `sql-user', `sql-password',
  ;; `sql-database', and `sql-server'.
  (let ((f #'(lambda (op val)
               (unless (string= "" val)
                 (setq sql-ms-options
                       (append (list op val) sql-ms-options)))))
        (params `(("-U" . ,sql-user)("-P" . ,sql-password)
                  ("-S" . ,sql-server)("-d" . ,sql-database))))
    (dolist (pair params)
      (funcall f (car pair)(cdr pair)))
    (sql-connect-1 sql-ms-program sql-ms-options)))

(sql-set-product-feature
 'ms :sqli-login 'sql-ms-login-params)
(sql-set-product-feature
 'ms :sqli-connect 'sql-connect-ms)

(defun run-mssql ()
  "Run mssql by SQL Server as an inferior process."
  (interactive)
  (sql-product-interactive 'ms))


;; ================================================
;; yasnippet
;; 2014/3/21
;; https://github.com/capitaomorte/yasnippet
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
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-map (kbd "C-t") 'forward-char)
(define-key helm-map (kbd "C-k") 'kill-line)
; TABキーで補完(C-zと同様の動き)
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)


;; ================================================
;; emmet
;; 2014/3/21
;; https://github.com/smihica/emmet-mode
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
;; dos-mode
;; 2014/3/29
;; http://www.emacswiki.org/emacs/dos.el
;; ================================================
(require 'dos)
(setq auto-mode-alist
      (append '(("\\.bat$" . dos-mode)) auto-mode-alist))

;; ================================================
;; org-mode
;; 2014/4/6
;; http://orgmode.org/ja/
;; ================================================
;(require 'org)
;(require 'org-install)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; ================================================
;; flycheck
;; 2014/6/4
;; ================================================
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ================================================
;; visual-basic-mode
;; 2014/10/1
;; http://www.emacswiki.org/cgi-bin/wiki/visual-basic-mode.el
;; ================================================
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vba\\)$" .
                                visual-basic-mode)) auto-mode-alist))


;; ================================================
;; cobol-mode
;; 2014/12/11
;; http://www.emacswiki.org/cgi-bin/wiki/cobol-mode.el
;; ================================================
(require 'cobol-mode)
(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.cob$" . cobol-mode))))
(autoload 'cobol-mode "cobol-mode" "Major mode for Tandem COBOL files." t nil)


;; ================================================
;; dired-hide-details-mode
;; 2016/04/19
;; ================================================
(require 'dired)
(define-key dired-mode-map (kbd "(") 'dired-hide-details-mode)

;; ================================================
;; wdired settings
;; 2016/04/18
;; ================================================
(require 'wdired)
(define-key dired-mode-map (kbd "f") 'wdired-change-to-wdired-mode)

;; ================================================
;; dired-toggle
;; 2016/04/19
;; https://github.com/fasheng/dired-toggle
;; ================================================
(require 'dired-toggle)
(global-set-key (kbd "\C-xd") 'dired-toggle)
(define-key dired-toggle-mode-map (kbd "q") 'delete-window)

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


;;; init.el ends here
