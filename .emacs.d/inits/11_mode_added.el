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
;; tabbar.el
;; 2014/1/16 追加
;; http://www.emacswiki.org/emacs/download/tabbar.el
;; ================================================
(require 'tabbar)
(tabbar-mode 1)


;; ================================================
;; cua-mode（矩形編集）の使用
;; ================================================
;(cua-mode t)
;(setq cua-enable-cua-keys nil)


;; ================================================
;; python-mode.el
;; 2013/1/13 追加, 2014/8/4 修正(org-modeにあたりがあった)
;; https://launchpad.net/python-mode/
;; ================================================
(require 'python-mode)
(setq py-load-pymacs-p t)

;; ================================================
;; csharp-modeの設定
;; ================================================
;; (require 'csharp-mode)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

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
;; dos-mode
;; 2014/3/29
;; http://www.emacswiki.org/emacs/dos.el
;; ================================================
(require 'dos)
(setq auto-mode-alist
      (append '(("\\.bat$" . dos-mode)) auto-mode-alist))


;; ================================================
;; html-helper-mode
;; 2013/1/3 追加
;; ================================================
;(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;(setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.phtml$" . html-helper-mode) auto-mode-alist))


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

