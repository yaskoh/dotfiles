;; ================================================
;; Settings
;; ================================================
;; モードラインに列番号を表示
(column-number-mode t)
;; タブを使わない
(setq-default indent-tabs-mode nil)
;; タブ幅を4に設定
(setq-default tab-width 4)
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
