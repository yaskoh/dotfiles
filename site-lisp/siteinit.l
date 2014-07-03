

;;;================================================
;;; Backup設定
;;;================================================
(require "backup")
(setq *hierarchic-backup-directory* t)
(setq *filer-use-recycle-bin* t)
(setq *backup-directory* "C:/Program Files (x86)/xyzzy/backup/")

;;;================================================
;;; インクリメンタルサーチ (C-s, C-r)
;;;================================================
(require "isearch")

;;;================================================
;;; Key設定
;;;================================================
(global-set-key #\C-h 'delete-backward-char)
(global-set-key #\C-o 'other-window)
(global-set-key #\C-t 'forward-char)
(global-set-key '(#\C-x #\o) 'other-frame)
(global-set-key '(#\C-x #\?) 'help-command)
(global-set-key #\C-z 'transpose-chars)

;(global-set-key #\M-s 'transpose-words)
(global-set-key #\M-t 'forward-word)

;;;================================================
;;; emacs風キーバインド
;;;================================================
; key bind
(global-set-key #\C-/ 'undo)
(global-set-key #\M-v 'previous-page)
(global-set-key #\M-% 'replace-string)
(global-set-key #\C-M-% 'replace-regexp)
;(global-set-key #\M-% 'query-replace)
;(global-set-key #\C-M-% 'query-replace-regexp)
(global-set-key #\C-M-\\ 'indent-region)
(global-set-key '(#\C-x #\h) 'mark-page)
(global-set-key '(#\C-x #\u) 'undo)
(global-set-key '(#\C-x #\r #\d) 'delete-rectangle)
(global-set-key '(#\C-x #\r #\k) 'kill-rectangle)
(global-set-key '(#\C-x #\r #\o) 'open-rectangle)
(global-set-key '(#\C-x #\r #\t) 'string-rectangle)
(global-set-key '(#\C-x #\r #\y) 'yank-rectangle)
(global-set-key '(#\C-x #\r #\w) 'copy-rectangle)
(global-set-key #\Home 'beginning-of-buffer)
(global-set-key #\End 'end-of-buffer)
(global-set-key #\C-_ 'undo)
(global-set-key #\C-- 'negative-argument)
(global-set-key #\C-\\ 'toggle-ime)
(dolist (c '(#\C-0 #\C-1 #\C-2 #\C-3 #\C-4 #\C-5 #\C-6 #\C-7 #\C-8 #\C-9))
  (global-set-key c 'digit-argument))
(global-set-key '(#\C-x #\n #\n) 'narrow-to-region)
(global-set-key '(#\C-x #\n #\w) 'widen)
(global-set-key #\M-: 'eval-expression)
(substitute-key-definition 'next-virtual-line 'next-line)
(substitute-key-definition 'previous-virtual-line 'previous-line)

; dynamic abbreviation
(require "dabbrev")
(global-set-key #\M-/ 'dabbrev-expand)

; write-file
; バッファーの名前や割り付けられているファイル名も新しく指定
; されたファイルに変える
(define-key ctl-x-map #\C-w 'emacs-write-file)

; C-x C-v
; 廃棄するバッファのファイル名がミニバッファに表示
(setq *find-other-file-requires-file-name* t)

; sort-lines
(defun sort-lines (start end)
  (interactive "*r")
  (filter-region "sort" start end))

; ウィンドウを左右に分割
;(define-key ctl-x-map #\3 'split-window-vertically)

; ispell-word
(defun ispell-word()
  (interactive "*")
  (let ((end (progn
               (skip-chars-forward "A-Za-z")
               (point)))
        (beg (progn
               (skip-chars-backward "A-Za-z")
               (point))))
    (ispell-region beg end)))
(global-set-key #\M-$ 'ispell-word)

; frame
; (global-set-key '(#\C-x #\5 #\o) 'other-pseudo-frame)
; (global-set-key '(#\C-x #\5 #\0) 'delete-pseudo-frame)
; (global-set-key '(#\C-x #\5 #\2) 'new-frame)

; 文字コードの変更
(global-set-key '(#\C-x #\RET #\f) 'change-fileio-encoding)



;;;===============================================
;;; WINAPIパッケージを使う
;;;===============================================
(load-file "C:/Program Files (x86)/xyzzy/site-lisp/api.lc")
(use-package "winapi")

;;;===============================================
;;;アイコンの変更
;;;===============================================

(require "api")
;;アイコンは~/etcによろしく。
(let ((f #'(lambda ()
	     (winapi::SendMessage
	      (ed::get-window-handle) winapi::WM_SETICON  1
	      (winapi::LoadImage
	       0
	       (si:make-string-chunk
		(map-slash-to-backslash
		 (merge-pathnames "xyzzy.ico" (etc-path))))
	       winapi::IMAGE_ICON 0 0 winapi::LR_LOADFROMFILE)))))
 (if (member "startup" *modules* :test #'string=)
      (funcall f)
    (ed::add-hook 'ed::*post-startup-hook* f)))


;;;===============================================
;;; html+-mode設定
;;;===============================================
(export 'ed::html+-mode "ed")
(autoload 'html+-mode "html+-mode" t)
(pushnew '("\\.s?html?$" . html+-mode) *auto-mode-alist* :test 'equal)


;;;===============================================
;;; py-mode
;;; 2013/1/13
;;;===============================================
(load-library "py-mode")
(push '("\\.py$" . py-mode) *auto-mode-alist*)


;===============================================
;水平スクロールバー表示
;===============================================
; 編集中。selected-bufferでない形で書きたい。
; (set-local-window-flags (selected-buffer) *window-flag-hscroll-bar* t)


