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
;(when (eq system-type 'windows-nt)
;  (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
;  (setq find-program "C:\\msys64\\usr\\bin\\find.exe"
;        grep-program "C:\\msys64\\usr\\bin\\grep.exe"))

;; ================================================
;; Serverをスタートしておく
;; ※windowsではserverフォルダの所有者権限が必要な模様。
;; http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
;; ================================================
(when (eq system-type 'windows-nt)
  (require 'server)
  (unless (server-running-p)
    (server-start)))

