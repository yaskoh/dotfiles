;; ================================================
;; helm
;; 2014/3/21
;; https://github.com/emacs-helm/helm
;; ================================================
(use-package helm
  :disabled t
  :config
  (require 'helm-config)
  (helm-mode 1)
  ; あいまい検索
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  ;; キーバインド(helm-map)
  (bind-key "C-h" 'delete-backward-char helm-map)
  (bind-key "C-t" 'forward-char helm-map)
  (bind-key "C-k" 'kill-line helm-map)
  ; TABキーで補完(C-zと同様の動き)
  (bind-key "<tab>" 'helm-execute-persistent-action helm-read-file-map)
  )

;; ================================================
;; auto-complete
;; 2014/3/23
;; https://github.com/auto-complete/auto-complete/
;; obsolete : 2017/2/27, company
;; ================================================
(use-package auto-complete
  :disabled t
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (auto-complete-mode 1)
)

;; ================================================
;; ivy
;; 2019/5/12
;; ================================================
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 30)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
)

;; ================================================
;; counsel
;; 2019/5/12
;; ================================================
(use-package counsel
  :config
  (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
)
;; ================================================
;; swipper
;; 2019/5/12
;; ================================================
(setq swiper-include-line-number-in-search t) ;; line-numberでも検索可能

;; ================================================
;; company
;; 2019/5/12
;; ================================================
(use-package company
  :config
  (global-company-mode) ; 全バッファで有効にする
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-idle-delay 0) ; デフォルトは0.5
  (setq company-minimum-prefix-length 3) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  ;(global-set-key (kbd "C-M-i") 'company-complete)
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             )
  (bind-keys :map company-active-map
             ("C-n" . company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
             ("C-p" . company-select-previous)
             ("C-s" . company-filter-candidates) ;; C-sで絞り込む
             ("C-i" . company-complete-selection) ;; TABで候補を設定
             ("C-f" . company-complete-selection) ;; C-fで候補を設定
             )
  (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
)
