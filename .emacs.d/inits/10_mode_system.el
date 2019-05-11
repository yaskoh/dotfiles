;; ================================================================
;; org-mode
;; 2014/4/6
;; http://orgmode.org/ja/
;; ================================================================
;; default mode on new buffers
(setq default-major-mode 'org-mode)

(use-package org)
;(require 'org-install)
(bind-key "C-c l" 'org-store-link org-mode-map)
(bind-key "C-c c" 'org-capture org-mode-map)
(bind-key "C-c a" 'org-agenda org-mode-map)
(bind-key "C-c b" 'org-iswitchb org-mode-map)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; ================================================================
;; nxml-mode
;; 2017/4/9
;; ================================================================
(require 'nxml-mode)
(setq nxml-slash-auto-complete-flag t) ;completion by </
(bind-key "C-<return>" 'nxml-complete nxml-mode-map)
