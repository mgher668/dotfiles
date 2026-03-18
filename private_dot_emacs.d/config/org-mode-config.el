;; org bullets
;; (use-package org-bullets
;;   :custom
;;   (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
;;   (org-ellipsis "⤵")
;;   :hook (org-mode . org-bullets-mode))
;; 
;; (font-lock-add-keywords 'org-mode
;; 			'(("^ *\\([-]\\) "
;; 			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
;; (font-lock-add-keywords 'org-mode
;; 			'(("^ *\\([+]\\) "
;; 			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; Nice bullets
;; (use-package org-superstar
;;   :config
;;   (setq org-superstar-special-todo-items t)
;;   (add-hook 'org-mode-hook (lambda ()
;;                              (org-superstar-mode 1))))

(use-package org-superstar
  :ensure t
  :hook ((org-mode . org-superstar-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-hide-leading-stars t
        org-superstar-special-todo-items t
        org-superstar-headline-bullets-list '("◉" "○" "●" "◆" "▶")))

;; (use-package org-superstar
;;   :ensure t
;;   :hook ((org-mode . org-superstar-mode)
;;          (org-mode . org-indent-mode))
;;   :config
;;   (setq org-hide-leading-stars t
;;         org-superstar-special-todo-items t
;;         org-superstar-headline-bullets-list '("◉" "○" "●" "◆" "▶")))

;; Org Mode 下默认启用视觉自动换行
;; (add-hook 'org-mode-hook #'visual-line-mode)

;; 全局禁用 truncate-lines，自动换行显示超出窗口宽度的文本  
(setq-default truncate-lines nil)

(with-eval-after-load 'org
  (setq org-preview-latex-default-process 'dvisvgm
        org-format-latex-options
        (plist-put org-format-latex-options :scale 2)))

(setq org-list-demote-modify-bullet
 (quote (("+" . "-")
 ("-" . "+")
 ("*" . "-")
 ("1." . "-")
 ("1)" . "-")
 ("A)" . "-")
 ("B)" . "-")
 ("a)" . "-")
 ("b)" . "-")
 ("A." . "-")
 ("B." . "-")
 ("a." . "-")
 ("b." . "-"))))

;; enable org-modern-mode globally
(with-eval-after-load 'org (global-org-modern-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

'(org-edit-src-content-indentation 0)
'(org-src-preserve-indentation nil)
