;; org bullets
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis "⤵")
  :hook (org-mode . org-bullets-mode))

(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
			'(("^ *\\([+]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; Nice bullets
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(setq org-preview-latex-default-process 'dvisvgm)

(setq org-format-latex-option (plist-put org-format-latex-options :scale 2))

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
