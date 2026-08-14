(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/Dropbox/roam-notes/")
        org-roam-dailies-directory "daily/"
        org-id-locations-file (expand-file-name ".orgids" org-roam-directory))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ("C-c n u" . org-roam-ui-mode)
         ("C-c n d" . org-roam-dailies-goto-date))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode 1))

(use-package org-roam-ql
  :ensure t
  :after (org-roam)
  :bind ((:map org-roam-mode-map
               ;; Have org-roam-ql's transient available in org-roam-mode buffers
               ("v" . org-roam-ql-buffer-dispatch)
               :map minibuffer-mode-map
               ;; Be able to add titles in queries while in minibuffer.
               ;; This is similar to `org-roam-node-insert', but adds
               ;; only title as a string.
               ("C-c n i" . org-roam-ql-insert-node-title))))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t)
  (org-roam-ui-follow t))
