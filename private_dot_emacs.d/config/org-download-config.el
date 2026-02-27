(use-package org-download
  :ensure t
  :after org
  :config
  (setq-default org-download-image-dir "./org-download")
  (setq org-download-image-org-width 600)
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))
