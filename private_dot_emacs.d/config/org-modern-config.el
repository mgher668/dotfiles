(use-package org-modern
    :after org
    :config
    ;; Keep org-modern features, but let org-superstar control headline bullets.
    (setq org-modern-star nil)
    (setq org-modern-table nil)
    (add-hook 'org-mode-hook #'org-modern-mode))
