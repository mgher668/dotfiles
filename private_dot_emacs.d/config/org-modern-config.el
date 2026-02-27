(use-package org-modern
    :after org
    :config
    (setq org-modern-table nil)
    (add-hook 'org-mode-hook #'org-modern-mode))
