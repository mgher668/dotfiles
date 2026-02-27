(use-package image+
  :ensure t
  :config
  ;; 在org mode中启用
  (with-eval-after-load 'org
    (require 'image+)
    (imagex-global-sticky-mode 1)))
