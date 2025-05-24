(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq vertico-cycle t))

(use-package marginalia
  :init
  (marginalia-mode 1)
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light
          nil))
)
