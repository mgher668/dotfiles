(use-package dtrt-indent
  :ensure t
  :config
  (dtrt-indent-global-mode 1)
  ;; 设置检测的最小行数
  (setq dtrt-indent-min-lines-required 20)
  ;; 显示检测结果
  (setq dtrt-indent-verbosity 2))
