(use-package valign
  :ensure t
  :hook ((org-mode . valign-mode)
         (markdown-mode . valign-mode))
  :custom
  ;; 启用花哨的表格边框样式
  ;; (valign-fancy-bar t)
  ;; 设置最大表格大小限制（字符数）
  ;; (valign-max-table-size 4000)
  ;; 是否显示解析错误信息
  (valign-signal-parse-error nil)
  ;; 模式行显示的轻量标识
  ;; (valign-lighter " ⚖")
)
