;; markdown-mode config
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; 启用 markdown-mode 嵌套代码块高亮
(add-hook 'markdown-mode-hook
	  (lambda ()
	    ;; 启用 Tree-sitter 以支持嵌套语言的高亮
	    (tree-sitter-mode)
	    ;; 添加 Typescript 支持
	    (tree-sitter-require 'typescript)))
