(use-package lsp-mode
  :hook (web-mode . lsp-deferred)   ; LSP在web-mode基础上工作
  :config
  (setq lsp-html-server-command '("vscode-html-language-server" "--stdio")))
