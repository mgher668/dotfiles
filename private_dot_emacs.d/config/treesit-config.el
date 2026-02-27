(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  
  ;; 语言源
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")))
  
  ;; 自动模式重映射
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode)
          (typescript-mode . typescript-ts-mode)))
  
  ;; 性能优化
  (setq treesit-font-lock-level 3)
  
  ;; 自动安装缺失语法
  (defun install-treesit-grammars ()
    "Install missing treesit grammars."
    (dolist (lang (mapcar 'car treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (message "Installing %s grammar..." lang)
        (treesit-install-language-grammar lang))))
  
  ;; 延迟安装，避免启动时阻塞
  (run-with-idle-timer 2 nil #'install-treesit-grammars))
