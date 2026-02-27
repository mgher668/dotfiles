(use-package web-mode
  :mode ("\\.html\\'" "\\.htm\\'" "\\.xhtml\\'")
  :config
  ;; 启用JavaScript引擎（关键配置）
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")
          ("blade" . "\\.blade\\.")
          ("php" . "\\.php\\'")))
  
  ;; 启用内容类型识别（重要）
  (setq web-mode-content-types-alist
        '(("json" . "/some/path.*\\.api\\'")
          ("xml"  . "/other/path.*\\.api\\'")
          ("jsx"  . ".*\\.js[x]?\\'")))
  
  ;; 启用JavaScript高亮和缩进
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  
  ;; 关键：启用script高亮
  (setq web-mode-enable-part-face t)        ; 启用部分面孔
  (setq web-mode-enable-block-face t)       ; 启用块面孔
  (setq web-mode-enable-comment-keywords t) ; 启用注释关键词
  (setq web-mode-enable-heredoc-fontification t)
  
  ;; 缩进配置
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)    ; JavaScript缩进
  (setq web-mode-script-padding 2)        ; script标签内的缩进
  (setq web-mode-style-padding 2)         ; style标签内的缩进
  
  ;; 字体面配置
  (setq web-mode-comment-style 2)
  (setq web-mode-code-indent-offset 2))
