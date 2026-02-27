;; Optionally use the `orderless' completion style.
(use-package orderless
  :config
  (setq orderless-matching-styles
	'(orderless-literal        ; 字面匹配
          orderless-regexp          ; 正则表达式
          orderless-initialism      ; 首字母缩写 (tab-new → tn)
          orderless-flex))          ; 灵活匹配 (tabnew → tab-new)
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides 
        '((file (styles basic partial-completion)))))
