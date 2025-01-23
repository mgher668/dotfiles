(require 'tree-sitter)
(require 'tree-sitter-langs)
(add-hook 'rust-mode-hook #'tree-sitter-mode)
(global-tree-sitter-mode)
(tree-sitter-require 'rust)
(tree-sitter-require 'go)
;; (require 'go-mode-autoloads)
(setq scroll-preserve-screen-position t)
(setq scroll-preserve-screen-position 'always)

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; (setq treesit-range-settings
;;       (treesit-range-rules
;;        :embed 'javascript
;;        :host 'html
;;        '((script_element (raw_text) @capture))
;;        :embed 'css
;;        :host 'html
;;        '((style_element (raw_text) @capture))))

;; ;; Major modes with multiple languages should always set
;; ;; `treesit-language-at-point-function' (which see).
;; (setq treesit-language-at-point-functionn
;;       (lambda (pos)
;;         (let* ((node (treesit-node-at pos 'html))
;;                (parent (treesit-node-parent node)))
;;           (cond
;;            ((and node parent
;;                  (equal (treesit-node-type node) "raw_text")
;;                  (equal (treesit-node-type parent) "script_element"))
;;             'javascript)
;;            ((and node parent
;;                  (equal (treesit-node-type node) "raw_text")
;;                  (equal (treesit-node-type parent) "style_element"))
;;             'css)
;;            (t 'html)))))
