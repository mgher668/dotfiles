;; undo-tree-mode config
(require 'undo-tree)
(global-undo-tree-mode)

;; (global-undo-tree-mode)
;; (setq undo-tree-auto-save-history t)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
