(require 'centaur-tabs)

;; 启用 centaur-tabs
(centaur-tabs-mode t)

;; Headline face
(centaur-tabs-headline-match)

;; 设置快捷键
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)

;; 设置标签外观
(setq centaur-tabs-style "alternate")  ; 标签样式，可以选择 bar, box 等
(setq centaur-tabs-set-bar 'under) ; 设置标签栏显示位置
(setq centaur-tabs-height 30)  ; 设置标签高度
(setq centaur-tabs-set-icons t)
(setq centaur-tabs-icon-type 'all-the-icons)  ; or 'nerd-icons

;; 标签激活后的前景色和背景色
;; (set-face-attribute 'centaur-tabs-selected nil
;;                     :background "gray30"
;;                     :foreground "white")
;; 
;; (set-face-attribute 'centaur-tabs-unselected nil
;;                     :background "gray20"
;;                     :foreground "gray70")
