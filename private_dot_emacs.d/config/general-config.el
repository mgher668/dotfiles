(setq inhibit-startup-message t
      visible-bell nil)
;; Turn off some unneeded UI elements
(menu-bar-mode 1)  ; Leave this one on if you're a beginner!
(tool-bar-mode -1)
(scroll-bar-mode -1)
(scroll-lock-mode 1)
(pixel-scroll-precision-mode 1)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; Window Resize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(windmove-default-keybindings)

;; package *buffer-move* is required (you can install it via MELPA or manually)
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; smoot scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

;; display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; set font
(set-frame-font "Iosevka 14" nil t)
;; (set-frame-font "intel one mono 14" nil t)
;; (set-frame-font "Sarasa Term SC Nerd 14" nil t)

;; 配置中文等宽字体
;; (require 'cnfonts)
;; 让cnfonts在Emacs启动时自动生效
;; (cnfonts-mode 1)
;; 添加两个字号增大缩小的快捷键
;; (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
;; (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq make-backup-files t) ;; 确保备份功能开启

(fido-vertical-mode 1)
