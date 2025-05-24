(setq inhibit-startup-message t
      visible-bell nil)
;; Turn off some unneeded UI elements
(menu-bar-mode -1)  ; Leave this one on if you're a beginner!
;; (tool-bar-mode -1)
(scroll-bar-mode 1)
(horizontal-scroll-bar-mode -1)
;; (scroll-lock-mode 1)
(pixel-scroll-precision-mode 1)

;; (window-divider-mode 1)
;; (setq window-divider-default-places t)
;; (setq window-divider-default-bottom-width 2)
;; (setq window-divider-default-right-width 2)

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

(when (< 26 emacs-major-version)
 (tab-bar-mode 1)                           ;; enable tab bar
 ;; (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
 (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
 (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
 (setq tab-bar-tab-hints t)                 ;; show tab numbers
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))
                                            ;; elements to include in bar

(custom-set-faces
 ;; '(tab-bar ((t (:height 1.1))))           ; 改为你需要的高度 (1.0 = 100%)
 ;; '(tab-bar-tab ((t (:height 1.1))))
 ;; '(tab-bar-tab-inactive ((t (:height 1.1))))
 '(tab-bar ((t (:height 1.0 :family "Iosevka"))))
 '(tab-bar-tab ((t (:inherit tab-bar :weight bold))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar))))
 )

;; package *buffer-move* is required (you can install it via MELPA or manually)
;; (require 'buffer-move)
;; (global-set-key (kbd "<C-S-up>") 'buf-move-up)
;; (global-set-key (kbd "<C-S-down>") 'buf-move-down)
;; (global-set-key (kbd "<C-S-left>") 'buf-move-left)
;; (global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; smoot scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 5)

;; display line numbers in every buffer
(global-display-line-numbers-mode 1)

;; bookmark
(global-set-key (kbd "C-c b s") 'bookmark-set)
(global-set-key (kbd "C-c b j") 'bookmark-jump)
(global-set-key (kbd "C-c b l") 'bookmark-bmenu-list)

;; set font
(set-frame-font "Iosevka 14" nil t)
;; (set-frame-font "Inconsolata 14" nil t)
;; (set-frame-font "Maple mono 14" nil t)
;; (set-frame-font "Sarasa Term SC Nerd 14" nil t)

;; 配置中文等宽字体
;; (require 'cnfonts)
;; 让cnfonts在Emacs启动时自动生效
;; (cnfonts-mode 1)
;; 添加两个字号增大缩小的快捷键
;; (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
;; (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(setq backup-directory-alist
 `(("." . "~/.emacs.d/backups")))
(setq make-backup-files t) ;; 确保备份功能开启

;; (fido-mode 1)
;; (fido-vertical-mode 1)
