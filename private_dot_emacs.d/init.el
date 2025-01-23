;; proxy
;; (setq url-proxy-services '(("no_proxy" . "work\\.com")
;;                            ("http" . "proxy.work.com:911")))
(setq url-proxy-services '(("http" . "127.0.0.1:7890")))

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0" "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(package-selected-packages
   '(orgnote keycast tree-sitter tree-sitter-ess-r tree-sitter-indent tree-sitter-ispell tree-sitter-langs magit doom-themes glsl-mode smooth-scrolling ef-themes mode-icons multi-vterm vterm vterm-hotkey vterm-toggle lsp-mode org-roam-ui org-roam typescript-mode buffer-move checkbox org-bullets expand-region paredit lsp-ui bufferlo pandoc-mode savehist scroll-restore org-superstar org org-beautify-theme org-modern lua-mode use-package markdown-toc markdown-preview-mode go-mode spacemacs-theme tsc)))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; )

;; org mode config
;; ref from http://blog.lujun9972.win/emacs-document/blog/2020/02/19/beautify-org-mode/index.html
;; (when (member "Symbola" (font-family-list))
;;  (set-fontset-font "fontset-default" nil
;; 		   (font-spec :size 20 :name "Symbola")))

;; (when (member "Symbola" (font-family-list))
;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (setq default-buffer-file-coding-system 'utf-8)

;; (setq org-startup-indented t
;;       org-src-tab-acts-natively t)

(add-to-list 'load-path "~/.emacs.d/config/")
;; 手动加载
(load "general-config")           ;; 加载general配置
(load "undo-tree-config")         ;; 加载undo-tree配置
(load "savehist-config")          ;; 加载savehist配置
(load "desktop-save-mode-config") ;; 加载desktop-save-mode配置
(load "tree-sitter-config")       ;; 加载tree-sitter配置
(load "markdown-mode-config")     ;; 加载markdown-mode配置
(load "org-mode-config")          ;; 加载org-mode配置
(load "ef-theme-config")          ;; 加载ef-theme配置
(load "project-config")           ;; 加载project.el配置
(load "keycast-config")           ;; 加载keycast配置
;; (load "orgnote-config")           ;; 加载orgnote配置

;; 自动加载配置目录下的所有 .el 文件  
;; (let ((config-dir "~/.emacs.d/config/"))
;;   (when (file-directory-p config-dir) ;; 确保目录存在
;;     (add-to-list 'load-path config-dir)
;;     (dolist (file (directory-files config-dir t "\\.el$")) ;; 查找所有 .el 文件
;;       (load file))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
