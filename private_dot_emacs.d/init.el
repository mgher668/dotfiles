;; proxy
;; (setq url-proxy-services '(("no_proxy" . "work\\.com")
;;                            ("http" . "proxy.work.com:911")))
(setq url-proxy-services '(("http" . "127.0.0.1:7890")))

;; packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     default))
 '(package-selected-packages
   '(aider all-the-icons buffer-move bufferlo centaur-tabs checkbox
           company consult doom-modeline doom-themes dtrt-indent
           ef-themes expand-region glsl-mode go-mode image+ keycast
           lsp-ui lua-mode marginalia markdown-preview-mode
           markdown-toc mode-icons multi-vterm orderless
           org-beautify-theme org-bullets org-download org-modern
           org-roam-ui org-superstar orgnote pandoc-mode paredit
           projectile rg scroll-restore smooth-scrolling
           spacemacs-theme tree-sitter-ess-r tree-sitter-indent
           tree-sitter-ispell typescript-mode undo-tree use-package
           valign vertico vterm-hotkey vterm-toggle web-mode yasnippet)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t) ;; 确保自动安装未安装的包

;; packages
(unless (package-installed-p 'use-package)  
  (package-refresh-contents)            ; 刷新包列表  
  (package-install 'use-package))       ; 安装 use-package

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
;; (load "centaur-tabs-config")      ;; 加载centaur-tabs配置
(load "undo-tree-config")         ;; 加载undo-tree配置
(load "savehist-config")          ;; 加载savehist配置
(load "desktop-save-mode-config") ;; 加载desktop-save-mode配置
(load "treesit-config")           ;; 加载treesit配置
;; (load "tree-sitter-config")       ;; 加载tree-sitter配置
(load "markdown-mode-config")     ;; 加载markdown-mode配置
(load "org-mode-config")          ;; 加载org-mode配置
(load "org-modern-config")        ;; 加载org-modern配置
(load "org-roam-config")          ;; 加载org-roam配置
(load "valign-config")            ;; 加载valign配置
;; (load "ef-theme-config")          ;; 加载ef-theme配置
(load "doom-themes-config")       ;; 加载doom-themes配置
(load "project-config")           ;; 加载project.el配置
(load "projectile-config")        ;; 加载projectile.el配置
(load "keycast-config")           ;; 加载keycast配置
(load "vertico-config")           ;; 加载vertico配置(vertico+orderless)
(load "orderless-config")         ;; 加载orderless配置(vertico+orderless)
(load "consult-config")           ;; 加载Consult配置
(load "marginalia-config")        ;; 加载marginalia配置
(load "company-config")           ;; 加载company配置
(load "yasnippet-config")         ;; 记载yasnippet配置
(load "org-download-config")      ;; 加载org-download配置
(load "image-plus-config")        ;; 加载image+配置
(load "dtrt-indent-config")       ;; 加载dtrt-indent配置
(load "lsp-mode-config")          ;; 加载lsp-mode配置
(load "web-mode-config")          ;; 加载web-mode配置
;; (load "aider-dot-el-config")      ;; 加载aider.el配置
;; (load "orgnote-config")           ;; 加载orgnote配置

;; 自动加载配置目录下的所有 .el 文件  
;; (let ((config-dir "~/.emacs.d/config/"))
;;   (when (file-directory-p config-dir) ;; 确保目录存在
;;     (add-to-list 'load-path config-dir)
;;     (dolist (file (directory-files config-dir t "\\.el$")) ;; 查找所有 .el 文件
;;       (load file))))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(tab-bar ((t (:height 1.0 :family "Iosevka"))))
 ;; '(tab-bar-tab ((t (:inherit tab-bar :weight bold))))
 ;; '(tab-bar-tab-inactive ((t (:inherit tab-bar)))))
;; (put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
