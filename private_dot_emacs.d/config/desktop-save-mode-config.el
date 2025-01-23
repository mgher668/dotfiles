;; 在重新启动时恢复上次的文件、窗口布局和选项卡（答案来自chatGPT-4o）
(desktop-save-mode 1)
(setq desktop-save t) ; 自动保存
(setq desktop-auto-save-timeout 300)  ; 每5分钟保存一次
(add-hook 'emacs-startup-hook 'desktop-read)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (if (not (file-directory-p desktop-dirname))
		(make-directory desktop-dirname t))
	    (desktop-read)))
