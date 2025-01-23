;; 指定保存历史的文件路径
(setq savehist-file "~/.emacs.d/savehist")

;; 启用savehist模式
(savehist-mode 1)

;; 选择要保存的minibuffer记录类型
(setq savehist-additional-variables
      '(search-ring
	regexp-search-ring
	extended-command-history))  ;; 包括其他你想保存的历史

;;设置保存的禁用级别
(setq history-length 1000) ;; 根据需要调整历史记录的长度

;; 自动保存间隔为 5 分钟
(setq savehist-save-minibuffer-history 1)
