(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p b") 'project-switch-to-buffer)
(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p e") 'project-eshell)

;; ;; 可选：定义特定命令快捷键  
;; (defun my-project-compile ()  
;;   "自定义的项目编译命令."  
;;   (interactive)  
;;   (project-compile))  

;; (global-set-key (kbd "C-c p c") 'my-project-compile)

;; 自定义项目根目录查找函数
(defun my-project-root (dir)
  "Define a custom project root finding function."
  (when (locate-dominating-file dir ".my-project-root")
    (cons 'vc dir)))

;; (with-eval-after-load 'project
;;   (add-to-list 'project--ignored 'node-modules))

(add-hook 'project-find-functions 'my-project-root)
