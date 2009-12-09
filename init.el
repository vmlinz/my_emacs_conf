;; Time-stamp: <2009-12-09 22:50:59 vmlinz>

;; #################### 01 localization ####################

;; #################### end 01 ####################
;; #################### 00 custom ####################
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(tool-bar-mode nil)
 '(inhibit-startup-screen t)
 '(inhibit-splash-screen t)
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; #################### end 00 ####################

(add-hook 'before-save-hook 'time-stamp)
(defun my-c-mode-hook ()
  (c-set-style "linux")
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (c-set-style "stroustrup")
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; only start emacs server when it's not started, I hate warnings.
(setq server-socket-file "/tmp/emacs1000/server")
(unless (file-exists-p server-socket-file)
  (server-start))
