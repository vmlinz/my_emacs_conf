;; Time-stamp: <2009-12-13 23:32:13 vmlinz>

;; #################### 01 localization ####################
(defun my-set-frame-font ()
  (interactive)
  ;; default ansi code font
  (set-default-font "Inconsolata-16")
  ;; font for other scripts
  (set-fontset-font t
		    nil '("Inconsolata-16" . "unicode-bmp"))
  (set-fontset-font "fontset-startup"
		    'han '("WenQuanYi Bitmap Song" . "unicode-bmp")
		    prepend)
  (set-fontset-font "fontset-startup"
		    'cjk-misc '("WenQuanYi Bitmap Song" . "unicode-bmp")
		    prepend)
  (set-fontset-font "fontset-startup"
		    'kana '("WenQuanYi Bitmap Song" . "unicode-bmp")
		    prepend)
  (set-fontset-font "fontset-startup"
		    'symbol '("WenQuanYi Bitmap Song" . "unicode-bmp")
		    prepend)
  (set-face-font 'tooltip "DejaVu Sans Mono-12")
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
  )
(add-hook 'after-make-frame-hook 'my-set-frame-font)
;; locales
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8 'utf-8)
'(set-buffer-process-coding-system 'utf-8 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
;; emacs shell color encoding
(ansi-color-for-comint-mode-on)
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
  (local-set-key "\C-c\C-c" 'comment-dwim)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (c-set-style "stroustrup")
  (local-set-key "\C-c\C-c" 'comment-dwim)
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; only start emacs server when it's not started, I hate warnings.
(setq server-socket-file "/tmp/emacs1000/server")
(unless (file-exists-p server-socket-file)
  (server-start))
;; exit emacs client
(defun exit-emacs-client ()
  "consistent exit emacsclient.
   if not in emacs client, echo a message in minibuffer, don't exit emacs.
   if in server mode
      and editing file, do C-x # server-edit
      else do C-x 5 0 delete-frame"
  (interactive)
  (if server-buffer-clients
      (server-edit)
    (delete-frame)))

(global-set-key (kbd "C-c q") 'exit-emacs-client)
(fset 'yes-or-no-p 'y-or-n-p)
(display-time)

;; ########## expand function ##########
(setq hippie-expand-try-functions-list
      '(try-expand-line
        try-expand-dabbrev
        try-expand-line-all-buffers
        try-expand-list
        try-expand-list-all-buffers
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-file-name-partially
        try-complete-lisp-symbol
        try-complete-lisp-symbol-partially
        try-expand-whole-kill))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-;") 'dabbrev-expand)
;;########## end ##########

;;########## key bindings ##########
;; buffer switching
(global-set-key (kbd "C-x p") 'previous-buffer)
(global-set-key (kbd "C-x n") 'next-buffer)
(global-set-key "\C-c\C-c" 'comment-dwim)
;; ########## end ##########

;; ########## auctex ##########
;; default to xelatex
(add-hook 'LaTeX-mode-hook (lambda()
			     (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
			     (setq TeX-command-default "XeLaTeX")
			     (setq TeX-save-query nil)
			     (setq TeX-show-compilation t)
			     (setq Tex-master nil)
			     ))
;; set preview application and preview style
(eval-after-load "tex"
  '(progn
     (TeX-global-PDF-mode t)
     (setq TeX-output-view-style
	   (cons '("^pdf$" "." "acroread %o") TeX-output-view-style)
	   )))
;; minor modes
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)  
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'TeX-mode-hook
          (lambda ()
            (turn-on-reftex)
            (auto-fill-mode)
            (outline-minor-mode)
            (flyspell-mode)))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (turn-on-reftex)
            (auto-fill-mode)
            (outline-minor-mode)
            (flyspell-mode)))
;; ########## end ##########