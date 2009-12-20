;; Time-stamp: <2009-12-20 21:42:15 vmlinz>
;; Brand new emacs configuration for TeXing and c/c++ programming
;; Let's keep it really simple and easy

;; add local elisp packages to load path
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/.emacs.d/site-lisp/")

;; local yasnippet settings, see the package doc

;; #################### 01 localization ####################
;; needs further checking and practicing, read more on x resource and fonts

(defun my-set-frame-font ()
  (interactive)
  ;; default ansi code font
  (set-default-font "Inconsolata-16")
  ;; font for other scripts
  (set-fontset-font t
		    nil '("Inconsolata-16" . "unicode-bmp"))
  (set-fontset-font "fontset-startup"
		    'han '("WenQuanYi Micro Hei Mono-16" . "unicode-bmp") nil
		    'prepend)
  (set-fontset-font "fontset-startup"
		    'cjk-misc '("WenQuanYi Micro Hei Mono-16" . "unicode-bmp") nil
		    'prepend)
  (set-fontset-font "fontset-startup"
		    'kana '("WenQuanYi Micro Hei Mono-16" . "unicode-bmp") nil
		    'prepend)
  (set-fontset-font "fontset-startup"
		    'symbol '("WenQuanYi Micro Hei Mono-16" . "unicode-bmp") nil
		    'prepend)
  (set-face-font 'tooltip "fontset-startup")
  (set-frame-font "fontset-startup")
  (add-to-list 'default-frame-alist '(font . "fontset-startup"))
  (tabbar-mode )
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
 '(diary-file "~/.emacs.d/diary")
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Documents/notes/dailylife.org" "~/Documents/notes/study.org" "~/Documents/notes/work.org")))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-handled-backends (quote (GIT CVS SVN SCCS Bzr RCS Hg Mtn Arch))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;; #################### end 00 ####################

;; ########## yasnppet ##########
;; yasnippet
(require 'yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
;; ########## end ##########

;; ########## cc-mode ##########
;; c mode common hook
(defun my-c-mode-common-hook()
  (add-hook 'compilation-finish-functions
            (lambda (buf str)
              (if (string-match "exited abnormally" str) 
                  (next-error)
                ;;no errors, make the compilation window go away in a few seconds
                (run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create "*compilation*"))
                (message "No Compilation Errors!")
                )
              ))

  (add-hook 'c-mode-common-hook 
            (lambda ()
              (which-function-mode t)))

  (defun do-compile()
    (interactive)
    (compile (make-command))
    )

  (defun do-lint()
    (interactive)
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s %s %s"
                   "splint"
                   "+single-include -strict -compdef -nullpass -preproc +matchanyintegral -internalglobs -I/usr/include/gtk-2.0/ -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include -I/usr/include/cairo/"
                   file
                   )))
    (message compile-command)
    (compile compile-command)
    )

  (defun do-cdecl () 
    (interactive)
    (shell-command
     (concat "cdecl explain \"" (buffer-substring (region-beginning)
                                                  (region-end)) "\""))
    )

  (setq compilation-window-height 16)
  (setq compilation-scroll-output t)
  (setq gdb-show-main t)  
  (setq gdb-many-windows t)

  (require 'xcscope)

  (eval-after-load `xcscope
    `(progn
       ;; cscope databases
       (setq cscope-database-regexps
             '(
               ( "/home/Projects/libc"
                 (t)
                 ("/usr/src/linux/include")
                 )
               ))
       
       (setq cscope-do-not-update-database t)
       (setq cscope-display-cscope-buffer nil)
       (setq cscope-edit-single-match nil)))

  (yas/minor-mode t)
  
  ;; major key bindings for c-modes
  (local-set-key "\C-c\C-c" 'comment-dwim)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  (define-key c-mode-base-map [(f7)] 'compile)
  (define-key c-mode-base-map [(meta \')] 'c-indent-command)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-c-mode-hook ()
  (c-set-style "linux")
  ;; c preprocessing
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (c-set-style "stroustrup")
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; ########## end ##########

;; ########## emacs server ##########
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
;; ########## end ##########

(fset 'yes-or-no-p 'y-or-n-p)
(display-time)
(add-hook 'before-save-hook 'time-stamp)

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

;; ########## backup ##########
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq make-backup-files t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)
(setq backup-by-copying t)
(setq version-control t)
;; ########## end ##########

;; ########## gtags ##########
;; it works for me
(defun my-tags-generate-files ()
  "Generate ctags reference file for emacs."
  (interactive)
  (cd
   (read-from-minibuffer
    "directory: "
    default-directory))
  (shell-command "ctags -e -R"))
(global-set-key "\C-c\C-t" 'my-tags-generate-files)
;; ########## end ##########

;; ########## org remember ##########
(org-remember-insinuate)
(setq org-directory "~/Documents/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; ########## end ##########

;; ########## git ##########
;; git contrib, various git controls
(require 'git)
(require 'git-blame)
;; ########## end #########

;; ########## emms ##########
;; now just ignore it, for programming and daily use come first
;; ########## end ##########
