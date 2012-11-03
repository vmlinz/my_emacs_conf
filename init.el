;; This file is not part of gnu emacs
;; Time-stamp: <2012-11-03 23:35:38 vmlinz>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; 1.Emacs configuration for TeXing and c/c++ programming
;; 2.Speed up startup time by making defuns and hooks
;; 3.The configuration contains some specifical settins for Lenovo X200
;; 4.Introduce the newly pacage manager _el-get_ to manage various third party
;; packages

;; [done]1.Configure cedet and auto-complete
;; [next]2.Get to know more about elisp programming
;; [done]3.Make it portable between computers
;; [todo]4.keep simplifying this configuration file
;; [todo]5.Make configuration of cedet really useful for programming
;; [done]6.Use gtags from el-get
;; [todo]7.Consider using builtin cedet or replace it completely
;; [todo]8.Reorganize this file into org file using babel to generate it

;; ########## cedet ##########
(add-to-list 'load-path "~/.emacs.d/site-start.d")
(require 'my-cedet-init)
;; ########## end ##########

;; ########## local lisp ##########
;; (add-to-list 'load-path "~/.emacs.d/site-lisp")
;; ########## end ##########

;; ########## localization ##########
;; needs further checking and practicing, read more on x resource and fonts
(defun my-set-frame-font ()
  (interactive)
  ;; default ansi code font
  (set-frame-font "Inconsolata-16")
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
  )

(when (window-system)
  (add-hook 'after-make-frame-hook 'my-set-frame-font)
  (add-hook 'before-make-frame-hook
	    #'(lambda ()
		(add-to-list 'default-frame-alist '(left   . 0))
		(add-to-list 'default-frame-alist '(top    . 0))
		(add-to-list 'default-frame-alist '(height . 25))
		(add-to-list 'default-frame-alist '(width  . 80))
		))
  )

;; encodings and locales
(defun my-coding-system-init()
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
  )
(my-coding-system-init)
;; ########## end ##########

;; ########## custom ##########
(setq custom-file "~/.emacs.d/site-start.d/my-custom.el")
(load custom-file)
;; ########## end ##########

;; ########## misc settings ##########
(defun my-misc-custom-init()
  (display-time)
  ;; time format
  (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  ;; emacs shell color encoding
  (ansi-color-for-comint-mode-on)
  ;; set inferior shell prompt read-only
  (setq comint-prompt-read-only t)
  (auto-image-file-mode t)
  (if (boundp 'tabbar-mode)
      (tabbar-mode -1)
    )
  ;; ido-mode
  (ido-mode t)
  (setq ido-enable-prefix nil
	ido-enable-flex-matching t
	ido-create-new-buffer 'always
	ido-use-filename-at-point 'guess
	ido-max-prospects 10)
  ;; icomplete
  (icomplete-mode t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq use-dialog-box nil)
  ;; set my email address
  (setq user-mail-address "vmlinz@gmail.com")
  (setq user-full-name "Nick Qi")

  ;; ########## editing ##########
  (setq x-select-enable-clipboard t)
  (add-hook 'before-save-hook 'time-stamp)
  ;; delete trailing whitespaces before save
  (add-hook 'before-save-hook 'whitespace-cleanup)
  ;; check last line to be a newline
  (setq require-final-newline t)
  ;; set insert parenthese without space
  (setq parens-require-spaces nil)
  ;; set kill-ring-max to 512
  (setq kill-ring-max 512)
  ;; enter view-mode when buffer is read-only
  (setq view-read-only t)
  ;; set comment style to multi-line
  (setq comment-style 'multi-line)
  ;; inhibit startup echo area message
  (setq inhibit-startup-echo-area-message 1)
  ;; ########## end ##########

  ;; ########## fullscreen ##########
  ;; fullscreen
  (defun my-fullscreen (&optional f)
    (interactive)
    (set-frame-parameter f 'fullscreen
			 (if (frame-parameter f 'fullscreen) nil 'fullboth)))

  (global-set-key [f11] 'my-fullscreen)
  ;; ########## end ##########

  ;; ########## current position  ##########
  ;; Get current line number and column number
  (defun my-current-line-and-column ()
    "Get current line number and column number."
    (interactive)
    (message "Line %d, Column %d" (line-number-at-pos) (current-column))
    )

  (global-set-key "\C-x?" 'my-current-line-and-column)
  ;; ########## end ##########

  ;; ########## menu-bar ##########
  ;; turn off menu-bar when in terminal
  (unless (window-system)
    (menu-bar-mode -1)
    )
  ;; ########## end ##########

  ;; ########## switches ##########
  ;; various toggles for the disabled commands
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  ;; ########## end ##########

  ;; ########## browse url ##########
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "google-chrome")
  ;; ########## end ##########

  ;; ########## emacs title ##########
  (setq frame-title-format '("" "%b - Emacs " emacs-version))
  ;; ########## end ##########

  (setq warning-suppress-types ())
  )
(my-misc-custom-init)
;; ########## end ##########

;; ########## emacs server ##########
;; only start emacs server when it's not started, I hate warnings.
(defun my-server-init()
  (setq server-socket-file
	(concat "/tmp/emacs"
		(int-to-string (user-uid)) "/server"))

  (unless (file-exists-p server-socket-file)
    (server-start))

  (defun my-server-force-start ()
    "delete current server socket and restart the server"
    (interactive)
    (progn
      '((server-force-delete)
	(server-start))))

  (defun my-server-kill-client ()
    "consistent exit emacsclient"
    (interactive)
    (if server-buffer-clients
	(server-edit)
      (delete-frame)))
  (global-set-key (kbd "C-c C-q") 'my-server-kill-client)
  )
(my-server-init)
;; ########## end ##########

;; ########## expand function ##########
(defun my-hippie-expand-init()
  (setq hippie-expand-try-functions-list
	'(
	  try-expand-dabbrev
	  try-expand-dabbrev-visible
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-expand-line
	  try-expand-line-all-buffers
	  try-expand-list
	  try-expand-list-all-buffers
	  try-complete-file-name
	  try-complete-file-name-partially
	  try-complete-lisp-symbol
	  try-complete-lisp-symbol-partially
	  try-expand-whole-kill))
  (global-set-key (kbd "M-;") 'hippie-expand)
  )
(my-hippie-expand-init)
;;########## end ##########

;;########## key bindings ##########
;; some keybindings here are specifical to Lenovo Thinkpad
(defun my-key-init()
  ;; skeleton pairs
  (setq skeleton-pair t)
  (global-set-key "(" 'skeleton-pair-insert-maybe)
  (global-set-key "{" 'skeleton-pair-insert-maybe)
  (global-set-key "[" 'skeleton-pair-insert-maybe)
  ;; buffer switching keys
  (global-set-key (kbd "C-<") 'previous-buffer)
  (global-set-key [(XF86Back)] 'previous-buffer)
  (global-set-key (kbd "C->") 'next-buffer)
  (global-set-key [(XF86Forward)] 'next-buffer)
  ;; comment-dwim
  (global-set-key "\C-c\C-c" 'comment-dwim)
  (global-set-key "\C-x\C-b" 'ibuffer)
  ;; set mark
  (global-set-key "\M-m" 'set-mark-command)
  (global-set-key "\C-c\M-m" 'pop-to-mark-command)
  ;; woman
  (global-set-key "\C-hj" 'woman)
  )
(my-key-init)
;; ########## end ##########

;; ########## backup ##########
(defun my-backup-init()
  (setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
  (setq make-backup-files t)
  (setq kept-old-versions 2)
  (setq kept-new-versions 10)
  (setq delete-old-versions t)
  (setq backup-by-copying t)
  (setq version-control t)
  )
(my-backup-init)
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

;; ########## org ##########
;;function gtd
(defun my-gtd ()
  (interactive)
  (find-file "~/Documents/notes/dailylife.org")
  )
;; ########## end ##########

;; ########## org mode and remember ##########
(defun my-org-mode-init()
  (setq org-directory "~/Documents/notes")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-log-done 'note)

  ;; custom capture-templates
  (setq org-capture-templates
	'(
	  ("d" "Daily Notes" entry
	   (file+datetree (concat org-directory "/dailylife.org"))
	   "* TODO %^{Description} %^g\n  %?%U")
	  ("s" "Study Journal" entry
	   (file+datetree (concat org-directory "/study.org"))
	   "* TODO %^{Description} %^g\n  %?%U")
	  ("w" "Work Journal" entry
	   (file+datetree (concat org-directory "/work.org"))
	   "* TODO %^{Description} %^g\n  %?%U")
	  ("l" "Time Log" entry
	   (file+datetree (concat org-directory "/timelog.org"))
	   "* %U - %^{Activity} :TIME:")
	  ("n" "Ramdom Notes" entry
	   (file+datetree (concat org-directory "/notes.org"))
	   "* %^{Description} %^g\n  %i\n  %a\n  %?%U")
	  ("x" "Clipboard Notes" entry
	   (file+datetree (concat org-directory "/notes.org"))
	   "* %^{Description} %^g\n  %x\n  %?%U")
	  ))

  ;;custom commands for the use of GTD.
  (setq org-agenda-custom-commands
	'(
	  ("w" todo "WAITING" nil)
	  ("n" todo "NEXT" nil)
	  ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT")))
	  ))

  (global-set-key "\C-cr" 'org-capture)
  (global-set-key "\C-cc" 'calendar)
  (global-set-key "\C-ca" 'org-agenda)
  )
(my-org-mode-init)
;; ########## end ##########

;; ########## woman ##########
;; settings for woman
(add-hook 'after-init-hook
	  '(lambda()
	     (setq woman-use-own-frame nil)
	     (setq woman-fill-column 80)
	     ))
;; ########## end ##########

;; ########## emms ##########
;; now just ignore it, for programming and daily use come first
;; ########## end ##########

;; ########## cc-mode ##########
;; c mode common hook
(defun my-compile-init ()
  "compile mode init function"
  (add-hook 'compilation-finish-functions
	    '(lambda (buf str)
	       (if (string-match "exited abnormally" str)
		   (next-error)
		 ;;no errors, make the compilation window go away in a few seconds
		 (run-at-time "2 sec" nil 'delete-windows-on
			      (get-buffer-create "*compilation*"))
		 (message "No Compilation Errors!")
		 )
	       ))
  (setq compilation-window-height 16)
  (setq compilation-scroll-output t)
  )

(defun my-gud-init ()
  "gud mode init function"
  (add-hook 'gud-mode-hook
	    '(lambda()
	       (setq gdb-show-main t)
	       (setq gdb-many-windows -1)
	       (define-key gud-mode-map [(f8)] 'gdb-many-windows)
	       ))
  )

(defun my-cscope-init ()
  "cscope emacs mode init function"
  (require 'xcscope)
  (eval-after-load `xcscope
    `(progn
       (setq cscope-do-not-update-database t)
       (setq cscope-display-cscope-buffer nil)
       (setq cscope-edit-single-match nil)))
  )

(defun my-c-mode-key-init ()
  "c mode key bindings"
  (local-set-key "\C-c\C-c" 'comment-dwim)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  (define-key c-mode-base-map [(f7)] 'compile)
  (define-key c-mode-base-map [(f8)] 'gdb)
  (define-key c-mode-base-map [(meta \')] 'c-indent-command)
  )

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defun my-c-linux-style-init ()
  "add linux c style"
  (c-add-style
   "linux-tabs-only"
   '("linux"
     (c-offsets-alist
      (arglist-cont-nonempty
       c-lineup-gcc-asm-reg
       c-lineup-arglist-tabs-only))))
  )

(defun my-c-mode-init ()
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)

  (c-set-style "linux-tabs-only")

  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)

  (auto-fill-mode 1)
  (hs-minor-mode 1)
  )

(defun my-cc-mode-init()
  (add-hook 'c-mode-common-hook 'my-compile-init)
  (add-hook 'c-mode-common-hook 'my-gud-init)
  (add-hook 'c-mode-common-hook 'my-cscope-init)
  (add-hook 'c-mode-common-hook 'my-c-mode-key-init)
  (add-hook 'c-mode-common-hook 'my-c-linux-style-init)

  (add-hook 'c-mode-hook 'my-c-mode-init)
  (add-hook 'c++-mode-hook
	    '(lambda ()
	       (c-set-style "stroustrup")

	       (setq indent-tabs-mode nil)
	       (setq tab-width 4)
	       (setq c-basic-offset 4)
	       )
	    )
  (add-hook 'java-mode-hook
	    '(lambda ()
	       (c-set-offset 'arglist-cont-nonempty '+)
	       (c-set-offset 'topmost-intro-cont 0)

	       (setq indent-tabs-mode nil)
	       (setq tab-width 4)
	       (setq c-basic-offset 4)
	       )
	    )
  )
(my-cc-mode-init)
;; ########## end ##########

;; ########## auctex ##########
;; feature rich package for TeXing
(defun my-auctex-init()
  (add-hook 'LaTeX-mode-hook
	    '(lambda()
	       (add-to-list 'TeX-command-list
			    '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
	       (setq TeX-command-default "XeLaTeX")
	       (setq TeX-save-query nil)
	       (setq TeX-show-compilation t)
	       (setq Tex-master nil)
	       )
	    )

  (eval-after-load "tex"
    '(progn
       (TeX-global-PDF-mode t)
       (setq TeX-output-view-style
	     (cons '("^pdf$" "." "acroread %o") TeX-output-view-style)
	     )))

  (add-hook 'TeX-mode-hook
	    '(lambda ()
	       (turn-on-reftex)
	       (auto-fill-mode)
	       (outline-minor-mode)
	       (flyspell-mode)))

  (add-hook 'LaTeX-mode-hook
	    '(lambda ()
	       (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
	       (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
	       (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
	       (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
	       (LaTeX-math-mode)
	       (turn-on-reftex)
	       (auto-fill-mode)
	       (outline-minor-mode)
	       (flyspell-mode)
	       ))
  )
(my-auctex-init)
;; ########## end ##########

;; ########## erc ##########
;; erc auto join channels
(add-hook 'erc-mode-hook
	  '(lambda()
	     (require 'erc-join)
	     (erc-autojoin-mode 1)
	     (setq erc-autojoin-channels-alist
		   '(("freenode.net" "#emacs" "#ubuntu" "#ubuntu-cn" "#kernel"))
		   )))
;; ########## end ##########

;; ########## sh mode ##########
;; .zsh files to sh auto-mode-alist
(defun my-sh-mode-init()
  (autoload 'sh-mode "sh-script"
    "Major mode for editing shell files" t)
  (setq auto-mode-alist
	(cons '("\\.zsh\\'" . sh-mode) auto-mode-alist))
  )
(my-sh-mode-init)
;; ########## end ##########


;; ########## lisp settings ##########
;; byte compile emacs init file and load it
(global-set-key [f12]
		'(lambda()
		   (interactive)
		   (byte-compile-file "~/.emacs.d/init.el" t)
		   (byte-compile-file custom-file t)
		   ))
;; ########## end ##########

;; ########## scheme mode ##########
;; scheme mode setup
(defun my-scheme-init()
  (setq quack-global-menu-p nil)
  (setq quack-default-program "guile")
  (setq quack-fontify-style 'emacs)
  (setq quack-run-scheme-always-prompts-p nil)

  (add-hook 'scheme-mode-hook
	    '(lambda ()
	       (setq scheme-program-name "guile")
	       ))
  )
;; ########## end ##########

;; ########## yasnippet ##########
;; yasnippet is now managed by _el-get_
(defun my-yasnippet-init()
  "simple yasnippet mode init function"
  (require 'yasnippet)
  ;; set yasnippet default dirs
  (setq yas-snippet-dirs (list
			  (concat el-get-dir
				  (file-name-as-directory "yasnippet")
				  "snippets")
			  (concat el-get-dir
				  (file-name-as-directory "yasnippet")
				  (file-name-as-directory "extras")
				  "imported")))
  (setq yas-use-menu 'abbreviate)
  (setq yas-prompt-functions
	(cons 'yas-dropdown-prompt
	      (remove 'yas-dropdown-prompt
		      yas-prompt-functions)))
  (setq yas-verbosity 0)
  (yas-global-mode t)
  )
;; ########## end ##########

;; ########## auto-complete ##########
;; this package is good to use and easy to config
(defun my-ac-semantic-setup ()
  "semantic source configuration for auto-complete"
  (setq ac-sources
	(append '(ac-source-semantic ac-source-semantic-raw) ac-sources))
  (define-key ac-mode-map "\M-/" 'ac-complete-semantic-raw)
  )

(defun my-ac-py-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
  )
(defun my-ac-sgml-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
  )

(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'my-ac-semantic-setup)
  ;; (ac-ropemacs-initialize)
  (add-hook 'python-mode-hook 'my-ac-py-setup)
  (add-hook 'html-mode-hook 'my-ac-sgml-setup)
  )

(defun my-auto-complete-init()
  "auto-complete init function"
  (require 'auto-complete-config)
  (ac-config-default)

  (my-ac-config)

  (setq ac-dwim t)
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (setq ac-delay 0.1)
  (setq ac-use-quick-help nil)
  (setq ac-menu-height 5)

  (setq ac-use-menu-map t)
  (define-key ac-menu-map "\C-n" 'ac-next)
  (define-key ac-menu-map "\M-n" 'ac-next)
  (define-key ac-menu-map "\C-p" 'ac-previous)
  (define-key ac-menu-map "\M-p" 'ac-previous)
  (define-key ac-menu-map "\C-f" 'ac-stop)
  (define-key ac-menu-map "\M-f" 'ac-stop)
  )
;; ########## end ##########

;; ########## git ##########
;; configure magit package for git support
(defun my-git-init()
  "magit init function for el-get"
  (autoload 'magit-status "magit" nil t)
  (global-set-key (kbd "C-x C-z") 'magit-status)
  (eval-after-load 'magit
    '(progn
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-foreground 'magit-diff-del "red3")))
  )
;; ########## end #########

;; ########## python mode ##########
;; python mode customizations
(defun my-py-init()
  (add-hook 'python-mode-hook
	    '(lambda ()
	       (setq indent-tabs-mode nil)
	       (setq python-indent 4)
	       (setq python-python-command "python3") ;default to python3
	       ))
  )
(my-py-init)
;; ########## end ##########

;; ########## sgml and html mode ##########
(defun my-sgml-init()
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  )
;; ########## end ##########

;; ########## evil mode ##########
(defun my-evil-init()
  "evil mode init function for el-get"
  (autoload 'evil-local-mode "evil" nil t)
  (global-set-key "\C-c\C-v" 'evil-local-mode)
  )
;; ########## end ##########

;; ########## nxhtml mode ##########
(defun my-nxhtml-init()
  "nxhtml init function for el-get"
  (autoload 'nxhtml-mode "autostart" nil t)
  )
;; ########## end ##########

;; ########## markdown ##########
;; markdown-mode for translating Pro Git
(defun my-markdown-mode-init()
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  )
;; ########## end ##########

;; ########## global ##########
;; local global mode
(defun my-gtags-init()
  "GNU Gloabal init function for el-get"
  (define-key gtags-mode-map "\M-." 'gtags-find-tag)
  (define-key gtags-mode-map "\M-," 'gtags-find-rtag)
  (define-key gtags-mode-map "\M-*" 'gtags-pop-stack)

  (add-hook 'gtags-select-mode-hook
	    '(lambda ()
	       (setq hl-line-face 'underline)
	       (hl-line-mode 1)
	       ))

  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (gtags-mode 1)))
  )
;; ########## end ##########

;; ########## el-get ##########
;; the great package management tool el-get
(defun my-el-get-init()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
  (unless (require 'el-get nil t)
    (url-retrieve
     "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
     (lambda (s)
       (let (el-get-master-branch)
	 (goto-char (point-max))
	 (eval-print-last-sexp))
       )))

  (setq el-get-sources
	'((:name zencoding-mode
		 :after (progn (my-sgml-init)))
	  (:name magit
		 :after (progn (my-git-init)))
	  (:name yasnippet
		 :after (progn (my-yasnippet-init)))
	  (:name quack
		 :after (progn (my-scheme-init)))
	  (:name auto-complete
		 :features auto-complete
		 :after (progn (my-auto-complete-init)))
	  (:name evil
		 :after (progn (my-evil-init))
		 :features nil)
	  (:name nxhtml
		 :after (progn (my-nxhtml-init))
		 :load nil)
	  (:name markdown-mode
		 :after (progn (my-markdown-mode-init)))
	  (:name gtags
		 :after (progn (my-gtags-init)))
	  ))

  (setq my-packages (append '(el-get package pos-tip cssh switch-window vkill xcscope) (mapcar 'el-get-source-name el-get-sources)))

  (el-get 'sync my-packages)
  )
(my-el-get-init)
;; ########## end ##########

;;; init.el for emacs ends here
