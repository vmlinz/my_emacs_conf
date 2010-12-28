;; This file is not part of gnu emacs
;; Time-stamp: <2010-12-29 00:06:37 vmlinz>

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

;; TODOS:
;; 1.Configure cedet and auto-complete
;; 2.Get to know more about elisp programming
;; 3.Make it portable between computers

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
(if (window-system)
  (add-hook 'after-make-frame-hook 'my-set-frame-font)
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
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(calendar-chinese-all-holidays-flag t)
  '(calendar-view-diary-initially-flag nil)
  '(column-number-mode nil)
  '(font-latex-fontify-sectioning (quote color))
  '(inhibit-startup-screen t)
  '(show-paren-mode t)
  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
  '(vc-handled-backends (quote (GIT SVN Hg Bzr CVS)))
  ;; depend on personal settings
  '(bookmark-default-file "~/.emacs.d/.emacs.bmk")
  '(diary-file "~/.emacs.d/diary.gpg")
  '(org-agenda-files (quote ("~/Documents/notes/dailylife.org"
			      "~/Documents/notes/study.org"
			      "~/Documents/notes/work.org")))
  )

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
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
  (tabbar-mode -1)
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
  ;; ########## end ##########

  ;; ########## fullscreen ##########
  ;; fullscreen
  (defun my-fullscreen (&optional f)
    (interactive)
    (set-frame-parameter f 'fullscreen
      (if (frame-parameter f 'fullscreen) nil 'fullboth)))

  (global-set-key [f11] 'my-fullscreen)
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
  ;; ########## end ##########
  )
(my-misc-custom-init)
;; ########## end ##########

;; ########## emacs server ##########
;; only start emacs server when it's not started, I hate warnings.
(defun my-emacs-daemon-init()
  (setq server-socket-file
    (concat "/tmp/emacs"
      (int-to-string (user-uid)) "/server"))

  (unless (file-exists-p server-socket-file)
    (server-start))

  (defun my-server-force-start ()
    "delete current server socket and restart the server"
    (interactive)
    (progn
      '(
	 (server-force-delete)
	 (server-start))))

  (defun my-server-kill-client ()
    "consistent exit emacsclient"
    (interactive)
    (if server-buffer-clients
      (server-edit)
      (delete-frame)))
  (global-set-key (kbd "C-c C-q") 'my-server-kill-client)
)
(my-emacs-daemon-init)
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
  (global-set-key (kbd "C-x p") 'previous-buffer)
  (global-set-key [(XF86Back)] 'previous-buffer)
  (global-set-key (kbd "C-x n") 'next-buffer)
  (global-set-key [(XF86Forward)] 'next-buffer)
  ;; comment-dwim
  (global-set-key "\C-c\C-c" 'comment-dwim)
  (global-set-key "\C-x\C-b" 'ibuffer)
  ;; set mark
  (global-set-key "\M-m" 'set-mark-command)
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
  (org-remember-insinuate)
  (setq org-directory "~/Documents/notes")
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-log-done 'note)
  (setq remember-annotation-functions '(org-remember-annotation))
  (setq remember-handler-functions '(org-remember-handler))
  ;;custome commands for the use of GTD.
  (setq org-agenda-custom-commands
    '(("w" todo "WAITING" nil)
       ("n" todo "NEXT" nil)
       ("d" "Agenda + Next Actions" ((agenda) (todo "NEXT"))))
    )
  (add-hook 'remember-mode-hook 'org-remember-apply-template)
  (global-set-key "\C-cr" 'org-remember)
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
  (setq indent-tabs-mode t)
  (c-set-style "linux-tabs-only")
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
       )
    )
  (add-hook 'java-mode-hook
    '(lambda ()
       (setq indent-tabs-mode nil)
       (c-set-offset 'arglist-cont-nonempty '+)
       (c-set-offset 'topmost-intro-cont 0)
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

;; ########## markdown ##########
;; markdown-mode for translating Pro Git
(defun my-markdown-mode-init()
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
    (cons '("\\.markdown\\'" . markdown-mode) auto-mode-alist))
  )
(my-markdown-mode-init)
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

;; ########## cedet ##########
;; cedet is now managed by _el-get_ using latest source from cvs
(defun my-semantic-hook ()
  "feature setting hook for semantic"
  (require 'semantic-ia)
  (require 'semantic-gcc)

  (imenu-add-to-menubar "TAGS")
  (global-ede-mode t)
  (global-srecode-minor-mode 1)

  (semantic-load-enable-code-helpers)
  (global-semantic-highlight-func-mode 1)
  (global-semantic-show-unmatched-syntax-mode -1)
  (global-semantic-tag-folding-mode -1)
  (global-semantic-idle-scheduler-mode -1)
  )

(defun my-semantic-key-hook ()
  "key bindings for semantic modes"
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-c\C-r" 'semantic-symref)

  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cd" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)

  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-cm" 'eassist-list-methods)
  )

(defun my-semanticdb-hook ()
  "semanticdb hook"
  (setq semanticdb-default-save-directory "/home/vmlinz/.emacs.d/semanticdb")
  (setq semanticdb-search-system-databases t)

  (setq-mode-local c-mode semanticdb-find-default-throttle
    '(project unloaded system recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle
    '(project unloaded system recursive))

  (semantic-load-enable-primary-exuberent-ctags-support)
  )

(defun my-semantic-init()
  "all in one semantic init function"
  (progn
    (add-hook 'semantic-init-hooks 'my-semantic-hook)
    (add-hook 'semantic-init-hooks 'my-semanticdb-hook)
    (add-hook 'semantic-init-hooks 'my-semantic-key-hook)
    )
  )
;; ########## end ##########

;; ########## lisp settings ##########
;; lisp indent offset to 2
(add-hook 'emacs-lisp-mode-hook
  '(lambda()
     (setq lisp-indent-offset 2)
     ))
;; byte compile emacs init file and load it
(global-set-key [f12]
  '(lambda()
     (interactive)
     (byte-compile-file "~/.emacs.d/init.el" t)
     ))
;; ########## end ##########

;; ########## scheme mode ##########
;; scheme mode setup
(defun my-scheme-init()
  (require 'quack)
  (setq quack-global-menu-p nil)
  (setq quack-default-program "guile")
  (quack-install)

  (add-hook 'scheme-mode-hook
    '(lambda ()
       (define-key scheme-mode-map "\C-x\M-r" 'run-scheme)
       (setq scheme-program-name "guile")
       ))
)
(my-scheme-init)
;; ########## end ##########

;; ########## yasnippet ##########
;; yasnippet is now managed by _el-get_
(defun my-yasnippet-init()
  "simple yasnippet mode init function"
  (require 'yasnippet)
  (setq yas/root-directory "~/.emacs.d/el-get/yasnippet/snippets")
  (yas/load-directory yas/root-directory)
  (setq yas/use-menu 'abbreviate)
  (setq yas/prompt-functions
    (cons 'yas/ido-prompt
      (remove 'yas/ido-prompt
	yas/prompt-functions)))
  (yas/global-mode t)
  )
;; ########## end ##########

;; ########## auto-complete ##########
;; this package is good to use and easy to config
(defun my-ac-semantic-setup ()
  "semantic source configuration for auto-complete"
  (add-hook 'semantic-init-hook
    '(lambda ()
       (setq ac-sources
	 (append '(ac-source-semantic ac-source-semantic-raw) ac-sources))
       (define-key ac-mode-map "\M-/" 'ac-complete-semantic-raw)
       )
    )
  )

(defun my-ac-clang-setup ()
  "clang source configuration for auto-complete"
  (require 'auto-complete-clang)
  (add-hook 'c-mode-common-hook
    '(lambda ()
       (setq ac-sources
	 (append '(ac-source-clang) ac-sources))
       (setq ac-clang-flags
	 (split-string
	   (shell-command-to-string "pkg-config --cflags gtk+-2.0")))
       (define-key ac-mode-map "\M-/" 'ac-complete-clang)
       )
    )
  )

(defun my-auto-complete-init()
  "auto-complete init function"
  (defvar my-ac-use-semantic nil
    "make auto-complete to use semantic to complete")
  (if my-ac-use-semantic
    (add-hook 'auto-complete-mode-hook 'my-ac-semantic-setup)
    (add-hook 'auto-complete-mode-hook 'my-ac-clang-setup)
    )

  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
    "~/.emacs.d/el-get/auto-complete/dict")
  (ac-config-default)

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
       )))
(my-py-init)
;; ########## end ##########

;; ########## el-get ##########
;; the great package management tool el-get
(defun my-el-get-init()
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get/")
  (require 'el-get)
  (setq el-get-sources
    '(el-get
       package
       pos-tip
       cssh
       sicp
       (:name magit
	 :build ("make")
	 :after (lambda () (my-git-init)))
       (:name yasnippet
	 :build ("rake compile")
	 :after (lambda () (my-yasnippet-init)))
       (:name cedet
	 :features cedet
	 :after (lambda () (my-semantic-init))
	 )
       (:name auto-complete
	 :build ("make")
	 :after (lambda () (my-auto-complete-init)))
       auto-complete-clang
       )
    )
  (el-get 'wait)
  )
(my-el-get-init)
;; ########## end ##########

;;; init.el for emacs ends here
