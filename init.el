;; This file is not part of gnu emacs
;; Time-stamp: <2010-06-12 09:59:12 vmlinz>

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

;; 1.Brand new emacs configuration for TeXing and c/c++ programming
;; 2.Let's keep it really simple and easy always
;; 3.Maybe I will restruct these code to get it more structured and maitainable
;; 4.The emacs i use is debian specifical, maybe I have to customize it through ;; debian source code
;; 5.tweak the third party lisp package configurations and make it more structured
;; 6.speed up startup time by make defuns and hooks
;; 7.the configuration contains some machine specifical settings for Lenovo X200

;; add local elisp packages to load path
;; (defun add-subdirs-to-load-path (dir)
;;   "Recursive add directories to `load-path'."
;;   (let ((default-directory (file-name-as-directory dir)))
;;   (add-to-list 'load-path dir)
;;   (normal-top-level-add-subdirs-to-load-path))
;;   )

;; this setting is annoying, it add all sub dirs to load path
;; (add-subdirs-to-load-path "~/.emacs.d/site-lisp/")

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
  (tabbar-mode )
  )
(add-hook 'after-make-frame-hook 'my-set-frame-font)

;; locales
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
  ;; emacs shell color encoding
  (ansi-color-for-comint-mode-on)
  )
(my-coding-system-init)
;; ########## end ##########

;; ########## custom ##########
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(bookmark-default-file "~/.emacs.d/.emacs.bmk")
  '(calendar-chinese-all-holidays-flag t)
  '(calendar-view-diary-initially-flag nil)
  '(column-number-mode t)
  '(diary-file "~/.emacs.d/diary.gpg")
  '(font-latex-fontify-sectioning (quote color))
  '(inhibit-startup-screen t)
  '(org-agenda-files (quote ("~/Documents/notes/dailylife.org" "~/Documents/notes/study.org" "~/Documents/notes/work.org")))
  '(show-paren-mode t)
  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
  '(vc-handled-backends (quote (GIT SVN Hg Bzr CVS))))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
;; ########## end ##########

;; ########## cc-mode ##########
;; c mode common hook
(defun my-cc-mode-init()
  (defun my-c-mode-common-hook()
    (add-hook 'compilation-finish-functions
      '(lambda (buf str)
	 (if (string-match "exited abnormally" str)
	   (next-error)
	   ;;no errors, make the compilation window go away in a few seconds
	   (run-at-time "2 sec" nil 'delete-windows-on (get-buffer-create "*compilation*"))
	   (message "No Compilation Errors!")
	   )
	 ))

    (defun do-compile()
      (interactive)
      (message (make-command))
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
    ;; (c-set-offset arglist-cont-nonempty +)
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
  ;; ########## linux kernel coding style ##########
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
	    (column (c-langelem-2nd-pos c-syntactic-element))
	    (offset (- (1+ column) anchor))
	    (steps (floor offset c-basic-offset)))
      (* (max steps 1)
	c-basic-offset)))

  (add-hook 'c-mode-common-hook
    (lambda ()
      ;; Add kernel style
      (c-add-style
	"linux-tabs-only"
	'("linux" (c-offsets-alist
		    (arglist-cont-nonempty
		      c-lineup-gcc-asm-reg
		      c-lineup-arglist-tabs-only))))))

  (add-hook 'c-mode-hook
    (lambda ()
      (let ((filename (buffer-file-name)))
	;; Enable kernel mode for the appropriate files
	(when (and filename
		(string-match (expand-file-name "~/Projects/kernel")
		  filename))
	  (setq indent-tabs-mode t)
	  (c-set-style "linux-tabs-only")))))
  )
;; ########## end ##########
(my-cc-mode-init)
;; ########## end ##########

;; ########## emacs server ##########
;; only start emacs server when it's not started, I hate warnings.
(defun my-emacs-daemon-init()
  (setq server-socket-file "/tmp/emacs1000/server")
  (unless (file-exists-p server-socket-file)
    (server-start))
  ;; exit emacs client
  (defun my-exit-emacs-client ()
    "consistent exit emacsclient.
   if not in emacs client, echo a message in minibuffer, don't exit emacs.
   if in server mode
      and editing file, do C-x # server-edit
      else do C-x 5 0 delete-frame"
    (interactive)
    (if server-buffer-clients
      (server-edit)
      (delete-frame)))
  (global-set-key (kbd "C-c C-q") 'my-exit-emacs-client)
  )
(my-emacs-daemon-init)
;; ########## end ##########

;; ########## various ##########
(defun my-misc-custom-init()
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; time format
  (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (display-time)
  (add-hook 'before-save-hook 'time-stamp)
  ;; delete trailing whitespaces before save
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (setq x-select-enable-clipboard t)
  (setq use-dialog-box nil)
  ;; set my email address
  (setq user-mail-address "vmlinz@gmail.com")
  (auto-image-file-mode t)
  ;; scrollbar
  (set-scroll-bar-mode 'right)
  (setq
    scroll-margin 0
    scroll-conservatively 100000
    scroll-preserve-screen-position 1)
  ;; check last line to be a newline
  (setq require-final-newline t)
  ;; set insert parenthese without space
  (setq parens-require-spaces nil)
  )
(my-misc-custom-init)
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
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-;") 'dabbrev-expand)
  )
(my-hippie-expand-init)
;;########## end ##########

;;########## key bindings ##########
;; buffer switching
;; some keybindings here are specifical to Lenovo Thinkpad
(defun my-key-init()
  (global-set-key (kbd "C-x p") 'previous-buffer)
  (global-set-key [(XF86Back)] 'previous-buffer)
  (global-set-key (kbd "C-x n") 'next-buffer)
  (global-set-key [(XF86Forward)] 'next-buffer)
  (global-set-key "\C-c\C-c" 'comment-dwim)
  )
(my-key-init)
;; ########## end ##########

;; ########## auctex ##########
;; default to xelatex
;; exec-path for texlive2009 from CTAN
(defun my-auctex-init()
  (add-hook 'LaTeX-mode-hook
    '(lambda()
       (setenv "PATH" (concat (getenv "PATH") ":/usr/local/texlive/2009/bin/x86_64-linux/"))
       (add-to-list 'exec-path "/usr/local/texlive/2009/bin/x86_64-linux/")
       (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
       (setq TeX-command-default "XeLaTeX")
       (setq TeX-save-query nil)
       (setq TeX-show-compilation t)
       (setq Tex-master nil)
       )
    )
  ;; set preview application and preview style
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
       ;; minor modes
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

;; ########## git ##########
;; git contrib, various git controls
;; git.el, git-blame.el and magit.el give me git support
;; I installed maigt from debian apt
(defun my-git-init()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/git-contrib/")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/magit/")
  (require 'git)
  (require 'git-blame)

  (autoload 'magit-status "magit" nil t)
  (eval-after-load 'magit
    '(progn
       (set-face-foreground 'magit-diff-add "green3")
       (set-face-foreground 'magit-diff-del "red3")))
  )
(my-git-init)
;; ########## end #########

;; ########## emms ##########
;; now just ignore it, for programming and daily use come first
;; ########## end ##########

;; ########## markdown ##########
;; markdown-mode for translating Pro Git
(defun my-markdown-mode-init()
  (autoload 'markdown-mode "markdown-mode.el"
    "Major mode for editing Markdown files" t)
  (setq auto-mode-alist
    (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
  )
(my-markdown-mode-init)
;; ########## end ##########

;; ########## notify ##########
;; use libnotify and mplayer for sound
(defun my-notify-send (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  (when sound (shell-command
		(concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
    (shell-command (concat "notify-send "
		     (if icon (concat "-i " icon) "")
		     " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))
;; ########## end ##########

;; ########## ido ##########
(ido-mode t)
(setq ido-enable-prefix nil
  ido-enable-flex-matching t
  ido-create-new-buffer 'always
  ido-use-filename-at-point 'guess
  ido-max-prospects 10)
;; ########## end ##########

;; ########## fullscreen ##########
;; fullscreen
(defun my-fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
    (if (frame-parameter f 'fullscreen) nil 'fullboth)))

(global-set-key [f11] 'my-fullscreen)
;; ########## end ##########

;; ########## turn off menu-bar ##########
;; turn off menu-bar when in terminal
(if (not (eq (window-system) 'x))
  (menu-bar-mode -1)
  nil
  )
;; ########## end ##########

;; ########## cedet ##########
;; cedet frome cvs configured for c/c++ programming
;; cedet' stable functions is going to be integrited in emacs 23.2
;; NOTE: cedet from cvs has more functions than the integrited one but it's not ;; as clean as the one integrited
;;
;; (add-to-list 'load-path (expand-file-name "/home/vmlinz/Projects/emacs/site-lisp/cedet/common"))
(add-to-list 'load-path "~/.emacs.d/site-lisp/cedet/common/")
(require 'cedet)

(defun my-semantic-hook ()
  "feature setting hook for semantic"
  (require 'semantic-ia)
  (require 'semantic-gcc)

  (imenu-add-to-menubar "TAGS")
  (global-ede-mode t)
  ;; srecode-minor-mode
  (srecode-minor-mode 1)
  (semantic-load-enable-code-helpers)
  ;;(semantic-load-enable-gaudy-code-helpers)
  (global-semantic-show-unmatched-syntax-mode -1)
  (global-semantic-tag-folding-mode -1)
  (global-semantic-highlight-func-mode 1)
  ;;(global-semantic-decoration-mode 1)

  (semantic-load-enable-all-exuberent-ctags-support)

  (setq-mode-local c-mode semanticdb-find-default-throttle
    '(project unloaded system recursive))
  (setq-mode-local c++-mode semanticdb-find-default-throttle
    '(project unloaded system recursive))
  )
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(defun my-semantic-key-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c/" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cd" 'semantic-ia-show-doc)
  (local-set-key "\C-cm" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)

  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'semantic-init-hooks 'my-semantic-key-hook)

(defun my-semanticdb-hook ()
  "semanticdb hook"
  (require 'semanticdb)
  (global-semanticdb-minor-mode 1)
  (setq semanticdb-default-save-directory "/home/vmlinz/.emacs.d/semanticdb")
  (setq semanticdb-search-system-databases t)
  )
(add-hook 'semantic-init-hooks 'my-semanticdb-hook)

(defun my-semantic-qt4-hook()
  "qt4 setting hook for semantic"
  ;; qt4 programming settings
  (setq qt4-base-dir "/usr/include/qt4")
  (semantic-add-system-include qt4-base-dir 'c++-mode)
  (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))
  )
(add-hook 'semantic-init-hooks 'my-semantic-qt4-hook)
;; ########## end ##########

;; ########## yasnippet ##########
;; yasnippet
(defun my-yasnippet-init()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet/")
  (require 'yasnippet)
  (setq yas/root-directory "~/.emacs.d/site-lisp/yasnippet/snippets")
  (yas/load-directory yas/root-directory)
  ;; set yas menu to abbreviate mode
  (setq yas/use-menu 'abbreviate)
  )
(my-yasnippet-init)
;; ########## end ##########

;; ########## auto-complete ##########
;; this package is good to use and easy to config
;; now without cedet semantic support, it will be add next
(defun my-auto-complete-init()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete/")
  (require 'auto-complete)
  (require 'auto-complete-config)
  ;; ac default configuration
  (ac-config-default)
  ;; ac customizations
  (set-face-background 'ac-candidate-face "lightgray")
  (set-face-underline-p 'ac-candidate-face "darkgray")
  (set-face-background 'ac-selection-face "steelblue")
  (define-key ac-completing-map "\M-n" 'ac-next)
  (define-key ac-completing-map "\M-p" 'ac-previous)
  (setq ac-dwim t)
  (define-key ac-mode-map [(C-tab)] 'auto-complete)
  ;;start completion when entered 3 characters
  (setq ac-auto-start 3)
  (setq ac-use-quick-help nil)
  )
(my-auto-complete-init)
;; ########## end ##########

;; ########## woman ##########
;; settings for woman
(add-hook 'after-init-hook
  '(lambda()
     (setq woman-use-own-frame nil)
     (setq woman-fill-column 80)
     ))
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

;; ########## lisp settings ##########
;; lisp indent offset to 2
(add-hook 'emacs-lisp-mode-hook
  '(lambda()
     (setq lisp-indent-offset 2))
  )
;; byte compile emacs init file and load it
(global-set-key [f12]
  '(lambda()
     (interactive)
     (byte-compile-file "~/.emacs.d/init.el" t)
     ))
;; ########## end ##########

;; ########## android-mode #########
(defun my-android-mode-init()
  (add-to-list 'load-path "~/.emacs.d/site-lisp/android-mode/")
  (require 'android-mode)
  (setq android-mode-sdk-dir "~/Projects/android-sdk-linux_86/")
  (setq android-mode-ndk-dir "~/Projects/android/ndks/android-ndk-r4/")
  ;; set default emulator
  (setq android-mode-avd "android-4")
  ;; set sdk tools and ndk tools to path env
  (setenv "PATH" (concat
		   (getenv "PATH")
		   (concat ":" (expand-file-name android-mode-sdk-dir) "tools")
		   (concat ":" (expand-file-name android-mode-ndk-dir))))
  ;; set sdk tools and ndk tools to emacs exec path
  (add-to-list 'exec-path (expand-file-name (concat android-mode-sdk-dir "tools")))
  (add-to-list 'exec-path (expand-file-name android-mode-ndk-dir))
  )
(my-android-mode-init)
;; ########## end ##########

;;; init.el for emacs ends here
