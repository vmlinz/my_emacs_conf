;; ########## cedet ##########
;; cedet is now managed by _el-get_ using latest source from bzr
;; NOTE: cedet config should be put at the top of the emacs init file
;; according to cedet INSTALL
(defun my-cedet-load ()
  (load-file "~/.emacs.d/el-get/cedet/cedet-devel-load.el")
  (add-to-list 'load-path "~/.emacs.d/el-get/cedet/contrib")
  (add-to-list  'Info-directory-list "~/.emacs.d/el-get/cedet/doc/info")
  )

(defun my-semantic-hook ()
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)

  (require 'semantic/bovine/c)
  (require 'semantic/bovine/gcc)
  (require 'semantic/bovine/clang)
  (require 'semantic/ia)
  (require 'semantic/decorate/include)
  (require 'semantic/lex-spp)
  (require 'eassist)
  )

(defun my-semantic-key-hook ()
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)
  )

(defun my-cedet-misc-hook ()
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)

  (when (cedet-ectag-version-check t)
    (semantic-load-enable-primary-ectags-support))

  (global-srecode-minor-mode 1)

  ;; (global-ede-mode 1)
  ;; (ede-enable-generic-projects)
)

(defun my-semantic-init-hooks()
  "all in one semantic init function"
  (progn
    (add-hook 'semantic-init-hooks 'my-semantic-hook)
    (add-hook 'semantic-init-hooks 'my-semantic-key-hook)
    (add-hook 'semantic-init-hooks 'my-cedet-misc-hook)
    )

  (semantic-mode 1)
  )

(defun my-c-mode-cedet-hook ()
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)

  (add-to-list 'ac-sources 'ac-source-gtags)
  )

(defun my-cedet-setup ()
  (add-hook 'c-mode-common-hook 'my-semantic-init-hooks)
  (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

  (add-hook 'lisp-mode-hook 'my-semantic-init-hooks)
  (add-hook 'scheme-mode-hook 'my-semantic-init-hooks)
  (add-hook 'emacs-lisp-mode-hook 'my-semantic-init-hooks)
  )

(my-cedet-load)
(my-cedet-setup)

(provide 'my-cedet-init)
;; ########## end ##########
