;; ########## cedet ##########
;; cedet is now managed by _el-get_ using latest source from bzr
;; NOTE: cedet config should be put at the top of the emacs init file
;; according to cedet INSTALL
(defun my-cedet-semantic-hook ()
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode))

(defun my-cedet-semantic-key-hook ()
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

(defun my-cedet-misc-hook ()
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  ;; (global-ede-mode 1)
  ;; (ede-enable-generic-projects)
  )

(defun my-cedet-init-hooks()
  "all in one semantic init function"
  (require 'semantic)
  (require 'srecode)

  (progn
    (add-hook 'semantic-init-hooks 'my-cedet-semantic-hook)
    (add-hook 'semantic-init-hooks 'my-cedet-semantic-key-hook)
    (add-hook 'semantic-init-hooks 'my-cedet-misc-hook)
    (semantic-mode 1)
    (srecode-minor-mode 1)))

(defun my-c-mode-cedet-hook ()
  (semantic-gcc-setup)
  (semantic-default-c-setup))

(defun my-scheme-mode-cedet-hook ()
  (semantic-default-scheme-setup))

(defun my-cedet-setup ()
  (add-hook 'c-mode-common-hook 'my-cedet-init-hooks)
  (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
  (add-hook 'scheme-mode-hook 'my-cedet-init-hooks)
  (add-hook 'scheme-mode-hook 'my-scheme-mode-cedet-hook)

  (add-hook 'lisp-mode-hook 'my-cedet-init-hooks)
  (add-hook 'emacs-lisp-mode-hook 'my-cedet-init-hooks))

(provide 'my-cedet-init)
;; ########## end ##########
