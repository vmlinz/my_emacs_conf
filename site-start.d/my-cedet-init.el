;; ########## cedet ##########
;; cedet is now managed by _el-get_ using latest source from bzr
;; NOTE: cedet config should be put at the top of the emacs init file
;; according to cedet INSTALL
(defun my-cedet-semantic-submodes-setup ()
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode))

(defun my-cedet-misc-setup ()
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  ;; (global-ede-mode 1)
  ;; (ede-enable-generic-projects)
  )

(defun my-cedet-semantic-key-hook ()
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))

(defun my-c-mode-cedet-hook ()
  (semantic-gcc-setup)
  (semantic-default-c-setup))

(defun my-scheme-mode-cedet-hook ()
  (semantic-default-scheme-setup))

(defun my-cedet-hooks-setup ()
  (add-hook 'semantic-init-hooks 'my-cedet-semantic-key-hook)
  (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
  (add-hook 'scheme-mode-hook 'my-scheme-mode-cedet-hook))

(defun my-cedet-setup()
  "all in one semantic init function"
  (require 'semantic)
  (require 'srecode)

  (my-cedet-semantic-submodes-setup)
  (my-cedet-misc-setup)
  (my-cedet-hooks-setup)

  (semantic-mode 1)
  (srecode-minor-mode 1))

(provide 'my-cedet-init)
;; ########## end ##########
