;;; lang/rust/config.el -*- lexical-binding: t; -*-

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :hook ((rustic-mode . lsp-completion-mode)
         (rustic-mode . lsp-ui-sideline-mode)
         (rustic-mode . direnv-mode))
  :bind (:map rustic-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c C-n" . helm-lsp-workspace-symbol)
              ("C-c C-a" . helm-lsp-code-actions))
  :commands (rustic-run-cargo-command rustic-cargo-outdated)
  :init
  (after! org-src
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config
  (setq rustic-lsp-server (quote rust-analyzer)
        rustic-lsp-client (quote lsp-mode)
        rustic-lsp-format t)

  (after! rustic-flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy)))

(use-package lsp-completion)
(use-package lsp-headerline)

(use-package lsp-modeline
  :commands (lsp-modeline-diagnostics-mode lsp-modeline-code-actions-mode))

(use-package lsp-ui
  :config
  (setq
   ;; sideline
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-update-mode 'line
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-diagnostics nil

   ;; lenses
   lsp-lens-enable t

   ;; breadcrumb
   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-enable-diagnostics nil
   lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols)

   ;; xrefs peek
   lsp-ui-peek-enable t

   ;; doc
   lsp-ui-doc-enable nil))
