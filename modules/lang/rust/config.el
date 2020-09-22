;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :commands (rustic-run-cargo-command rustic-cargo-outdated)
  :init
  (after! org-src
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config
  (require 'helm-lsp)
  (direnv-mode t)
  (define-key rustic-mode-map (kbd "C-x C-d") #'lsp-describe-thing-at-point)
  (define-key rustic-mode-map (kbd "C-c n")   #'helm-lsp-workspace-symbol)
  (define-key rustic-mode-map (kbd "C-c c")   #'helm-lsp-code-actions)
  (setq rustic-lsp-server (quote rust-analyzer)
        rustic-lsp-client (quote lsp-mode)
        rustic-lsp-format t)
  (after! rustic-flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy)))

(use-package lsp-completion)

(use-package lsp-modeline
  :commands (lsp-modeline-diagnostics-mode lsp-modeline-code-actions-mode))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil))
