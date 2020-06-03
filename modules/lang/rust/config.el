;;; lang/rust/config.el -*- lexical-binding: t; -*-

(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

(def-package! rustic
  :mode ("\\.rs$" . rustic-mode)
  :commands (rustic-run-cargo-command rustic-cargo-outdated)
  :init
  (after! org-src
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config
  (setq rustic-lsp-server (quote rust-analyzer)
        rustic-lsp-client (quote lsp-mode)
        rustic-lsp-format t)

  (define-key lsp-mode-map (kbd "C-x C-d") #'lsp-describe-thing-at-point)

  (after! rustic-flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy)))
