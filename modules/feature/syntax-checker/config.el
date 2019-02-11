;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it, so we do
;; it ourselves:
(autoload 'pkg-info-version-info "pkg-info")

(def-package! flycheck
  :commands (flycheck-mode global-flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  ;; Emacs feels snappier without checks on idle/change
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))
