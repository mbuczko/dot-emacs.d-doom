;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode flycheck-list-errors flycheck-buffer)
  :hook (prog-mode . flycheck-mode)
  :config
  ;; Check only when saving or opening files. Newline & idle checks are a mote
  ;; excessive and can catch code in an incomplete state, producing false
  ;; positives, so we removed them.
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch))

  ;; For the above functionality, check syntax in a buffer that you switched to
  ;; only briefly. This allows "refreshing" the syntax check state for several
  ;; buffers quickly after e.g. changing a config file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25))
