;;; lang/perl/config.el -*- lexical-binding: t; -*-

;; There's also `perl-mode' for perl < 6, which is already set up.
(add-hook 'perl-mode-hook #'flycheck-mode)


(use-package perl6-detect)


(use-package flycheck-perl6
  :after perl6-mode
  :config (add-hook 'perl6-mode-hook #'flycheck-mode))
