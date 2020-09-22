;;; feature/jump/config.el -*- lexical-binding: t; -*-

(set! :popup "*xref*" :noselect t :autokill t :autoclose t)

;; Recenter after certain jumps
(add-hook! '(imenu-after-jump-hook dumb-jump-after-jump-hook) #'recenter)

;;
;; Packages
;;

(use-package helm-xref
  :after xref)

(use-package dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-back dumb-jump-result-follow dumb-jump-xref-activate)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector 'helm))

(use-package deadgrep
  :commands (deadgrep))

(use-package dash-at-point
  :commands (dash-at-point))

(use-package goto-last-change
  :commands (goto-last-change))
