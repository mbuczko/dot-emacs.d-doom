;;; feature/jump/config.el -*- lexical-binding: t; -*-

(set! :popup "*xref*" :noselect t :autokill t :autoclose t)

;; Recenter after certain jumps
(add-hook! '(imenu-after-jump-hook  dumb-jump-after-jump-hook) #'recenter)

;;
;; Packages
;;

(def-package! ivy-xref
  :when (featurep! :completion ivy)
  :after xref
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))


(def-package! helm-xref
  :when (featurep! :completion helm)
  :after xref
  :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs))


(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look
             dumb-jump-back dumb-jump-result-follow)
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-selector (cond ((featurep! :completion ivy) 'ivy)
                                 ((featurep! :completion helm) 'helm)
                                 (t 'popup))))

(def-package! deadgrep
  :commands (deadgrep))

(def-package! dash-at-point
  :commands (dash-at-point))

(def-package! goto-last-change
  :commands (goto-last-change))
