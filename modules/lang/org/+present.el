;;; lang/org/+present.el -*- lexical-binding: t; -*-

(defvar +org-present-text-scale 7
  "The `text-scale-amount' for `org-tree-slide-mode'.")


;;
;; Plugins
;;

(use-package ox-reveal
  :defer t
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"
        org-reveal-mathjax t))


(use-package org-tree-slide
  :commands org-tree-slide-mode
  :config
  (org-tree-slide-simple-profile)
  (setq org-tree-slide-skip-outline-level 2
        org-tree-slide-activate-message " "
        org-tree-slide-deactivate-message " "
        org-tree-slide-modeline-display nil)

  (add-hook! 'org-tree-slide-mode-after-narrow-hook
    #'(+org-present|detect-slide +org-present|add-overlays org-display-inline-images))

  (add-hook 'org-tree-slide-mode-hook #'+org-present|init-org-tree-window)
  (advice-add #'org-tree-slide--display-tree-with-narrow
              :around #'+org-present*narrow-to-subtree))


(use-package centered-window-mode
  :commands centered-window-mode)
