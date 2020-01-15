;;; completion/fzf/config.el -*- lexical-binding: t; -*-

;;
;; Packages
;;

(def-package! fzf
  :defer t
  :commands (fzf fzf-git)
  :config
  (define-key global-map (kbd "C-x C-o") #'fzf-git))
