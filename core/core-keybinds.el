;;; core-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' will be ignored.

(use-package which-key
  :config
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  ;; embolden local bindings
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'doom-init-hook #'which-key-mode))


(use-package hydra
  :init
  ;; In case I later need to wrap defhydra in any special functionality.
  (defalias 'def-hydra! 'defhydra)
  (defalias 'def-hydra-radio! 'defhydradio)
  :config
  (setq lv-use-seperator t))


(use-package pretty-hydra
  :commands (pretty-hydra-define))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
