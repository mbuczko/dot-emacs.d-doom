;;; lang/web/config.el -*- lexical-binding: t; -*-

(load! +html)
(load! +css)


(def-package! web-beautify
  :commands (web-beautify-html web-beautify-css))

(def-package! emmet-mode
  :commands emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode)
  :config (setq emmet-move-cursor-between-quotes t))
