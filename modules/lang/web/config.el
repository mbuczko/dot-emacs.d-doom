;;; lang/web/config.el -*- lexical-binding: t; -*-

(load! +html)
(load! +css)


(use-package web-beautify
  :commands (web-beautify-html web-beautify-css))

(use-package emmet-mode
  :commands emmet-mode
  :preface (defvar emmet-mode-keymap (make-sparse-keymap))
  :hook (css-mode web-mode html-mode haml-mode nxml-mode rjsx-mode)
  :config (setq emmet-move-cursor-between-quotes t))
