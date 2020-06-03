;;; lang/web/+html.el -*- lexical-binding: t; -*-

(use-package web-mode
  :mode "\\.p?html?$"
  :mode "\\.\\(tpl\\|blade\\)\\(\\.php\\)?$"
  :mode "\\.erb$"
  :mode "\\.jsp$"
  :mode "\\.as[cp]x$"
  :mode "\\.mustache$"
  :mode "\\.tsx$"
  :mode "wp-content/themes/.+/.+\\.php$"
  :config
  (add-hook 'web-mode-hook #'turn-off-smartparens-mode)
  (setq web-mode-enable-html-entities-fontification t))

(use-package company-web
  :after web-mode)

(use-package emmet-mode
  :after web-mode)

(use-package haml-mode
  :mode "\\.haml$")

(use-package pug-mode
  :mode "\\.jade$"
  :mode "\\.pug$"
  :config
  (set! :company-backend 'pug-mode '(company-yasnippet)))
