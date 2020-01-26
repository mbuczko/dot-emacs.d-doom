;;; lang/web/+html.el -*- lexical-binding: t; -*-

(def-package! web-mode
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

(def-package! company-web
  :when (featurep! :completion company)
  :after web-mode)

(def-package! emmet-mode
  :after web-mode)

(def-package! haml-mode :mode "\\.haml$")

(def-package! pug-mode
  :mode "\\.jade$"
  :mode "\\.pug$"
  :config
  (set! :company-backend 'pug-mode '(company-yasnippet)))
