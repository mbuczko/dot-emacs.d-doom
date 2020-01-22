;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(def-package! markdown-mode
  :mode "/README$"
  :mode "\\.m\\(d\\|arkdown\\)$"
  :mode ("/README\\.md$" . gfm-mode)
  :init
  (setq markdown-enable-wiki-links t
        markdown-enable-math t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil) ; trigger with `markdown-toggle-url-hiding'

  :config
  (add-hook! markdown-mode
    (setq line-spacing 2
          fill-column 80)))

(def-package! markdown-toc
  :commands markdown-toc-generate-toc)

