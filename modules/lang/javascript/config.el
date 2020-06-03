;;; lang/javascript/config.el -*- lexical-binding: t; -*-

(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :config
  (setq js2-skip-preprocessor-directives t
        js2-highlight-external-variables nil
        js2-mode-show-parse-errors nil)

  (add-hook! 'js2-mode-hook
    #'(flycheck-mode rainbow-delimiters-mode))

  (set! :repl 'js2-mode #'+javascript/repl)
  (set! :electric 'js2-mode :chars '(?\} ?\) ?.))
  (set! :jump 'js2-mode :xref-backend #'xref-js2-xref-backend)

  ;; Conform switch-case indentation to js2 normal indent
  (defvaralias 'js-switch-indent-offset 'js2-basic-offset)

  (sp-with-modes '(js2-mode rjsx-mode)
    (sp-local-pair "/* " " */" :post-handlers '(("| " "SPC"))))

  ;; If it's available globally, use eslint_d
  (setq flycheck-javascript-eslint-executable (executable-find "eslint_d"))

  (defun +javascript|init-flycheck-eslint ()
    "Favor local eslint over global installs and configure flycheck for eslint."
    (when (derived-mode-p 'js-mode)
      (when-let* ((exec-path (list (doom-project-expand "node_modules/.bin")))
                  (eslint (executable-find "eslint")))
        (setq-local flycheck-javascript-eslint-executable eslint))
      (when (flycheck-find-checker-executable 'javascript-eslint)
        ;; Flycheck has it's own trailing command and semicolon warning that was
        ;; conflicting with the eslint settings.
        (setq-local js2-strict-trailing-comma-warning nil)
        (setq-local js2-strict-missing-semi-warning nil))))
  (add-hook 'flycheck-mode-hook #'+javascript|init-flycheck-eslint))


;; A find-{definition,references} backend for js2-mode. NOTE The xref API is
;; unstable and may break with an Emacs update.
(use-package xref-js2
  :commands xref-js2-xref-backend)

(use-package tern
  :hook (js2-mode . tern-mode)
  :config
  (advice-add #'tern-project-dir :override #'doom-project-root))

(use-package company-tern
  :after tern
  :config
  (set! :company-backend 'js2-mode '(company-tern)))


(use-package rjsx-mode
  :commands rjsx-mode
  :mode "\\.jsx$"
  :mode "components/.+\\.js$"
  :init
  (defun +javascript-jsx-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "js")
         (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                            magic-mode-regexp-match-limit t)
         (progn (goto-char (match-beginning 1))
                (not (sp-point-in-string-or-comment)))))

  (push (cons #'+javascript-jsx-file-p 'rjsx-mode) magic-mode-alist)

  :config
  (set! :electric 'rjsx-mode :chars '(?\} ?\) ?. ?>))

  ;; disable electric keys (I use snippets and `emmet-mode' instead)
  (define-key rjsx-mode-map (kbd "<") nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)

  (add-hook! rjsx-mode
    ;; jshint doesn't really know how to deal with jsx
    (push 'javascript-jshint flycheck-disabled-checkers)))


(use-package coffee-mode
  :mode "\\.coffee$"
  :init (setq coffee-indent-like-python-mode t))


(use-package web-beautify
  :commands web-beautify-js)


(use-package eslintd-fix
  :commands (eslintd-fix-mode eslintd-fix))


;;
;; Skewer-mode
;;

(use-package skewer-mode
  :commands (skewer-mode run-skewer))

(use-package skewer-css ; in skewer-mode
  :commands skewer-css-mode)

(use-package skewer-html ; in skewer-mode
  :commands skewer-html-mode)


;;
;; Projects
;;

(def-project-mode! +javascript-screeps-mode
  :match "/screeps\\(-ai\\)?/.+$"
  :modes (+javascript-npm-mode)
  :add-hooks (+javascript|init-screeps-mode)
  :on-load (load! +screeps))

(def-project-mode! +javascript-gulp-mode
  :files "gulpfile.js")

(def-project-mode! +javascript-npm-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files "package.json"
  :on-enter
  (when (make-local-variable 'exec-path)
    (push (doom-project-expand "node_modules/.bin")
          exec-path)))


;;
;; Tools
;;

(def-project-mode! +javascript-eslintd-fix-mode
  :add-hooks (eslintd-fix-mode))

