;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; requires node npm tern js-beautify eslint eslint-plugin-react

(package! js2-mode)
(package! tern)
(package! web-beautify)
(package! skewer-mode)
(package! eslintd-fix)

(when (featurep! :completion company)
  (package! company-tern))

(when (featurep! :feature jump)
  (package! xref-js2))

