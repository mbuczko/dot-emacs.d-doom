;;; lang/data/config.el -*- lexical-binding: t; -*-

(push '("/sxhkdrc" . conf-mode) auto-mode-alist)


(use-package nxml-mode
  :mode "\\.plist$"
  :config
  (set! :company-backend 'nxml-mode '(company-nxml company-yasnippet)))


(use-package toml-mode :mode "\\.toml$")

(use-package yaml-mode :mode "\\.ya?ml$")

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)$"
  :config
  (set! :electric 'json-mode :chars '(?\n ?: ?{ ?})))


(use-package vimrc-mode
  :mode "/\\.?g?vimrc$"
  :mode "\\.vim$"
  :mode "\\.?vimperatorrc$"
  :mode "\\.vimp$")


(use-package dockerfile-mode
  :mode "/Dockerfile$")


;; For ROM hacking or debugging
(use-package hexl
  :mode ("\\.hex$" . hexl-mode)
  :mode ("\\.nes$" . hexl-mode))


;;
;; Frameworks
;;

(def-project-mode! +data-ansible-mode
  :modes (yaml-mode)
  :files "roles/")

(def-project-mode! +data-vagrant-mode
  :files "Vagrantfile")

