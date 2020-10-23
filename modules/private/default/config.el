;;; private/default/config.el -*- lexical-binding: t; -*-

(load! +customs)
(load! +bindings)
(load! +hydras)

;;
;; Plugins
;;

(use-package emacs-snippets
  :after yasnippet)

(use-package powerthesaurus
  :commands (powerthesaurus-lookup-word))

(use-package github-stars
  :commands (github-stars-browse-url))

;;
;; Config
;;

(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))

(use-package nuke-buffers
  :commands (nuke-buffers))
