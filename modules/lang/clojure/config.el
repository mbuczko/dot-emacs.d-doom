;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode "\\.clj$"
  :mode "\\.cljc$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :config
  (require 'helm-clojuredocs)

  ;; treat some-symbol as a single word for editing lispy sources
  (dolist (c (string-to-list ":_-/?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table))

  (define-key clojure-mode-map (kbd "C-x C-d") 'helm-clojuredocs-at-point))

(def-package! clj-refactor
  :after clojure-mode
  :config

  ;; setup some extra namespace auto completion for great awesome
  (dolist (mapping '(("time"      . "clj-time.core")
                     ("try"       . "clj-try.core")
                     ("rf"        . "re-frame.core")
                     ("log"       . "clojure.tools.logging")
                     ("str"       . "clojure.string")
                     ("json"      . "cheshire.core")
                     ("csrf"      . "ring.util.anti-forgery")
                     ("selmer"    . "selmer.parser")
                     ("response"  . "ring.util.response")
                     ("compojure" . "compojure.core")
                     ("liberator" . "liberator.core")
                     ("pp"        . "fipp.edn")))
    (add-to-list 'cljr-magic-require-namespaces mapping t))
  (cljr-add-keybindings-with-prefix "M-l"))


(def-package! cider
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  ;; settings for cider repl as a popup (prevent it from being closed on escape, especially.)
  (set! :popup "^\\*cider" :regexp t :noselect t :noesc t)

  (define-key cider-repl-mode-map (kbd "C-x C-d") 'helm-clojuredocs-at-point)
  (define-key cider-repl-mode-map (kbd "C-x C-p") 'cider-repl-previous-matching-input)
  (define-key cider-repl-mode-map (kbd "M-r")     'cider-switch-repl))

(def-package! cider-find
  :commands (cider-find-resource cider-find-ns))

(def-package! cider-scratch
  :commands (cider-scratch))

(def-package! cider-ns
  :commands (cider-ns-refresh))

