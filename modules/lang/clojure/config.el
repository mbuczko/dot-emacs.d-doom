;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(def-package! clojure-mode
  :mode "\\.clj$"
  :mode "\\.cljc$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :config
  (require 'helm-clojuredocs)
  (require 'flycheck-joker)

  ;; get docstrings colored correctly with defn-spec
  (put 'defn-spec 'clojure-doc-string-elt 3)

  ;; treat some-symbol as a single word for editing lispy sources
  (dolist (c (string-to-list ":_-/?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table))

  (map! :map clojure-mode-map "C-x C-d" #'helm-clojuredocs-at-point))

(def-package! clj-refactor
  :after clojure-mode
  :config

  ;; setup some extra namespace auto completion for great awesome
  (dolist (mapping '(("time"      . "clj-time.core")
                     ("try"       . "clj-try.core")
                     ("r"         . "reagent.core")
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

  (map! :map cider-mode-map "C-h r" #'cljr-helm)
  (cljr-add-keybindings-with-prefix "M-m"))


(def-package! cider
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  ;; settings for cider repl as a popup (prevent it from being closed on escape, especially.)
  (set! :popup "^\\*cider" :regexp t :noselect t :noesc t)

  (map! :map cider-repl-mode-map "C-x C-d" #'helm-clojuredocs-at-point)
  (map! :map cider-repl-mode-map "C-x C-p" #'cider-repl-previous-matching-input)
  (map! :map cider-repl-mode-map "M-r"     #'cider-switch-repl)
  (map! :map cider-mode-map "C-x C-t"      #'cider-eval-and-run-test))

(def-package! cider-find
  :commands (cider-find-resource cider-find-ns cider-find-var))

(def-package! cider-scratch
  :commands (cider-scratch))

(def-package! cider-apropos
  :commands (cider-apropos))

(def-package! cider-ns
  :commands (cider-ns-refresh))

(def-package! cljr-helm
  :commands (cljr-helm))
