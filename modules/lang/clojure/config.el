;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(defun smart-sexp-open-line ()
  (interactive)
  (smart-backward)
  (smart-backward)
  (smart-forward)
  (newline-and-indent))

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

  (define-key clojure-mode-map (kbd "C-x C-d") #'helm-clojuredocs-at-point)
  (define-key clojure-mode-map (kbd "M-RET")   #'smart-sexp-open-line))

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
                     ("compojure" . "compojure.core")))
    (add-to-list 'cljr-magic-require-namespaces mapping t))

  (define-key cider-mode-map (kbd "C-h r") #'cljr-helm)
  (cljr-add-keybindings-with-prefix "M-m"))


(def-package! cider
  :commands (cider-jack-in cider-mode cider-jack-in-clojurescript)
  :config
  (setq nrepl-hide-special-buffers t)

  ;; settings for cider repl as a popup (prevent it from being closed on escape, especially.)
  (set! :popup "^\\*cider" :regexp t :noselect t :noesc t)

  (defadvice cider-jump-to (after cider-jump activate)
    "Auto re-centers screen after jump"
    (recenter))

  (define-key cider-repl-mode-map (kbd "C-x C-d") #'helm-clojuredocs-at-point)
  (define-key cider-repl-mode-map (kbd "C-x C-p") #'cider-repl-previous-matching-input)
  (define-key cider-repl-mode-map (kbd "M-r")     #'cider-switch-repl)
  (define-key cider-repl-mode-map (kbd "C-c n")   #'cider-find-ns)
  (define-key cider-mode-map (kbd "C-c n")        #'cider-find-ns)
  (define-key cider-mode-map (kbd "C-x C-t")      #'cider-eval-and-run-test))

(def-package! cider-find
  :commands (cider-find-resource cider-find-ns cider-find-var))

(def-package! cider-scratch
  :commands (cider-scratch))

(def-package! cider-apropos
  :commands (cider-apropos))

(def-package! cider-ns
  :commands (cider-ns-refresh cider-refresh))

(def-package! cider-selector
  :commands (cider-selector))

(def-package! cljr-helm
  :commands (cljr-helm))

(def-package! helm-cider
  :commands (helm-cider-spec helm-cider-repl-history))

(def-package! zprint-mode
  :commands (zprint))
