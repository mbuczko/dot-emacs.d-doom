;;; lang/clojure/config.el -*- lexical-binding: t; -*-

(defun jet ()
  (interactive)
  (shell-command-on-region
   (region-beginning)
   (region-end)
   "jet --from transit --to edn --pretty --edn-reader-opts '{:default tagged-literal}'"
   (current-buffer)
   t
   "*jet error buffer*"
   t))

(use-package clojure-mode
  :mode "\\.clj$"
  :mode "\\.cljc$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :config

  ;; get docstrings colored correctly with defn-spec
  (put 'defn-spec 'clojure-doc-string-elt 3)

  ;; treat some-symbol as a single word for editing lispy sources
  (dolist (c (string-to-list ":_-/?!#*"))
    (modify-syntax-entry c "w" clojure-mode-syntax-table)))

(use-package cider
  :hook ((clojure-mode-local-vars . cider-mode)
         (cider-mode . eldoc-mode)
         (cider-mode . highlight-symbol-mode)
         (cider-repl-mode . doom-hide-modeline-mode)
         (cider-repl-mode . doom-disable-ligatures)
         (cider-repl-mode . yas-minor-mode))
  :bind (:map clojure-mode-map
              ("M-r"     . cider-switch-repl)
              ("C-c C-t" . cider-eval-and-run-test)
              ("C-c C-n" . cider-find-ns)
              ("C-c c"   . clj-actions/body)
         :map cider-repl-mode-map
              ("C-c C-p" . cider-repl-previous-matching-input)
              ("C-c c"   . clj-actions/body))
  :config
  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-overlays-use-font-lock t
        cider-prompt-for-symbol nil
        cider-repl-display-help-banner nil
        cider-repl-display-in-current-window t
        cider-repl-pop-to-buffer-on-connect 'display-only
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history nil
        cider-stacktrace-default-filters '(tooling dup))

  (defadvice cider-jump-to (after cider-jump activate)
    "Auto re-centers screen after jump"
    (recenter))

  (pretty-hydra-define clj-actions
    (:color pink :quit-key "q" :title (with-octicon "dashboard" "Clojure Dev Kit") :separator "-")
    ("Code"
     (("s" helm-cider-spec "spec..." :color teal)
      ("n" cider-find-ns "find namespace..." :color teal)
      ("v" cider-eval-ns-form "eval ns form" :color teal))
     "Doc"
     (("c" cider-doc "cider doc" :color teal)
      ("j" cider-javadoc "cider javadoc" :color teal)
      ("d" helm-clojuredocs-at-point "clojuredocs" :color teal)
      ("D" dash-at-point "dash" :color teal))
     "REPL"
     (("i" cider-insert-region-in-repl "insert region to REPL" :color teal)
      ("r" cider-refresh "reload code" :color teal)
      ("h" helm-cider-repl-history "REPL history..." :color teal))
     "Buffs"
     (("t" cider-scratch "cider scratch" :color teal)
      ("e" cider-selector "cider selector..." :color teal))
     "Format"
     (("j" jet "Transit -> EDN")
      ("z" zprint "zprint formatter" :color teal)
      ("f" cider-format-edn-region "format EDN region" :color teal)))))

(use-package clj-refactor
  :hook ((clojure-mode . clj-refactor-mode))
  :config
  (cljr-add-keybindings-with-prefix "M-m")

  (setq cljr-auto-clean-ns t
        cljr-hotload-dependencies t
        cljr-suppress-middleware-warnings t
        cljr-warn-on-eval nil)

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
    (add-to-list 'cljr-magic-require-namespaces mapping t)))

(use-package cider-find     :after cider)
(use-package cider-scratch  :after cider)
(use-package cider-apropos  :after cider)
(use-package cider-ns       :after cider)
(use-package cider-selector :after cider)
(use-package cider-format   :after cider)

(use-package cljr-helm
  :after clj-refactor
  :bind (:map clojure-mode-map
              ("C-h r" . cljr-helm)))

(use-package helm-clojuredocs
  :after clojure-mode
  :bind (:map clojure-mode-map
              ("C-c C-d C-p" . helm-clojuredocs-at-point)
         :map cider-repl-mode-map
              ("C-c C-d C-p" . helm-clojuredocs-at-point)))

(use-package helm-cider
  :after cider)

(use-package zprint-mode
  :after clojure-mode)

(use-package flycheck-joker
  :after clojure-mode)
