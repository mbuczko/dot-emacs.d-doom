;;; private/default/+hydras.el -*- lexical-binding: t; -*-

;; pretty-hydra
(defun with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-fileicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-octicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun with-material (icon str &optional height v-adjust)
  (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(pretty-hydra-define global-toggles
  (:color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Global switches") :separator "-")
  ("UI"
   (("t" centaur-tabs-mode "centaur tabs" :toggle t)
    ("o" company-posframe-mode "posframe" :toggle t)
    ("l" mac-auto-operator-composition-mode "ligatures" :toggle t)
    ("b" beacon-mode "beacon" :toggle t))
   "Basic"
   (("L" page-break-lines-mode "page break lines" :toggle t)
    ("n" display-line-numbers-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" ws-butler-mode "whitespace cleanup" :toggle t)
    ("v" visual-line-mode "visual line" :toggle t))
   "Highlight"
   (("h" hl-line-mode "line" :toggle t)
    ("s" highlight-symbol-mode "symbol" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("r" highlight-parentheses-mode "parens" :toggle t))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("F" fancy-narrow-mode "fancy narrow" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
    ("N" neotree-show "neotree" :color teal))))

(pretty-hydra-define dev-actions
  (:color pink :quit-key "q" :separator "-")
  ("Git"
   (("f" magit-file-dispatch "file..." :color teal)
    ("b" magit-diff-buffer-file "diff buffer" :color teal)
    ("h" git-timemachine "time machine" :color teal)
    ("l" magit-todos-list "todos list" :color teal))
   "GitHub"
   (("S" github-stars-browse-url "stars..." :color teal)
    ("L" gist-list "gists..." :color teal)
    ("P" github--goto-pr "goto PR" :color teal)
    ("I" github--goto-issue "goto ISSUE" :color teal))
   "Chunk"
   (("s" git-gutter:stage-hunk "stage" :color teal)
    ("v" git-gutter:revert-hunk "revert" :color teal)
    ("=" git-gutter:popup-hunk "popup" :color teal)
    ("[" git-gutter:previous-hunk "prev ←" :toggle t)
    ("]" git-gutter:next-hunk "next →" :toggle t))
   "Search"
   (("g" deadgrep "deadgrep" :color teal)
    ("G" helm-google-suggest "google" :color teal))
   "Tags"
   (("e" helm-etags-select "etags select" :color teal)
    ("t" projectile-find-tag "projectile tags" :color teal))
   "Other"
   (("-" nuke-buffers "nuke unused buffers" :color teal)
    ("k" diff-last-two-kills "diff last 2 kills" :color teal)
    ("d" deft "deft" :color teal)
    ("m" helm-filtered-bookmarks "bookmarks" :color teal)
    ("p" powerthesaurus-lookup-word "powerthesaurus" :color teal))))
