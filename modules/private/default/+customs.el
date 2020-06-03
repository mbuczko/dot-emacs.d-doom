;;; private/default/+customs.el -*- lexical-binding: t; -*-

(require 'helm-git-grep)
(require 'window-numbering)
(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'ws-butler)

;; global modes

(global-company-mode)
(global-flycheck-mode)
(ws-butler-global-mode)
(company-posframe-mode)

;; ligatures turned on by default
(mac-auto-operator-composition-mode)

;; sane mouse clicks
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;; some more defaults
(setq-default
 abbrev-mode t
 rfc-mode-directory (expand-file-name "~/Dropbox/rfc/"))

;; solve performance problems
(put 'minibuffer-history 'history-length 50)
(put 'kill-ring 'history-length 25)

;; search engines
;; (require 'engine-mode)
;; (engine-mode)
;; (defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d")
;; (defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
;; (defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "s")
;; (defengine twitter "https://twitter.com/search?q=%s" :keybinding "t")

;; projectile default prefix
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; a few handy functions to make life easier
(defun mark-from-point-to-end-of-line ()
  "Mark everything from point to end of line."
  (interactive)
  (set-mark (line-end-position))
  (activate-mark))

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun find-tag-without-ns ()
  (interactive)
  (xref-find-apropos
   (car (last (split-string (symbol-name (symbol-at-point)) "/"))))
  (recenter))

(defun whack-whitespace ()
  (interactive)
  (re-search-forward "[ \t\n]+" nil t)
  (replace-match "" nil nil))

(defun cider-switch-repl ()
  "Switches between cider-repl and last active buffer."
  (interactive)
  (let ((buf (or (cider-current-repl)
                 (cider-selector--recently-visited-buffer 'cider-repl-mode))))
    (if (string-match "cider-repl" (buffer-name) 1)
        (switch-to-prev-buffer)
     (when buf (switch-to-buffer buf)))))

(defun cider-eval-and-run-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(defun close-other ()
  (interactive)
  (delete-window (other-window 1)))

(defun hide-ctrl-M ()
  "Hides the disturbing '^M' showing up in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun wrap-round  () (interactive) (er/expand-region 1) (paredit-wrap-round))
(defun wrap-curly  () (interactive) (er/expand-region 1) (paredit-wrap-curly))
(defun wrap-square () (interactive) (er/expand-region 1) (paredit-wrap-square))
(defun indent-defn () (interactive) (save-excursion
                                      (er/mark-defun)
                                      (crux-cleanup-buffer-or-region)))

(defadvice select-window-by-number (after select-window activate)
  "Resizing windows automatically when selected by window-numbering shortcuts."
  (golden-ratio))

(defadvice clipboard-kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (+doom/blink-cursor)
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun clipboard-cut-line-or-region ()
  "Cuts currently selected region or entire line if nothing was selected."
  (interactive)
  (if (use-region-p)
      (clipboard-kill-region (region-beginning) (region-end) t)
    (clipboard-kill-region (line-beginning-position) (line-beginning-position 2))))

(defun github--get-issue-or-pr-at-point ()
  (if-let ((iop (car (seq-filter
                      (lambda (s) (string-prefix-p "#" s))
                      (split-string (thing-at-point 'line t))))))
      (progn
        (string-match "[0-9]+" iop)
        (match-string 0 iop))))

(defun github--goto-issue-or-pr (id type)
  "Opens a browser with issue or PR (denoted by TYPE) of given ID."
  (let* ((origin-url (car (git-link--exec "config" "--get" "remote.origin.url")))
         (repo-match (string-match "^git@github.com:\\([^\\.]+\\)" origin-url))
         (repo-url   (concat "https://github.com/" (match-string 1 origin-url)))
         (sub-path   (cond ((eq 'issue type) "/issues")
                           ((eq 'pr type) "/pull"))))

    (message (concat repo-url sub-path "/" id))
    (browse-url
     (concat repo-url sub-path "/" id))))

(defun github--goto-issue (id)
  "Opens in a browser issue with given ID or with a one found at current line."
  (interactive
   (let* ((at-point (github--get-issue-or-pr-at-point))
          (default (if at-point (concat "Issue (" at-point ") #") "Issue #"))
          (str (read-string default nil nil at-point)))
     (list str)))
  (github--goto-issue-or-pr id 'issue))

(defun github--goto-pr (id)
  "Opens in a browser pull request with given ID or with a one found at current line."
  (interactive
   (let* ((at-point (github--get-issue-or-pr-at-point))
          (default (if at-point (concat "Pull-Request (" at-point ") #") "Pull-Request #"))
          (str (read-string default nil nil at-point)))
     (list str)))
  (github--goto-issue-or-pr id 'pr))

(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

;; javascript mode for all *.js and *.vue files
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'cider-mode-hook
          (lambda ()
            (highlight-symbol-mode)
            (clj-refactor-mode)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (doom-hide-modeline-mode)
            (doom-disable-ligatures)
            (yas-minor-mode 1)))

(add-hook 'org-mode-hook
          (lambda ()
            (require 'org-bullets)
            (org-bullets-mode 1)))

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
    ("r" git-gutter:revert-hunk "revert" :color teal)
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
    ("t" powerthesaurus-lookup-word "powerthesaurus" :color teal))))

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
   (("z" zprint "zprint formatter" :color teal)
    ("f" cider-format-edn-region "format EDN region" :color teal))))

(use-package powerthesaurus
  :commands (powerthesaurus-lookup-word))

(use-package github-stars
  :commands (github-stars-browse-url))
