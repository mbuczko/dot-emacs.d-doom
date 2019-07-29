;;; private/default/+customs.el -*- lexical-binding: t; -*-

(require 'deadgrep)
(require 'helm-git-grep)
(require 'golden-ratio)
(require 'window-numbering)
(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'dash-at-point)
(require 'goto-last-change)
(require 'ws-butler)

;; global modes

(global-company-mode)
(company-posframe-mode)
(global-flycheck-mode)
(ws-butler-global-mode)

;; turns ligatures on
(mac-auto-operator-composition-mode)

;; sane mouse clicks
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;; global abbrev mode with most frequently used phrases
(setq-default
 abbrev-mode t
 rfc-mode-directory (expand-file-name "~/Dropbox/rfc/"))

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
   (car (last (split-string (symbol-name (symbol-at-point)) "/")))))

(defun whack-whitespace ()
  (interactive)
  (re-search-forward "[ \t\n]+" nil t)
  (replace-match "" nil nil))

(defun cider-switch-repl ()
  "Switches between cider-repl and last active buffer."
  (interactive)
  (if (string-match "cider-repl" (buffer-name) 1)
      (cider-switch-to-last-clojure-buffer)
    (cider-switch-to-repl-buffer)))

(defun cider-eval-and-run-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(defun close-other ()
  (interactive)
  (delete-window (other-window 1)))

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
  (if-let ((iop (first (seq-filter
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

;; javascript mode for all *.js and *.vue files
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(add-hook 'clojure-mode-hook
          (lambda ()
            (highlight-symbol-mode)
            (clj-refactor-mode)))

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
  (:color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Global switches") :separator "┄")
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" ws-butler-mode "whitespace cleanup" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("l" hl-line-mode "line" :toggle t)
    ("s" highlight-symbol-mode "symbol" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("r" highlight-parentheses-mode "parens" :toggle t))
   "UI"
   (("t" centaur-tabs-mode "centaur tabs" :toggle t)
    ("n" neotree-show "neotree" :color teal))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(pretty-hydra-define dev-actions
  (:color pink :quit-key "q" :title (with-faicon "cog" "Development toolbox") :separator "┄")
  ("Doc"
   (("d" helm-clojuredocs-at-point "clojuredocs" :color teal)
    ("c" cider-doc "cider doc" :color teal)
    ("j" cider-javadoc "cider javadoc" :color teal)
    ("h" dash-at-point "dash" :color teal))
   "Git"
   (("d" magit-diff-buffer-file "diff buffer" :color teal)
    ("f" magit-file-dispatch "file..." :color teal)
    ("h" git-timemachine "time machine" :color teal)
    ("t" magit-todos-list "todos list" :color teal))
   "Chunk"
   (("a" git-gutter:stage-hunk "stage" :color teal)
    ("r" git-gutter:revert-hunk "revert" :color teal)
    ("n" git-gutter:next-hunk "next →" :toggle t)
    ("p" git-gutter:previous-hunk "prev ←" :toggle t)
    ("=" git-gutter:popup-hunk "popup" :color teal))
   "Search"
   (("g" deadgrep "deadgrep" :color teal)
    ("l" helm-projectile-grep "projectile grep" :color teal))))
