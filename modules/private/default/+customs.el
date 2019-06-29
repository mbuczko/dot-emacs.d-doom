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
(global-flycheck-mode)
(ws-butler-global-mode)

;; turns ligatures on
(mac-auto-operator-composition-mode)

;; sane mouse clicks

(define-key global-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

(setq rfc-mode-directory (expand-file-name "~/Dropbox/rfc/"))

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

(defun fixup-defun-whitespace ()
  "Normalise all whitespace between forms in defun, and group closing parens.
Does not affect strings/comments"
  (interactive)
  (let* ((bnds (bounds-of-thing-at-point 'defun))
         (beg (car bnds)) (end (cdr bnds)))
    (save-excursion
      (goto-char end)
      (while (> (point) beg)
        (search-backward-regexp "[([{ \t\r\n]" beg)
        (unless (or (nth 8 (syntax-ppss)) ;; in a string or comment
                    (nth 4 (syntax-ppss)))
          (let ((s (buffer-substring (point-at-bol) (point))))
            (cond
             ;; indentation, skip
             ((string-match-p "^[ \t]*\\'" s)
              (forward-line 0))
             ;; closing paren on separate line
             ((string-match-p "^[ \t]*[])}]" s)
              (forward-line -1)
              (end-of-line)
              ;; don't join if prev line is a comment
              (unless (nth 8 (syntax-ppss))
                (delete-char 1)
                (fixup-whitespace)
                (forward-line 1)))
             (t
              (fixup-whitespace))))))
      (goto-char beg)
      (indent-sexp)
      (when (derived-mode-p 'clojure-mode)
        (call-interactively 'clojure-align)))))

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

;; projectile default prefix
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; global abbrev mode with most frequently used phrases
(setq-default abbrev-mode t)
