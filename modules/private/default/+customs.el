;;; private/default/+customs.el -*- lexical-binding: t; -*-

(require 'window-numbering)
(require 'highlight-symbol)
(require 'highlight-parentheses)

;; adjust PATHs
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; ligatures turned on by default
(mac-auto-operator-composition-mode)

;; sane mouse clicks
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;; some more defaults
(setq-default
 rfc-mode-directory (expand-file-name "~/.deft/rfc/"))

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

;; custom order on org-agenda
(setq org-agenda-sorting-strategy '((agenda habit-down time-up deadline-up scheduled-down priority-down category-keep)
                                    (todo priority-down category-keep)
                                    (tags priority-down category-keep)
                                    (search category-keep)))

;; a few handy functions to make life easier
(defun smart-sexp-open-line ()
  (interactive)
  (smart-backward)
  (smart-backward)
  (smart-forward)
  (newline-and-indent))

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
  (xref-find-definitions
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
