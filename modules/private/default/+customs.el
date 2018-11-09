;;; private/default/+customs.el -*- lexical-binding: t; -*-

(require 'helm-git-grep)
(require 'magit-gitflow)
(require 'golden-ratio)
(require 'window-numbering)
(require 'highlight-symbol)
(require 'highlight-parentheses)
(require 'dash-at-point)
(require 'goto-last-change)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
      mouse-wheel-progressive-speed nil             ; don't accelerate scrolling

      ;; no copy-on-select!
      x-select-enable-clipboard nil
      x-select-enable-primary nil
      mouse-drag-copy-region nil

      default-frame-alist '((left . 40) (top . 40) (width . 120) (height . 40))

      confirm-nonexistent-file-or-buffer nil
      tags-revert-without-query t
      auto-window-vscroll nil

      ;; keep window splitting at sane proportions
      ;; with golden-ratio switched on

      split-width-threshold 0
      split-height-threshold nil
      window-min-width 30

      ibuffer-expert t
      ibuffer-show-empty-filter-groups nil
      ibuffer-saved-filter-groups
      '(("home"
         ("emacs-config" (or (filename . ".emacs") (filename . ".gnus")))
         ("Dired" (mode . dired-mode))
         ("Ruby" (mode . ruby-mode))
         ("CSS" (or (mode . scss-mode) (mode . css-mode)))
         ("JS" (mode . js2-mode))
         ("Clojure" (mode . clojure-mode))
         ("EShell" (mode . eshell-mode))
         ("Org" (or (mode . org-mode)))
         ("Gnus" (or (mode . message-mode)
                     (mode . bbdb-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode)))
         ("REPL" (name . "*cider-repl*"))
         ("ERB" (name ."*.erb*"))
         ("Magit" (name . "\*magit"))
         ("ERC" (mode . erc-mode))
         ("Help" (or (name . "\*Help\*")
                     (name . "\*Apropos\*")
                     (name . "\*info\*")))))

      ;; helm config
      helm-dash-common-docsets '("Clojure/Clojure"
                                 "Java_SE8/Java"
                                 "jQuery/jQuery"
                                 "Lo-Dash/Lo-Dash"
                                 "D3JS/D3JS"
                                 "JavaScript/JavaScript"))

;; auto-enabled modes

(company-mode)
(golden-ratio-mode)
(window-numbering-mode)
(global-highlight-parentheses-mode)

;; a few handy functions to make life easier

(defun magit-diff-current-buffer ()
  (interactive)
  (magit-diff-unstaged nil (list (buffer-file-name))))

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-arguments)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-arguments "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-arguments (remove "-w" magit-diff-arguments))
  (magit-refresh))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

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
  (xref-find-definitions (car (last (split-string (symbol-name (symbol-at-point)) "/")))))

(defun whack-whitespace ()
  (interactive)
  (re-search-forward "[ \t\n]+" nil t)
  (replace-match "" nil nil))

(defun cider-switch-repl ()
  "Switches between cider-repl and last active buffer"
  (interactive)
  (if (string-match "cider-repl" (buffer-name) 1)
      (cider-switch-to-last-clojure-buffer)
    (cider-switch-to-repl-buffer)))

(defun repl-reset ()
  "Sends (reset) to currently running repl"
  (interactive)
  (save-buffer)
  (sleep-for 1)
  (cider-interactive-eval "(boot.user/reset)"))

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

(defadvice magit-status (around magit-fullscreen activate)
  "Full screen magit-status."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice clipboard-kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun clipboard-cut-line-or-region ()
  (interactive)
  (if (use-region-p)
      (clipboard-kill-region (region-beginning) (region-end) t)
    (clipboard-kill-region (line-beginning-position) (line-beginning-position 2))))


;; github integration enabler
;;(magithub-feature-autoinject t)

;; javascript mode for all *.js and *.vue files
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(add-hook 'less-css-mode-hook
          (lambda ()
            (define-key less-css-mode-map (kbd "C-o") 'helm-css-scss)))

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map (kbd "C-o") 'helm-css-scss)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (require 'ws-butler)
            (ws-butler-mode)
            (highlight-symbol-mode)
            (clj-refactor-mode)))

(add-hook 'cider-repl-mode-hook
          (lambda ()
            (turn-on-smartparens-strict-mode)))

;; sane mouse clicks

(define-key global-map (kbd "<S-down-mouse-1>") 'ignore)
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;; projectile default prefix

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
