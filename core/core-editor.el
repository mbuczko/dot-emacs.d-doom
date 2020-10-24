;;; core-editor.el -*- lexical-binding: t; -*-

(defvar doom-large-file-size 1
  "Size (in MB) above which the user will be prompted to open the file literally
to avoid performance issues. Opening literally means that no major or minor
modes are active and the buffer is read-only.")

(defvar doom-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
    doc-view-mode doc-view-mode-maybe ebrowse-tree-mode pdf-view-mode)
  "Major modes that `doom|check-large-file' will ignore.")

(setq-default
 vc-follow-symlinks t
 abbrev-mode t

 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t

 ;; Bookmarks
 bookmark-default-file (concat doom-etc-dir "bookmarks")
 bookmark-save-flag t

 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t

 ;; Scrolling
 hscroll-margin 1
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 0

 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab

 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50

 ;; whitespace-mode
 whitespace-line-column fill-column
 whitespace-style '(face indentation tabs tab-mark spaces space-mark newline newline-mark trailing lines-tail)
 whitespace-display-mappings '((tab-mark ?\t [?› ?\t])
                               (newline-mark ?\n [?¬ ?\n])
                               (space-mark ?\  [?·] [?.])))

;; ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function #'split-window-horizontally
      ediff-window-setup-function #'ediff-setup-windows-plain)

(defun doom|dont-kill-scratch-buffer ()
  "Don't kill the scratch buffer."
  (or (not (string= (buffer-name) "*scratch*"))
      (ignore (bury-buffer))))
(add-hook 'kill-buffer-query-functions #'doom|dont-kill-scratch-buffer)

;; temporary windows often have q bound to `quit-window', which only buries the
;; contained buffer. I rarely don't want that buffer killed, so...
(defun doom|quit-window (orig-fn &optional kill window)
  (funcall orig-fn (not kill) window))
(advice-add #'quit-window :around #'doom|quit-window)

(defun doom|check-large-file ()
  "Check if the buffer's file is large (see `doom-large-file-size'). If so,
open it literally (read-only, disabled undo and in fundamental-mode) for
 performance sake."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and (not (memq major-mode doom-large-file-modes-list))
               size (> size (* 1024 1024 doom-large-file-size)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))
(add-hook 'find-file-hook #'doom|check-large-file)

(defun doom-popup-buffer (buffer &optional plist extend-p)
  "Display BUFFER in a shackle popup with PLIST rules. See `shackle-rules' for
possible rules. If EXTEND-P is non-nil, don't overwrite the original rules for
this popup, just the specified properties. Returns the new popup window."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (shackle-display-buffer
   buffer
   nil (or (if extend-p
               (append plist (shackle-match buffer))
             plist)
(shackle-match buffer))))

(defun doom-disable-ligatures ()
  (setq auto-composition-mode nil))


;;
;; Built-in plugins
;;

;; revert buffers for changed files
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; enabled by default in Emacs 25+. No thanks.
;;(electric-indent-mode -1)

;; savehist / saveplace
(setq savehist-file (concat doom-cache-dir "savehist")
      savehist-save-minibuffer-history t
      savehist-autosave-interval nil ; save on kill only
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      save-place-file (concat doom-cache-dir "saveplace"))
(add-hook! 'doom-init-hook #'(savehist-mode save-place-mode))

;; Keep track of recently opened files
(use-package recentf
  :hook (doom-init . recentf-mode)
  :config
  (setq recentf-save-file (concat doom-cache-dir "recentf")
        recentf-max-menu-items 0
        recentf-max-saved-items 300
        recentf-filename-handlers '(file-truename)
        recentf-exclude
        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
              "^/var/folders/.+$"
              ;; ignore private DOOM temp files (but not all of them)
              (concat "^" (file-truename doom-local-dir)))))


;;
;; Core Plugins
;;

;; Auto-close delimiters and blocks as you type
(use-package smartparens
  :hook (doom-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)

  (setq sp-show-pair-delay 0
        sp-max-pair-length 3)

  (sp-local-pair '(xml-mode nxml-mode php-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC"))))

;; Branching undo
(use-package undo-tree
  :config
  (add-hook 'doom-init-hook #'global-undo-tree-mode)
  ;; persistent undo history is known to cause undo history corruption, which
  ;; can be very destructive! So disable it!
  (setq undo-tree-auto-save-history nil
        undo-tree-history-directory-alist
        (list (cons "." (concat doom-cache-dir "undo-tree-hist/")))))

;; Auto-saving
(defun doom-auto-save-command ()
  "Save the current buffer if `prelude-auto-save' is not nil."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(add-hook 'mouse-leave-buffer-hook 'doom-auto-save-command)
(add-hook 'focus-out-hook 'doom-auto-save-command)

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'visual-line-mode-hook (lambda () (setq fill-column 100)))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'html-mode-hook (lambda () (visual-fill-column-mode -1)))

;; advise all window switching functions
(advise-commands "auto-save"
                 (switch-to-buffer other-window)
                 before
                 (doom-auto-save-command))


;;
;; Autoloaded Plugins
;;

(use-package avy
  :commands (avy-goto-line avy-goto-char-timer)
  :config
  (setq avy-all-windows nil
        avy-background t))

(use-package command-log-mode
  :commands (command-log-mode global-command-log-mode)
  :config
  (set! :popup "*command-log*" :size 40 :align 'right :noselect t)
  (setq command-log-mode-auto-show t
        command-log-mode-open-log-turns-on-mode t))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package multiple-cursors
  :commands (mc/mark-more-like-this-extended mc/mark-all-like-this-dwim))

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word er/mark-defun er/mark-inside-quotes er/mark-inside-pairs er/mark-inner-tag er/mark-outer-tag))

(use-package smart-forward
  :commands (smart-up smart-down smart-backward smart-forward))

(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode)
  :config (setq wgrep-auto-save-buffer t))

(use-package bm
  :commands (bm-toggle bm-show-all))

(use-package helm-bm
  :commands (helm-bm))

(use-package smartrep
  :commands (smartrep-define-key))

(use-package crux
  :commands (crux-duplicate-current-line-or-region crux-cleanup-buffer-or-region crux-smart-open-line))

(use-package paxedit
  :commands (paxedit-delete paxedit-transpose-forward paxedit-transpose-backward))

(use-package paredit
  :commands (paredit-wrap-round paredit-wrap-square paredit-wrap-curly))

(use-package shackle
  :commands (shackle-display-buffer shackle-match))

(use-package zop-to-char
  :commands (zop-to-char))

(use-package beacon
  :commands (beacon-mode))

(use-package swiper
  :commands (swiper-thing-at-point swiper-isearch))

(provide 'core-editor)
;;; core-editor.el ends here
