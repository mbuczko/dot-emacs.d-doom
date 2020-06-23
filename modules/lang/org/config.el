;;; lang/org/config.el -*- lexical-binding: t; -*-

(defvar +org-dir (expand-file-name "~/.deft")
  "The directory where org files are kept.")

;; Ensure ELPA org is prioritized above built-in org.
(when-let* ((path (locate-library "org" nil doom--package-load-path)))
  (setq load-path (delete path load-path))
  (push (file-name-directory path) load-path))

;; Sub-modules
(if (featurep! +attach)  (load! +attach))
(if (featurep! +babel)   (load! +babel))
(if (featurep! +capture) (load! +capture))
(if (featurep! +export)  (load! +export))
(if (featurep! +present) (load! +present))
(if (featurep! +sources) (load! +sources))

;;
;; Plugins
;;

(use-package toc-org
  :commands toc-org-enable)

(use-package helm-org
  :commands helm-org-capture-templates)

(use-package ox-extra ;org-plus-contrib
   :commands (ox-extras-activate ignore-headlines)
   :config
   (ox-extras-activate '(ignore-headlines)))

(use-package org-bullets
  :commands org-bullets-mode)

(use-package poporg
  :commands poporg-dwim)

(use-package direnv
  :commands direnv-mode)

;;
;; Bootstrap
;;

(after! org
  ;; Occasionally, Emacs encounters an error loading the built-in org, aborting
  ;; the load. This results in a broken, partially loaded state. This require
  ;; tries to set it straight.
  (require 'org)
  (setq org-directory +org-dir)

  (+org-init-ui)
  (+org-hacks))

(add-hook! org-mode
  #'(org-bullets-mode           ; "prettier" bullets
     org-indent-mode            ; margin-based indentation
     toc-org-enable             ; auto-table of contents
     direnv-mode
     visual-line-mode           ; line wrapping
     hide-ctrl-M                ; no garbage please

     +org|enable-auto-update-cookies
     +org|smartparens-compatibility-config
     +org|unfold-to-2nd-level-or-point
     +org|show-paren-mode-compatibility
     +org|colored-src-listings))


;;
;; Config hooks
;;

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
  (defun +org-sp-point-in-checkbox-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "\\s-*]")))

  ;; make delimiter auto-closing a little more conservative
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "*" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(sp-point-after-word-p sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(sp-point-after-word-p sp-point-before-word-p)))))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving."
  (add-hook 'before-save-hook #'+org|update-cookies nil t))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))

(defun +org|colored-src-listings ()
  "Make the listing colorful."
  (setq
   org-latex-listings 'minted
   org-latex-packages-alist '(("" "minted"))
   org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f")
   org-startup-with-latex-preview t

   ;; org-mode math is now highlighted ;-)
   org-highlight-latex-and-related '(latex)

   ;; Hide the *,=,/ markers
   org-hide-emphasis-markers t))

(defun +org-init-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (directory-files +org-dir t "\\.org$" t)
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files nil
   org-agenda-current-time-string #("<-now-" 0 2 (org-heading t))
   org-agenda-tags-column -120
   org-catch-invisible-edits 'smart
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
   org-duration-format '((special . h:mm))
   org-email-link-description-format "Email %c: %s"
   org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   ;; org-ellipsis " ... "
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-log-into-drawer t
   org-log-note-headings '((done        . "CLOSING NOTE %t")
                           (state       . "State %-12s from %-12S %t")
                           (note        . "Note taken on %t")
                           (reschedule  . "Schedule changed on %t: %S -> %s")
                           (delschedule . "Not scheduled, was %S on %t")
                           (redeadline  . "Deadline changed on %t: %S -> %s")
                           (deldeadline . "Removed deadline, was %S on %t")
                           (refile      . "Refiled on %t")
                           (clock-out   . ""))
   org-log-state-notes-insert-after-drawers t
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   `((?a . ,(face-foreground 'error))
     (?b . ,(face-foreground 'warning))
     (?c . ,(face-foreground 'success)))
   org-src-window-setup 'current-window
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column -100
   org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "BLOCKED(b)" "DONE(d)"))
   org-todo-keyword-faces '(("TODO" . "green")
                            ("BLOCKED" . "red")
                            ("" . "")
                            ("WAITING" . "yellow")
                            ("IN-PROGRESS" . "white"))

   org-use-sub-superscripts '{}
   outline-blank-line t

   ;; LaTeX previews are too small and usually render to light backgrounds, so
   ;; this enlargens them and ensures their background (and foreground) match the
   ;; current theme.
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
   org-format-latex-options
   (plist-put org-format-latex-options
              :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                              'default)
                                          :background nil t)))

  ;; Custom links
  (org-link-set-parameters
   "org"
   :complete (lambda () (+org-link-read-file "org" +org-dir))
   :follow   (lambda (link) (find-file (expand-file-name link +org-dir)))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link +org-dir))
                   'org-link
                 'error))))

;;
(defun +org-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  ;; Org sources autocompleted
  (define-key org-mode-map (kbd "<") #'org-insert-char-dwim)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (add-to-list 'recentf-exclude #'+org-is-agenda-file)))
