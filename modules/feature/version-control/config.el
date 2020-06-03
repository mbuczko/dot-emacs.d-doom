;;; feature/version-control/config.el -*- lexical-binding: t; -*-

(or (featurep! -git) (load! +git))

;;
(setq vc-make-backup-files nil)

(defvar +vcs-auto-hydra-smerge nil
  "When entering `smerge-mode' automatically open associated hydra.")

(after! vc-annotate
  (set! :popup
    '("*vc-diff*" :size 15 :noselect t)
    '("*vc-change-log*" :size 15)
    '(vc-annotate-mode :same t)))

(use-package smerge-mode
  :hook (find-file . +vcs|enable-smerge-mode-maybe)
  :config
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias #'smerge-keep-upper #'smerge-keep-mine)
      (defalias #'smerge-keep-lower #'smerge-keep-other)
      (defalias #'smerge-diff-base-upper #'smerge-diff-base-mine)
      (defalias #'smerge-diff-upper-lower #'smerge-diff-mine-other)
      (defalias #'smerge-diff-base-lower #'smerge-diff-base-other))))
