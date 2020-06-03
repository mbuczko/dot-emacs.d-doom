;;; tools/gist/config.el -*- lexical-binding: t; -*-

;; NOTE On occasion, the cache gets corrupted, causing wrong-type-argument
;; errors. If that happens, try `+gist/kill-cache'. You may have to restart
;; Emacs.

(use-package gist
  :commands (gist-list gist-region-or-buffer-private gist-region-or-buffer)
  :config
  (set! :popup "*github:gists*" :size 15 :select t :autokill t)

  (defun +gist*list-render (orig-fn &rest args)
    (funcall orig-fn (car args) t)
    (unless (cadr args)
      (doom-popup-buffer (current-buffer))))
  (advice-add #'gist-list-render :around #'+gist*list-render))
