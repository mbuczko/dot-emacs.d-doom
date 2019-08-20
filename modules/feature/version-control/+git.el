;;; feature/version-control/+git.el -*- lexical-binding: t; -*-
;;;###if (not (featurep! -git))

(def-package! gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")


(def-package! gitignore-mode
  :mode "/\\.gitignore$")


(def-package! git-gutter-fringe
  :commands git-gutter-mode
  :init
  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode) #'+version-control|git-gutter-maybe)
  :config
  (set! :popup "^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows))


(def-package! git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)

  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details nil)
  (add-hook 'git-timemachine-mode-hook #'+vcs|init-header-line)
  (advice-add #'git-timemachine-show-revision :after #'+vcs*update-header-line))


(def-package! magit
  :commands (magit-status magit-blame magit-diff-buffer-file magit-file-dispatch)
  :config
  (magit-todos-mode)
  (defadvice magit-status (around magit-fullscreen activate)
    "Full screen magit-status."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(def-package! magit-gitflow
  :commands (turn-on-magit-gitflow))

(def-package! magit-todos
  :commands (magit-todos-mode))

(def-package! git-link
  :commands (git-link git-link-commit git-link-homepage git-link--exec))
