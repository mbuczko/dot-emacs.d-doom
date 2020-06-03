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
  (setq git-gutter:disabled-modes '(org-mode image-mode pdf-view-mode))
  (add-hook! 'find-file-hook
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name)))
               (vc-backend buffer-file-name)
               (not (memq major-mode git-gutter:disabled-modes)))
      (git-gutter-mode +1)))
  :config
  (defun +vc-gutter-update-h (&rest _)
    (when (and git-gutter-mode
               (not (memq this-command '(git-gutter:stage-hunk
                                         git-gutter:revert-hunk))))
      (ignore (git-gutter))))
  ;; update git-gutter when using magit commands
  (advice-add #'magit-stage-file   :after #'+vc-gutter-update-h)
  (advice-add #'magit-unstage-file :after #'+vc-gutter-update-h))


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
  (setq magit-save-repository-buffers 'dontask)
  (defadvice magit-status (around magit-fullscreen activate)
    "Full screen magit-status."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (add-hook 'magit-mode-hook (lambda ()
                               (doom-hide-modeline-mode)
                               (turn-on-magit-gitflow)
                               ;; (magit-todos-mode)
                               ;; fix visual glitches with tiny fringe icons
                               (setq left-fringe-width 10))))

(def-package! magit-gitflow
  :commands (turn-on-magit-gitflow))

(def-package! magit-todos
  :commands (magit-todos-mode magit-todos-list))

(def-package! git-link
  :commands (git-link git-link-commit git-link-homepage git-link--exec))
