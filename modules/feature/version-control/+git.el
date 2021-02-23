;;; feature/version-control/+git.el -*- lexical-binding: t; -*-
;;;###if (not (featurep! -git))

(defun github--visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(defun github--get-issue-or-pr-at-point ()
  (if-let ((iop (car (seq-filter
                      (lambda (s) (string-prefix-p "#" s))
                      (split-string (thing-at-point 'line t))))))
      (progn
        (string-match "[0-9]+" iop)
        (match-string 0 iop))))

(defun github--goto-issue-or-pr (id type)
  "Opens a browser with issue or PR (denoted by TYPE) of given ID."
  (let* ((origin-url (car (git-link--exec "config" "--get" "remote.origin.url")))
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

(fset 'github--format-link
      (lambda (&optional arg)
        "Formats given link into less messy form."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([?\M-f ?\M-b ?| ?\C-  ?\C-s ?· return left left ?\M-x ?\C-s ?# return ?\M-d ?\C-r ?| return ?\C-k ?\M-v ?\M-y ?\M-b ?\C-e ?  ?\M-v ?\M-y] 0 "%d")) arg)))

(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")

(use-package git-gutter-fringe
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


(use-package git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)

  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details nil)
  (add-hook 'git-timemachine-mode-hook #'+vcs|init-header-line)
  (advice-add #'git-timemachine-show-revision :after #'+vcs*update-header-line))


(use-package magit
  :commands (magit-status magit-blame magit-diff-buffer-file magit-file-dispatch)
  :hook ((magit-status-mode . visual-fill-column-mode)
         (git-commit-mode . visual-fill-column-mode))
  :config
  (setq magit-save-repository-buffers 'dontask)
  (defadvice magit-status (around magit-fullscreen activate)
    "Full screen magit-status."
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (define-key magit-mode-map "v" #'github--visit-pull-request-url)
  (add-hook 'magit-mode-hook (lambda ()
                               (doom-hide-modeline-mode)
                               (turn-on-magit-gitflow)
                               ;; (magit-todos-mode)
                               ;; fix visual glitches with tiny fringe icons
                               (setq left-fringe-width 10))))

(use-package forge
  :after magit)

(use-package magit-gitflow
  :commands (turn-on-magit-gitflow))

(use-package magit-todos
  :commands (magit-todos-mode magit-todos-list))

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage git-link--exec))

(use-package helm-git-grep
  :commands (helm-git-grep helm-git-grep-at-point))
