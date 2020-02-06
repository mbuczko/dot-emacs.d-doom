;;; completion/helm/config.el -*- lexical-binding: t; -*-

;; Warning: since I don't use helm, this may be out of date.

(defvar +helm-global-prompt "››› "
  "The helm text prompt prefix string is globally replaced with this string.")


;;
;; Packages
;;

(def-package! helm
  :init
  (setq helm-quick-update t
        ;; Speedier without fuzzy matching
        helm-mode-fuzzy-match nil
        helm-buffers-fuzzy-matching nil
        helm-apropos-fuzzy-match nil
        helm-M-x-fuzzy-match nil
        helm-recentf-fuzzy-match nil
        helm-projectile-fuzzy-match nil
        ;; Display extraineous helm UI elements
        helm-display-header-line nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        ;; helm-mode-handle-completion-in-region nil
        ;; Don't wrap item cycling
        ;; helm-move-to-line-cycle-in-source t
        helm-candidate-number-limit 50
        helm-boring-buffer-regexp-list '("\\*" "\\` " "TAGS")
        helm-buffer-max-length 60
        helm-candidate-separator "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"
        helm-ff-lynx-style-map t
        helm-git-grep-pathspecs '("*" ":!:*.inc.js" ":!:*yarn*" ":!:*.min.*")
        helm-imenu-lynx-style-map t
        helm-occur-use-ioccur-style-keys t
        helm-prevent-escaping-from-minibuffer t
        helm-semantic-lynx-style-map t
        helm-split-window-default-side 'other
        helm-split-window-inside-p t)

  :config
  (load "helm-autoloads" nil t)
  (add-hook 'doom-init-hook #'helm-mode)

  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  (require 'helm-projectile)
  (require 'helm-command)
  (set-keymap-parent helm-projectile-find-file-map helm-map)

  ;; helm is too heavy for find-file-at-point
  (after! helm-mode
    (add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
    (define-key helm-map (kbd "ESC")     nil)
    (define-key helm-map (kbd "<left>")  'helm-previous-source)
    (define-key helm-map (kbd "<right>") 'helm-next-source)
    (define-key helm-map (kbd "C-s")     'helm-minibuffer-history)
    (define-key helm-map (kbd "C-u")     'helm-delete-minibuffer-contents)
    (define-key helm-map (kbd "M-p")     'previous-history-element)
    (define-key helm-map (kbd "M-n")     'next-history-element)
    (define-key helm-map [escape]        'helm-keyboard-quit))

  (set! :popup "\\` ?\\*[hH]elm.*?\\*\\'" :size 14 :regexp t)
  (setq projectile-completion-system 'helm)

  ;;; Helm hacks
  (defun +helm*replace-prompt (plist)
    "Globally replace helm prompts with `+helm-global-prompt'."
    (if (keywordp (car plist))
        (plist-put plist :prompt +helm-global-prompt)
      (setf (nth 2 plist) +helm-global-prompt)
      plist))
  (advice-add #'helm :filter-args #'+helm*replace-prompt)

  (defun +helm*hide-header (&rest _)
    "Hide header-line & mode-line in helm windows."
    (setq mode-line-format nil
          auto-composition-mode nil)
    (setq-local header-line-format nil))

  (advice-add #'helm-display-mode-line :override #'+helm*hide-header)

  (define-key global-map [remap apropos]                     #'helm-apropos)
  (define-key global-map [remap find-file]                   #'helm-find-files)
  (define-key global-map [remap recentf-open-files]          #'helm-recentf)
  (define-key global-map [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer)
  (define-key global-map [remap projectile-recentf]          #'helm-projectile-recentf)
  (define-key global-map [remap projectile-find-file]        #'helm-projectile-find-file)
  (define-key global-map [remap imenu]                       #'helm-semantic-or-imenu)
  (define-key global-map [remap bookmark-jump]               #'helm-bookmarks)
  (define-key global-map [remap noop-show-kill-ring]         #'helm-show-kill-ring)
  (define-key global-map [remap projectile-switch-project]   #'helm-projectile-switch-project)
  (define-key global-map [remap projectile-find-file]        #'helm-projectile-find-file)
  (define-key global-map [remap imenu-anywhere]              #'helm-imenu-anywhere)
  (define-key global-map [remap execute-extended-command]    #'helm-M-x))

(def-package! helm-locate
  :defer t
  :init (defvar helm-generic-files-map (make-sparse-keymap))
  :config (set-keymap-parent helm-generic-files-map helm-map))


(def-package! helm-bookmark
  :commands helm-bookmark
  :config (setq-default helm-bookmark-show-location t))


(def-package! helm-files
  :defer t
  :config
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


(def-package! helm-ag
  :defer t
  :config
  (define-key helm-ag-edit-map [remap doom/kill-this-buffer] #'helm-ag--edit-abort)
  (define-key helm-ag-edit-map [remap quit-window]           #'helm-ag--edit-abort))


(def-package! helm-css-scss ; https://github.com/ShingoFukuyama/helm-css-scss
  :commands (helm-css-scss
             helm-css-scss-multi
             helm-css-scss-insert-close-comment)
  :config
  (setq helm-css-scss-split-direction #'split-window-vertically
        helm-css-scss-split-with-multiple-windows t))


(def-package! helm-swoop ; https://github.com/ShingoFukuyama/helm-swoop
  :commands (helm-swoop helm-multi-swoop helm-multi-swoop-all)
  :config
  (setq helm-swoop-use-line-number-face t
        helm-swoop-candidate-number-limit 200
        helm-swoop-speed-or-color t
        helm-swoop-pre-input-function (lambda () "")))


(def-package! helm-describe-modes
  :commands helm-describe-modes)
