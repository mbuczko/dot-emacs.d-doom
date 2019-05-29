;;; ../.local/etc/custom.el -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-executable "/usr/local/bin/ack")
 '(auth-sources (quote ("~/.authinfo" "~/.authinfo.gpg")))
 '(auto-window-vscroll nil t)
 '(bm-highlight-style (quote bm-highlight-line-and-fringe))
 '(canlock-password "65fd570ab0dff8c5eab2bf832f5e58157748881a")
 '(cider-default-repl-command "boot")
 '(cider-jack-in-default "boot")
 '(cider-preferred-build-tool "boot")
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-display-in-current-window t)
 '(cider-repl-pop-to-buffer-on-connect (quote display-only))
 '(cljr-auto-clean-ns t)
 '(cljr-hotload-dependencies t)
 '(cljr-suppress-middleware-warnings t)
 '(cljr-warn-on-eval nil)
 '(clojure-project-root-function
   (lambda
     (dir-name)
     (ignore-errors
       (projectile-project-root))))
 '(comment-line-break-function (quote indent-new-comment-line) t)
 '(comment-multi-line t)
 '(comment-start "/** " t)
 '(comment-start-skip "/\\*\\*" t)
 '(company-abort-manual-when-too-short t)
 '(company-auto-complete-chars (quote (41 46 47)))
 '(company-backends
   (quote
    (company-capf company-dabbrev company-dabbrev-code company-etags company-css company-files company-semantic company-abbrev)))
 '(company-idle-delay 0.4)
 '(company-minimum-prefix-length 1)
 '(company-occurrence-weight-function (quote company-occurrence-prefer-closest-above))
 '(company-search-regexp-function (quote company-search-words-regexp))
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-process-setup-function nil t)
 '(compilation-scroll-output t)
 '(compilation-skip-to-next-location nil t)
 '(compilation-window-height 12)
 '(compile-auto-highlight t)
 '(confirm-kill-emacs nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(css-indent-offset 2)
 '(cua-delete-selection t)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(default-frame-alist
    (quote
     ((left . 40)
      (top . 40)
      (width . 160)
      (height . 30))))
 '(dired-filter-group-saved-groups
   (quote
    (("((\"default\"
  (\"Clojure\"
   (extension . \"clj\" \"cljs\"))
  (\"JavaScript\"
   (extension \"js\"))
  (\"Org\"
   (extension . \"org\"))
  (\"Archives\"
   (extension \"zip\" \"rar\" \"gz\" \"bz2\" \"tar\"))))"))))
 '(dired-use-ls-dired nil)
 '(electric-indent-mode nil)
 '(epg-debug t)
 '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg2")
 '(epg-gpgsm-program "/usr/local/MacGPG2/bin/gpgsm")
 '(erc-nick "grabarz")
 '(feature-cucumber-command "cucumber --format pretty -s {options} {feature}")
 '(feature-use-rvm t)
 '(file-cache-filter-regexps
   (quote
    ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "tmp/")))
 '(flycheck-global-modes t)
 '(gh-api-v3-authenticator (quote gh-oauth-authenticator))
 '(git-link-open-in-browser t)
 '(global-flycheck-mode t)
 '(global-highlight-parentheses-mode t)
 '(global-hl-line-mode t)
 '(gnus-asynchronous nil)
 '(gnus-make-format-preserve-properties nil)
 '(gnus-parameter-large-newsgroup-initial 700)
 '(gnus-use-correct-string-widths t)
 '(golden-ratio-auto-scale nil)
 '(golden-ratio-exclude-buffer-names
   (quote
    ("*helm imenu*" "*helm etags*" "*Helm Swoop*" "*fzf*" "*Ediff Control Panel*" "*helm M-x*")))
 '(golden-ratio-exclude-buffer-regexp (quote ("undo")))
 '(golden-ratio-exclude-modes
   (quote
    ("helm-mode" "gnus-summary-mode" "gnus-group-mode" "gnus-article-mode" minimap-mode TERM)))
 '(golden-ratio-mode t)
 '(helm-boring-buffer-regexp-list (quote ("\\*" "\\` " "TAGS")))
 '(helm-buffer-max-length 60)
 '(helm-git-grep-pathspecs (quote ("*" ":!:*.inc.js" ":!:*yarn*" ":!:*.min.*")))
 '(helm-prevent-escaping-from-minibuffer t)
 '(helm-split-window-default-side (quote other))
 '(helm-split-window-inside-p t)
 '(highlight-symbol-idle-delay 1.1)
 '(hl-paren-colors (quote ("#aff" "#acf" "#a9f" "#a6f")))
 '(horizontal-scroll-bar-mode nil)
 '(ibuffer-always-compile-formats nil)
 '(ibuffer-display-summary nil)
 '(ibuffer-expert t)
 '(ibuffer-never-show-predicates (quote (".newsrc*" "*.*" "TAGS")) nil (ibuf-ext))
 '(ibuffer-saved-filter-groups
   (quote
    (("home"
      ("emacs-config"
       (or
        (filename . ".emacs")
        (filename . ".gnus")))
      ("Dired"
       (mode . dired-mode))
      ("Ruby"
       (mode . ruby-mode))
      ("CSS"
       (or
        (mode . scss-mode)
        (mode . css-mode)))
      ("JS"
       (mode . js2-mode))
      ("Clojure"
       (mode . clojure-mode))
      ("EShell"
       (mode . eshell-mode))
      ("Org"
       (or
        (mode . org-mode)))
      ("Gnus"
       (or
        (mode . message-mode)
        (mode . bbdb-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)))
      ("REPL"
       (name . "*cider-repl*"))
      ("ERB"
       (name . "*.erb*"))
      ("Magit"
       (name . "*magit"))
      ("ERC"
       (mode . erc-mode))
      ("Help"
       (or
        (name . "*Help*")
        (name . "*Apropos*")
        (name . "*info*")))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(ibuffer-use-header-line nil)
 '(jabber-account-list
   (quote
    (("michal.buczko@gmail.com"
      (:network-server . "talk.google.com")
      (:port . 443)
      (:connection-type . ssl)))))
 '(jabber-roster-line-format " %c %-25n %u %-8s")
 '(jabber-show-resources nil)
 '(js2-auto-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-global-externs
   (quote
    ("$" "Ext"
     (\, "describe")
     (\, "it")
     (\, "require"))))
 '(js2-include-browser-externs t)
 '(js2-include-node-externs t)
 '(js2-mirror-mode nil)
 '(js2-missing-semi-one-line-override t)
 '(mac-command-key-is-meta t)
 '(mac-option-key-is-meta nil)
 '(magit-commit-show-diff nil)
 '(magit-diff-auto-show (quote (stage-all log-oneline log-select blame-follow)))
 '(magit-process-connection-type nil)
 '(menu-bar-mode nil)
 '(mouse-drag-copy-region nil)
 '(mouse-wheel-progressive-speed nil)
 '(neo-smart-open t)
 '(org-agenda-current-time-string #("<-now-" 0 2 (org-heading t)))
 '(org-agenda-files nil)
 '(org-agenda-tags-column -80)
 '(org-catch-invisible-edits (quote smart))
 '(org-cycle-separator-lines 0)
 '(org-duration-format (quote ((special . h:mm))))
 '(org-email-link-description-format "Email %c: %s")
 '(org-log-into-drawer nil)
 '(org-log-state-notes-insert-after-drawers t)
 '(org-tags-column -90)
 '(org-todo-keyword-faces
   (quote
    (("TODO" . "green")
     ("BLOCKED" . "red")
     ("" . "")
     ("WAITING" . "yellow")
     ("IN-PROGRESS" . "white"))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO" "IN-PROGRESS" "WAITING" "BLOCKED" "DONE"))))
 '(package-selected-packages
   (quote
    (ob-restclient poporg ox-hugo magit-todos pdf-tools org-bullets deadgrep hackernews jabber company-auctex auctex helm-cider company-tern flycheck-joker zprint-mode lorem-ipsum stylus-mode ripgrep cljr-helm clj-refactor groovy-mode helm-css-scss zop-to-char ws-butler bm crux dash-at-point dired-subtree flycheck golden-ratio goto-last-change helm-clojuredocs helm-git-grep highlight-parentheses highlight-symbol ibuffer-projectile magit-gitflow mark-multiple neotree paxedit smartrep w3m window-numbering persistent-soft yaml-mode xref-js2 which-key wgrep web-mode web-beautify visual-fill-column use-package undo-tree smartparens smart-forward shrink-path shackle sass-mode rainbow-mode rainbow-delimiters quickrun quelpa nav-flash magit json-mode imenu-list imenu-anywhere hl-todo highlight-quoted highlight-numbers highlight-indentation helm-xref helm-swoop helm-projectile helm-describe-modes helm-company helm-ag gitignore-mode gitconfig-mode git-timemachine git-link git-gutter-fringe gist exec-path-from-shell emmet-mode emacs-snippets eldoc-eval editorconfig dumb-jump doom-themes dockerfile-mode dired-k company-web company-statistics company-shell company-quickhelp company-dict auto-yasnippet auto-compile)))
 '(popup-kill-ring-popup-width 50 t)
 '(projectile-completion-system (quote helm))
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-buffers (quote ("TAGS" "^\\\\*")))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "~/.emacs.d/.local/" ".sync" "build" "target" "dist")))
 '(projectile-globally-ignored-file-suffixes
   (quote
    ("groovy" "java" "gz" "png" "log" "min.css" "min.js" "inc.js")))
 '(projectile-globally-ignored-files (quote ("TAGS")))
 '(projectile-idle-timer-seconds 45)
 '(projectile-indexing-method (quote alien))
 '(projectile-mode-line
   (quote
    (:eval
     (format " Projectile[%s]"
             (projectile-project-name)))))
 '(projectile-project-root-files
   (quote
    ("rebar.config" "project.clj" "build.boot" "pom.xml" "build.sbt" "Gemfile" "setup.py" "TAGS" "GTAGS")))
 '(projectile-require-project-root t)
 '(rainbow-delimiters-outermost-only-face-count 2)
 '(recentf-exclude
   (quote
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" prelude-recentf-exclude-p ".*TAGS")))
 '(safe-local-variable-values
   (quote
    ((eval when
           (require
            (quote projectile))
           (setq projectile-enable-idle-timer t projectile-project-name "cerber-roles" cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history")))
     (eval when
           (require
            (quote projectile))
           (setq cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history")))
     (eval when
           (require
            (quote projectile))
           (setq projectile-enable-idle-timer nil cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history")))
     (eval progn
           (pp-buffer)
           (indent-buffer))
     (eval when
           (require
            (quote projectile))
           (setq projectile-enable-idle-timer t cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history"))))))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(select-enable-clipboard nil)
 '(select-enable-primary nil)
 '(show-paren-mode t)
 '(shr-color-visible-luminance-min 70)
 '(size-indication-mode t)
 '(smartparens-global-strict-mode t)
 '(smiley-style (quote medium))
 '(sp-base-key-bindings (quote sp))
 '(sp-navigate-close-if-unbalanced nil)
 '(sp-override-key-bindings
   (quote
    (("M-r" . cider-switch-repl)
     ("M-s" . projectile-find-file)
     ("M-x" . clipboard-cut-line-or-region)
     ("M-<up>" . sp-backward-up-sexp)
     ("M-<down>" . sp-down-sexp)
     ("M-<right>" . sp-forward-sexp)
     ("M-<left>" . sp-backward-sexp)
     ("M-C-<down>" . paxedit-transpose-forward)
     ("M-C-<up>" . paxedit-transpose-backward))))
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(tab-width 4)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(undo-tree-visualizer-diff t)
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 4)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-script-padding 0)
 '(window-min-width 30)
 '(window-numbering-mode t)
 '(yas-alias-to-yas/prefix-p nil)
 '(yas-snippet-dirs
   (quote
    (emacs-snippets-dir +file-templates-dir "~/.emacs.d/snippets")))
 '(yas-wrap-around-region t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "nil" :slant normal :weight light :height 141 :width normal))))
 '(doom-modeline-buffer-file ((t (:inherit mode-line-buffer-id))))
 '(doom-modeline-buffer-major-mode ((t (:inherit mode-line-emphasis))))
 '(doom-modeline-buffer-modified ((t (:inherit error))))
 '(doom-modeline-buffer-path ((t (:inherit mode-line-emphasis))))
 '(doom-modeline-info ((t (:inherit success))))
 '(doom-modeline-urgent ((t (:inherit error))))
 '(doom-modeline-warning ((t (:inherit warning))))
 '(gnus-summary-normal-ancient ((t (:inherit nil :foreground "#5B6268"))))
 '(gnus-summary-normal-unread ((t (:inherit medium :foreground "#98be65"))))
 '(helm-source-header ((t (:background "#202328" :foreground "#5B6268" :height 0.8))))
 '(highlight-symbol-face ((t (:underline t))))
 '(magit-diff-added-highlight ((t (:background "#3e493d" :foreground "#98be65" :weight normal))))
 '(magit-diff-file-heading-selection ((t (:background "#2257A0" :foreground "#c678dd" :weight normal))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#a9a1e1" :foreground "#282c34" :weight normal))))
 '(magit-diff-our-highlight ((t (:inherit magit-diff-removed-highlight :weight normal))))
 '(magit-diff-removed-highlight ((t (:background "#4f343a" :foreground "#ff6c6b" :weight normal))))
 '(magit-diff-their-highlight ((t (:inherit magit-diff-added-highlight :weight normal))))
 '(magit-header-line ((t (:background "#2257A0" :foreground "#DFDFDF" :box (:line-width 3 :color "#2257A0") :weight normal))))
 '(mc/cursor-bar-face ((t (:background "#51afef" :foreground "gold" :height 1))))
 '(mc/cursor-face ((t (:inherit cursor :foreground "gold"))))
 '(mode-line ((t (:background "#1c1e24" :box nil :height 0.9))))
 '(mode-line-inactive ((t (:background "#1d2026" :foreground "#5B6268" :box nil :height 0.9)))))
