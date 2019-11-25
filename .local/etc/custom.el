;;; ../.local/etc/custom.el -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources (quote ("~/.authinfo" "~/.authinfo.gpg")))
 '(auto-window-vscroll nil t)
 '(bm-highlight-style (quote bm-highlight-only-line))
 '(bm-recenter t)
 '(canlock-password "65fd570ab0dff8c5eab2bf832f5e58157748881a")
 '(comment-line-break-function (quote indent-new-comment-line) t)
 '(comment-multi-line t)
 '(comment-start "/** " t)
 '(comment-start-skip "/\\*\\*" t)
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
 '(eldoc-idle-delay 0.8)
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
 '(flycheck-global-modes (quote (not css-mode html-mode)))
 '(gh-api-v3-authenticator (quote gh-oauth-authenticator))
 '(git-link-open-in-browser t)
 '(global-flycheck-mode t)
 '(global-highlight-parentheses-mode t)
 '(global-hl-line-mode nil)
 '(global-so-long-mode t)
 '(gnus-asynchronous nil)
 '(gnus-make-format-preserve-properties nil)
 '(gnus-parameter-large-newsgroup-initial 700)
 '(gnus-use-correct-string-widths t)
 '(highlight-symbol-idle-delay 1.1)
 '(history-length 100)
 '(hl-paren-colors (quote ("#aff" "#acf" "#a9f" "#a6f")))
 '(hl-sexp-background-color "#442288")
 '(horizontal-scroll-bar-mode nil)
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
 '(mac-use-title-bar t)
 '(magit-commit-show-diff nil)
 '(magit-diff-auto-show (quote (stage-all log-oneline log-select blame-follow)))
 '(magit-process-connection-type nil)
 '(mouse-drag-copy-region nil)
 '(mouse-wheel-progressive-speed nil)
 '(neo-smart-open t)
 '(package-selected-packages
   (quote
    (helm-org ox-pandoc ox-gfm direnv org-sticky-header doom-themes direx dired-k ox-rfc w3m fancy-narrow swiper company-posframe perspective inf-ruby cargo company-racer racer flycheck-rust rust-mode toml-mode deft centaur-tabs shackle helm-cider github-stars powerthesaurus engine-mode pretty-hydra rfc-mode org-plus-contrib toc-org ob-restclient poporg ox-hugo magit-todos pdf-tools org-bullets deadgrep hackernews company-auctex auctex company-tern flycheck-joker zprint-mode lorem-ipsum stylus-mode ripgrep cljr-helm clj-refactor helm-css-scss zop-to-char ws-butler bm crux dash-at-point flycheck golden-ratio goto-last-change helm-clojuredocs helm-git-grep highlight-parentheses highlight-symbol ibuffer-projectile magit-gitflow mark-multiple neotree paxedit smartrep window-numbering persistent-soft yaml-mode xref-js2 which-key wgrep web-mode web-beautify visual-fill-column use-package undo-tree smartparens smart-forward shrink-path sass-mode rainbow-mode rainbow-delimiters quickrun quelpa nav-flash magit json-mode imenu-list imenu-anywhere hl-todo highlight-quoted highlight-numbers highlight-indentation helm-xref helm-swoop helm-projectile helm-describe-modes helm-company helm-ag gitignore-mode gitconfig-mode git-timemachine git-link git-gutter-fringe gist exec-path-from-shell emmet-mode emacs-snippets eldoc-eval editorconfig dockerfile-mode company-web company-statistics company-shell company-quickhelp company-dict auto-yasnippet auto-compile)))
 '(popup-kill-ring-popup-width 50 t)
 '(racer-command-timeout 3)
 '(rainbow-delimiters-outermost-only-face-count 2)
 '(recentf-exclude
   (quote
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" prelude-recentf-exclude-p ".*TAGS")))
 '(safe-local-variable-values
   (quote
    ((org-confirm-babel-evaluate)
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
           (setq projectile-enable-idle-timer t projectile-project-name "cerber-roles" cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history")))
     (eval when
           (require
            (quote projectile))
           (setq projectile-enable-idle-timer t cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history"))))))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position 1)
 '(select-enable-clipboard t)
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
     ("C-S-w" . sp-copy-sexp)
     ("M-<up>" . sp-backward-up-sexp)
     ("M-<down>" . sp-down-sexp)
     ("M-<right>" . smart-forward)
     ("M-<left>" . smart-backward)
     ("M-C-<down>" . paxedit-transpose-forward)
     ("M-C-<up>" . paxedit-transpose-backward))))
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(tab-width 4)
 '(tags-add-tables nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(transient-mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-buffer-identification)))
 '(undo-tree-visualizer-diff t)
 '(w3m-use-tab-line nil)
 '(web-mode-auto-close-style 2)
 '(web-mode-code-indent-offset 4)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-script-padding 0)
 '(window-min-width 30)
 '(window-numbering-mode t)
 '(yas-alias-to-yas/prefix-p nil)
 '(yas-snippet-dirs (quote (emacs-snippets-dir "~/.emacs.d/snippets")))
 '(yas-wrap-around-region t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "nil" :slant normal :weight light :height 141 :width normal))))
 '(bm-face ((t (:background "LightPink4" :foreground "orange" :weight semi-bold))))
 '(bm-fringe-face ((t (:background "LightPink4" :foreground "orange" :underline t))))
 '(company-tooltip-mouse ((t (:background "gray8" :foreground "#282c34"))))
 '(doom-modeline-buffer-file ((t (:inherit mode-line-buffer-id))))
 '(doom-modeline-buffer-major-mode ((t (:inherit mode-line-emphasis))))
 '(doom-modeline-buffer-modified ((t (:inherit error))))
 '(doom-modeline-buffer-path ((t (:inherit mode-line-emphasis))))
 '(doom-modeline-info ((t (:inherit success))))
 '(doom-modeline-urgent ((t (:inherit error))))
 '(doom-modeline-warning ((t (:inherit warning))))
 '(gnus-summary-normal-ancient ((t (:inherit nil :foreground "#5B6268"))))
 '(gnus-summary-normal-unread ((t (:inherit medium :foreground "#98be65"))))
 '(helm-separator ((t (:foreground "gray20"))))
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
 '(mode-line ((t (:background "#1c1e24" :box nil :height 0.92))))
 '(mode-line-inactive ((t (:background "#1d2026" :foreground "#5B6268" :box nil :height 0.9)))))
