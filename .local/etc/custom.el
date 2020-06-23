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
 '(cljr-eagerly-build-asts-on-startup nil)
 '(cljr-find-usages-ignore-analyzer-errors t)
 '(cljr-ignore-analyzer-errors t)
 '(cljr-middleware-ignored-paths (quote ("/target" "/scripts" "/test")))
 '(comment-line-break-function (quote indent-new-comment-line) t)
 '(comment-multi-line t)
 '(comment-start "/** " t)
 '(comment-start-skip "/\\*\\*" t)
 '(company-idle-delay 0.2)
 '(company-posframe-quickhelp-delay nil)
 '(company-tooltip-margin 1)
 '(company-tooltip-maximum-width 64)
 '(company-tooltip-minimum 3)
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
 '(custom-safe-themes
   (quote
    ("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "1c8171893a9a0ce55cb7706766e57707787962e43330d7b0b6b0754ed5283cda" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "0736a8e34702a67d84e32e2af90145ed19824f661776a0e966cea62aa1943a6e" "72fda75af7caddec17ba9b49d2f99703c20a5f5f5c4dcec641d34a0b83569e88" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "d1c7f2db070c96aa674f1d61403b4da1fff2154163e9be76ce51824ed5ca709c" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "6de37d6d573e18138aa948683c8ff0e72b89e90d1cdbf683787ea72f8e6295ab" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" default)))
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
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(epg-gpgsm-program "/usr/local/bin/gpgsm")
 '(erc-nick "grabarz")
 '(feature-cucumber-command "cucumber --format pretty -s {options} {feature}")
 '(feature-use-rvm t)
 '(file-cache-filter-regexps
   (quote
    ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "tmp/")))
 '(find-file-visit-truename t)
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
 '(helm-candidate-number-limit 30)
 '(helm-completion-style (quote emacs))
 '(helm-grep-ag-command
   "rg --color=always --smart-case --no-heading --line-number %s %s %s")
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
 '(lsp-diagnostic-package :none)
 '(lsp-rust-all-targets nil)
 '(lsp-rust-analyzer-call-info-full nil)
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(mac-command-key-is-meta t)
 '(mac-option-key-is-meta nil)
 '(mac-use-title-bar t)
 '(magit-commit-show-diff nil)
 '(magit-diff-auto-show (quote (stage-all log-oneline log-select blame-follow)))
 '(magit-process-connection-type nil)
 '(mouse-drag-copy-region nil)
 '(mouse-wheel-progressive-speed nil)
 '(neo-smart-open t)
 '(org-bullets-bullet-list (quote ("◉" "○" "★" "✽" "✿" "✸")))
 '(org-use-speed-commands t)
 '(package-selected-packages
   (quote
    (helm-bm lsp-mode rustic beacon bibtex-completion org-ref company-posframe clj-refactor posframe magit-todos magit-gitflow magit fzf zprint-mode helm-org ox-pandoc ox-gfm direnv doom-themes direx dired-k ox-rfc w3m fancy-narrow swiper toml-mode deft centaur-tabs shackle helm-cider github-stars powerthesaurus pretty-hydra rfc-mode org-plus-contrib toc-org ob-restclient poporg ox-hugo pdf-tools org-bullets deadgrep company-auctex auctex company-tern flycheck-joker lorem-ipsum stylus-mode ripgrep helm-css-scss zop-to-char ws-butler bm crux dash-at-point flycheck golden-ratio goto-last-change helm-clojuredocs helm-git-grep highlight-parentheses highlight-symbol ibuffer-projectile mark-multiple neotree paxedit smartrep window-numbering persistent-soft yaml-mode xref-js2 which-key wgrep web-mode web-beautify visual-fill-column use-package undo-tree smartparens smart-forward shrink-path sass-mode rainbow-mode rainbow-delimiters quickrun quelpa nav-flash json-mode imenu-list imenu-anywhere hl-todo highlight-quoted highlight-numbers helm-xref helm-projectile helm-describe-modes helm-company helm-ag gitignore-mode gitconfig-mode git-timemachine git-link git-gutter-fringe gist emmet-mode emacs-snippets eldoc-eval editorconfig dockerfile-mode company-web company-statistics company-shell company-quickhelp company-dict auto-yasnippet auto-compile)))
 '(popup-kill-ring-popup-width 50 t)
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
           (setq projectile-enable-idle-timer t cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history")))
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (org-confirm-babel-evaluate)
     (eval when
           (require
            (quote projectile))
           (setq cider-repl-history-file
                 (concat
                  (projectile-project-root)
                  ".nrepl-history")
                 projectile-enable-idle-timer t)))))
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
 '(swiper-goto-start-of-match t)
 '(swiper-verbose nil)
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
 '(ws-butler-global-exempt-modes (quote (markdown-mode org-mode)))
 '(yas-alias-to-yas/prefix-p nil)
 '(yas-snippet-dirs (quote (emacs-snippets-dir "~/.emacs.d/snippets")))
 '(yas-wrap-around-region t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "nil" :slant normal :weight light :height 120 :width normal))))
 '(bm-face ((t (:background "maroon4"))))
 '(bm-fringe-face ((t (:background "LightPink4" :foreground "orange" :underline t))))
 '(company-tooltip ((t (:inherit tooltip :foreground "salmon"))))
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
 '(highlight-symbol-face ((t (:background "gray20" :foreground "light slate blue"))))
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
 '(mode-line-inactive ((t (:background "#1d2026" :foreground "#5B6268" :box nil :height 0.9))))
 '(org-column ((t (:background "grey18" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((t (:background "gray18" :foreground "dim gray" :weight bold :width normal :family "Iosevka"))))
 '(org-headline-done ((t (:foreground "#5B6268" :strike-through t)))))
