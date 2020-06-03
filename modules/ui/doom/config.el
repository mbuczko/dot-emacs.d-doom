;;; ui/doom/config.el -*- lexical-binding: t; -*-

;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :config
  (unless doom-theme
    (setq doom-theme 'doom-one)
    (after! solaire-mode
      (add-hook 'doom-init-ui-hook #'solaire-mode-swap-bg t)
      (add-hook 'doom-init-ui-hook #'centaur-tabs-headline-match)))

  ;; improve integration w/ org-mode
  (add-hook 'doom-load-theme-hook #'doom-themes-org-config)

  ;; more Atom-esque file icons for neotree/treemacs
  (when (featurep! :ui neotree)
    (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-neotree-enable-variable-pitch t
          doom-neotree-file-icons 'simple
          doom-neotree-line-spacing 2))
  (when (featurep! :ui treemacs)
    (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)
    (setq doom-treemacs-enable-variable-pitch t)))

(def-package! solaire-mode
  :hook (after-change-major-mode . turn-on-solaire-mode)
  :hook (doom-popup-mode . turn-off-solaire-mode)
  :config
  (setq solaire-mode-real-buffer-fn #'doom-real-buffer-p)

  ;; Prevent color glitches when reloading either DOOM or the theme
  (add-hook! '(doom-init-ui-hook doom-reload-hook) #'solaire-mode-reset)

  (add-hook!
    (gist-mode twittering-mode mu4e-view-mode org-tree-slide-mode +regex-mode)
    #'solaire-mode))


(after! hideshow
  (defface +doom-folded-face
    `((((background dark))
       (:inherit font-lock-comment-face :background ,(doom-color 'base0)))
      (((background light))
       (:inherit font-lock-comment-face :background ,(doom-color 'base3))))
    "Face to hightlight `hideshow' overlays."
    :group 'doom)

  ;; Nicer code-folding overlays (with fringe indicators)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (when (featurep 'vimish-fold)
              (overlay-put
               ov 'before-string
               (propertize "…" 'display
                           (list vimish-fold-indication-mode
                                 'empty-line
                                 'vimish-fold-fringe))))
            (overlay-put
             ov 'display (propertize "  [...]  " 'face '+doom-folded-face))))))


;; NOTE Adjust these bitmaps if you change `doom-fringe-size'
(after! flycheck
  ;; because git-gutter is in the left fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "...X...."
    "..XX...."
    ".XXX...."
    "XXXX...."
    ".XXX...."
    "..XX...."
    "...X...."))

;; subtle diff indicators in the fringe
(after! git-gutter-fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."))

(def-package! perspective
  :commands (persp-mode persp-switch persp-rename)
  :init
  (persp-mode))

(def-package! fancy-narrow
  :commands (fancy-narrow-mode fancy-widen fancy-narrow-to-defun fancy-narrow-to-region))

(def-package! centaur-tabs
  :init
  (setq x-underline-at-descent-line t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "●"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-height 24
        centaur-tabs-style "box"
        centaur-tabs-set-bar 'under)
  :config
  (centaur-tabs-change-fonts "iosevka" 120)
  ;(centaur-tabs-headline-match)
  ;(centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-suffix-p "TAGS" name)
       (string-prefix-p "*Article" name)
       (string-prefix-p "*Summary" name)
       (string-prefix-p "*Group" name)
       (string-prefix-p " *Minibuf" name)
       (string-prefix-p " *NeoTree*" name)
       (string-prefix-p " *transient" name)
       (string-prefix-p " *which" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*cider" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))))))

(def-package! golden-ratio
  :commands golden-ratio
  :config
  (setq golden-ratio-auto-scale nil
        golden-ratio-exclude-buffer-names '("*helm imenu*" "*helm etags*" "*Helm Swoop*" "*fzf*" "*Ediff Control Panel*" "*helm M-x*" "*transient*")
        golden-ratio-exclude-buffer-regexp '("undo" "transient")
        golden-ratio-exclude-modes '("helm-mode" "gnus-summary-mode" "gnus-group-mode" "gnus-article-mode" minimap-mode TERM)
        golden-ratio-mode t))

(def-package! deft
  :commands deft
  :config
  (setq deft-extensions '("org" "md" "tex" "txt")
        deft-default-extension "org"
        ;; de-couples filename and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-org-mode-title-prefix t
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
              '((noslash . "-")
                (nospace . "-")
                (case-fn . downcase))))

(def-package! ibuffer
  :commands ibuffer
  :config
  (setq  ibuffer-always-compile-formats nil
         ibuffer-display-summary nil
         ibuffer-expert t
         ;ibuffer-never-show-predicates '(".newsrc*" "*.*" "TAGS") nil (ibuf-ext)
         ibuffer-saved-filter-groups '(("home"
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
                                          (name . "*info*")))))
         ibuffer-show-empty-filter-groups nil
         ibuffer-use-header-line nil))
