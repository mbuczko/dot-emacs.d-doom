;;; completion/company/config.el -*- lexical-binding: t; -*-

(def-setting! :company-backend (modes &rest backends)
  "Prepends BACKENDS to `company-backends' in major MODES.

MODES should be one major-mode symbol or a list of them."
  `(progn
     ,@(cl-loop for mode in (doom-enlist (doom-unquote modes))
                for def-name = (intern (format "doom--init-company-%s" mode))
                collect
                `(defun ,def-name ()
                   (when (and (eq major-mode ',mode)
                              ,(not (eq backends '(nil))))
                     (require 'company)
                     (make-variable-buffer-local 'company-backends)
                     (dolist (backend (list ,@(reverse backends)))
                       (cl-pushnew backend company-backends :test #'equal))))
                collect `(add-hook! ,mode #',def-name))))


;;
;; Packages
;;

(use-package company
  :commands (company-mode global-company-mode company-complete company-active-map
             company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-abort-manual-when-too-short t
        company-auto-complete-chars '(40 46 47)
        company-idle-delay 0.4
        company-tooltip-idle-delay nil
        company-tooltip-limit 8
        company-tooltip-margin 2
        company-tooltip-minimum-width 30
        company-tooltip-offset-display 'scrollbar
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
        company-frontends '(company-echo-metadata-frontend company-pseudo-tooltip-frontend)
        company-backends '(company-capf company-dabbrev company-dabbrev-code company-etags company-css company-files company-semantic company-abbrev)
        company-transformers '(company-sort-by-occurrence)
        company-minimum-prefix-length 3
        company-occurrence-weight-function 'company-occurrence-prefer-closest-above
        company-search-regexp-function 'company-search-words-regexp)

  (global-company-mode +1)
  (define-key company-active-map "\e" 'company-abort))

(use-package company-posframe
  :after company
  :commands (company-posframe-mode)
  :config
  (setq company-posframe-show-metadata nil
        company-posframe-show-indicator nil
        company-posframe-show-params (list :internal-border-color "#444"
                                           :internal-border-width 1)))

(use-package company-statistics
  :after company
  :config
  (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))


;;
;; Autoloads
;;

(autoload 'company-capf "company-capf")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
