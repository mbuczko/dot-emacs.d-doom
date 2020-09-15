;;; lang/org/+babel.el -*- lexical-binding: t; -*-

(defvar +org-babel-languages
  '(;calc
    ;css
    emacs-lisp
    ;haskell
    js
    latex
    ;ledger
    ;lilypond
    lisp
    ;matlab
    plantuml
    ;python
    restclient ; ob-restclient
    ruby
    ;rust       ; ob-rust
    shell
    sqlite
    sql-mode   ; ob-sql-mode
    ;translate   ; ob-translate
    )
  "A list of org-babel languages to load.")


(after! org
  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil
        org-display-inline-images t
        org-startup-with-inline-images "inlineimages"
        org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar"
        org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2019.6/libexec/plantuml.jar")

  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-loop for sym in +org-babel-languages
            collect (cons sym t)))

  ;; I prefer C-c C-c for confirming over the default C-c '
  (define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

  ;; To update and redraw diagrams
  ;; (define-key org-mode-map (kbd "C-c C-p") (lambda ()
  ;;                                            (interactive)
  ;;                                            (org-ctrl-c-ctrl-c)
  ;;                                            (org-redisplay-inline-images)))

  ;; In a recent update, `org-babel-get-header' was removed from org-mode, which
  ;; is something a fair number of babel plugins use. So until those plugins
  ;; update, this polyfill will do:
  (defun org-babel-get-header (params key &optional others)
    (cl-loop with fn = (if others #'not #'identity)
             for p in params
             if (funcall fn (eq (car p) key))
             collect p)))
