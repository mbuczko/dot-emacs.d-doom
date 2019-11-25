;;; lang/org/+capture.el -*- lexical-binding: t; -*-

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(defvar +org-default-todos-file "todo.org"
  "TODO")

(defvar +org-default-notes-file "notes.org"
  "TODO")

(defvar org-capture-templates
  '(("t" "Todo" entry
     (file+headline +org-default-todos-file "Todos")
     "* [ ] %?\n%i" :prepend t :kill-buffer t)

    ("n" "Notes" entry
     (file+headline +org-default-notes-file "Inbox")
     "* %u %?\n%i" :prepend t :kill-buffer t)

    ("i" "Issue" entry
     (file+headline "~/workspace/pitch-app/worklog.org" "Ongoing")
     "** %?%(with-current-buffer (org-capture-get :original-file-nondirectory) (github--babel-codeblock (thing-at-point 'line t)))"
     :immediate-finish t)))

(after! org
  (defvaralias 'org-default-notes-file '+org-default-notes-file)
  (defvaralias 'org-default-todos-file '+org-default-todos-file)

  (setq org-default-notes-file (expand-file-name +org-default-notes-file +org-dir)
        org-default-todos-file (expand-file-name +org-default-todos-file +org-dir))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame))
