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

(defvar +org-default-notes-file "inbox.org"
  "TODO")

(defvar +org-default-tils-file "tils.org"
  "TIL")

(defvar org-capture-templates
  '(("l" "Today I learned..." entry
     (file+headline +org-default-tils-file "Today I learned")
     "** %u %?\n%i" :prepend t :kill-buffer t)

    ("t" "Todo" entry
     (file+headline +org-default-todos-file "Todos")
     "** TODO %u %?\n%i" :prepend t :kill-buffer t)

    ("n" "Note" entry
     (file+headline +org-default-notes-file "Inbox")
     "* %u %?\n%i" :prepend t :kill-buffer t)

    ("c" "org-protocol-capture" entry
     (file+headline +org-default-notes-file "Links")
     "* [[%:link][%:description]]\n\n %i" :immediate-finish t)

    ("i" "Issue" entry
     (file+headline "~/workspace/pitch-app/worklog.org" "Worklog")
     "** %u %?%(with-current-buffer (org-capture-get :original-file-nondirectory) (github--babel-codeblock (thing-at-point 'line t)))"
     :immediate-finish t)))

(after! org
  (defvaralias 'org-default-notes-file '+org-default-notes-file)
  (defvaralias 'org-default-todos-file '+org-default-todos-file)
  (defvaralias 'org-default-tils-file  '+org-default-tils-file)

  (setq org-default-notes-file (expand-file-name +org-default-notes-file +org-dir)
        org-default-todos-file (expand-file-name +org-default-todos-file +org-dir)
        org-default-tils-file  (expand-file-name +org-default-tils-file  +org-dir)

        org-refile-targets (quote (("worklog.org" :maxlevel . 1)
                                   (+org-default-todos-file :level . 1))))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame))
