;;; private/default/+bindings.el -*- lexical-binding: t; -*-

(map! [remap find-tag] #'projectile-find-tag
      [remap newline]  #'doom/newline-and-indent
      [remap sp-splice-sexp-killing-backward] #'paxedit-delete

      "M-w"       nil
      "M-e"       #'execute-extended-command
      "M-:"       #'eval-expression

      ;; Text-scaling
      "M-="       (λ! (text-scale-set 0))
      "M-+"       #'text-scale-increase
      "M--"       #'text-scale-decrease

      "C-a"       #'doom/backward-to-bol-or-indent
      "M-c"       #'clipboard-kill-ring-save
      "M-x"       #'clipboard-cut-line-or-region
      "M-v"       #'clipboard-yank
      "M-z"       #'undo

      "M-l"       #'helm-buffers-list
      "M-a"       #'find-tag-without-ns
      "M-p"       #'indent-defn
      "M-q"       #'kill-buffer-and-window
      "M-s"       #'projectile-find-file
      "M-t"       #'projectile-toggle-between-implementation-and-test
      "M-;"       #'comment-or-uncomment-region-or-line
      "M-("       #'wrap-round
      "M-["       #'wrap-square
      "M-{"       #'wrap-curly
      "M-/"       #'company-complete

      "C-o"       #'helm-semantic-or-imenu
      "C-b"       #'helm-buffers-list
      "C-w"       #'sp-backward-kill-word
      "C->"       #'mc/mark-more-like-this-extended
      "C-<"       #'mc/mark-all-like-this-dwim
      "C-|"       #'whack-whitespace
      "C-z"       #'zop-to-char

      "C-)"       #'sp-forward-slurp-sexp
      "C-("       #'sp-forward-barf-sexp
      "C-}"       #'sp-backward-barf-sexp
      "C-{"       #'sp-backward-slurp-sexp

      "M-w M-q"   #'er/mark-inside-quotes
      "M-w M-p"   #'er/mark-inside-pairs
      "M-w M-w"   #'er/mark-word
      "M-w M-d"   #'er/mark-defun
      "M-w i"     #'er/mark-inner-tag
      "M-w o"     #'er/mark-outer-tag
      "M-w e"     #'mark-from-point-to-end-of-line
      "C-c d"     #'crux-duplicate-current-line-or-region
      "C-c m"     #'magit-status
      "C-x a"     #'helm-git-grep-at-point
      "C-x s"     #'helm-git-grep
      "C-x i"     #'helm-imenu-in-all-buffers
      "C-x o"     #'helm-occur
      "C-x p"     #'popup-kill-ring
      "C-x d"     #'dash-at-point
      "C-x f"     #'deadgrep
      "C-x w"     #'close-other
      "C-x q"     #'kill-this-buffer
      "C-x v d"   #'magit-diff-popup
      "C-x v ="   #'magit-diff-buffer-file
      "C-x C-b"   #'projectile-ibuffer
      "C-x C-o"   #'avy-goto-char-timer
      "C-x C-i"   #'projectile-find-tag
      "C-x C-h"   #'helm-resume
      "C-x C-r"   #'helm-mini
      "C-x C-m"   #'bm-toggle
      "C-x C-l"   #'bm-show-all
      "C-h r"     #'cljr-helm
      "C-S-h"     #'highlight-symbol-at-point

      [C-S-down]        'highlight-symbol-next
      [C-S-up]          'highlight-symbol-prev
      [(C-backspace)]   'backward-kill-word
      [(C-M-return)]    'er/expand-region
      [(backtab)]       'helm-buffers-list
      [(control ?.)]    'goto-last-change
      [(control ?,)]    'goto-last-change-reverse

      (:map company-active-map
        "\e" 'company-abort)

      (:after helm
        (:map helm-map
           "ESC" nil
           "C-s" #'helm-minibuffer-history
           "C-u" #'helm-delete-minibuffer-contents
           "M-r" #'next-history-element
           "M-n" #'previous-history-element
           [escape] #'helm-keyboard-quit))

      ;; --- Built-in plugins -----------------------------
      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete))

(smartrep-define-key
    global-map "C-x v"
  '(("]"   . git-gutter:next-hunk)
    ("["   . git-gutter:previous-hunk)
    ("|"   . git-gutter:stage-hunk)
    ("\\"  . git-gutter:revert-hunk)))

