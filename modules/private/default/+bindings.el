;;; private/default/+bindings.el -*- lexical-binding: t; -*-

(define-key global-map [remap find-tag] #'projectile-find-tag)

(define-key global-map [remap newline]  #'doom/newline-and-indent)
(define-key global-map [remap sp-splice-sexp-killing-backward] #'paxedit-delete)

(define-key global-map (kbd "M-w")       nil)
(define-key global-map (kbd "M-e")       #'helm-M-x)
(define-key global-map (kbd "M-:")       #'eval-expression)

(define-key global-map (kbd "M-=")       (λ! (text-scale-set 0)))
(define-key global-map (kbd "M-+")       #'text-scale-increase)
(define-key global-map (kbd "M--")       #'text-scale-decrease)

(define-key global-map (kbd "C-a")       #'doom/backward-to-bol-or-indent)
(define-key global-map (kbd "M-c")       #'clipboard-kill-ring-save)
(define-key global-map (kbd "M-x")       #'clipboard-cut-line-or-region)
(define-key global-map (kbd "M-v")       #'clipboard-yank)
(define-key global-map (kbd "M-z")       #'undo-fu-only-undo)
(define-key global-map (kbd "M-_")       #'undo-fu-only-redo)

(define-key global-map (kbd "M-l")       #'helm-buffers-list)
(define-key global-map (kbd "M-a")       #'find-tag-without-ns)
(define-key global-map (kbd "M-p")       #'indent-defn)
(define-key global-map (kbd "M-q")       #'kill-buffer-and-window)
(define-key global-map (kbd "M-t")       #'projectile-toggle-between-implementation-and-test)
(define-key global-map (kbd "M-s")       #'projectile-find-file)
(define-key global-map (kbd "M-;")       #'comment-or-uncomment-region-or-line)
(define-key global-map (kbd "M-(")       #'wrap-round)
(define-key global-map (kbd "M-[")       #'wrap-square)
(define-key global-map (kbd "M-{")       #'wrap-curly)
(define-key global-map (kbd "M-/")       #'company-complete)

(define-key global-map (kbd "C-)")       #'sp-forward-slurp-sexp)
(define-key global-map (kbd "C-(")       #'sp-forward-barf-sexp)
(define-key global-map (kbd "C-}")       #'sp-backward-barf-sexp)
(define-key global-map (kbd "C-{")       #'sp-backward-slurp-sexp)

(define-key global-map (kbd "C-f")       #'swiper-thing-at-point)
(define-key global-map (kbd "C-o")       #'helm-semantic-or-imenu)
(define-key global-map (kbd "C-w")       #'sp-backward-kill-word)
(define-key global-map (kbd "C->")       #'mc/mark-more-like-this-extended)
(define-key global-map (kbd "C-<")       #'mc/mark-all-like-this-dwim)
(define-key global-map (kbd "C-|")       #'whack-whitespace)
(define-key global-map (kbd "C-z")       #'zop-to-char)

(define-key global-map (kbd "M-w M-l")   #'avy-goto-char-timer)
(define-key global-map (kbd "M-w M-e")   #'mark-from-point-to-end-of-line)
(define-key global-map (kbd "M-w M-q")   #'er/mark-inside-quotes)
(define-key global-map (kbd "M-w M-p")   #'er/mark-inside-pairs)
(define-key global-map (kbd "M-w M-w")   #'er/mark-word)
(define-key global-map (kbd "M-w M-d")   #'er/mark-defun)
(define-key global-map (kbd "M-w i")     #'er/mark-inner-tag)
(define-key global-map (kbd "M-w o")     #'er/mark-outer-tag)
(define-key global-map (kbd "C-c k")     #'helm-all-mark-rings)
(define-key global-map (kbd "C-c m")     #'magit-status)
(define-key global-map (kbd "C-c t")     #'global-toggles/body)
(define-key global-map (kbd "C-c d")     #'dev-actions/body)
(define-key global-map (kbd "C-c o")     #'helm-org-capture-templates)
(define-key global-map (kbd "C-x d")     #'crux-duplicate-current-line-or-region)
(define-key global-map (kbd "C-x a")     #'helm-git-grep-at-point)
(define-key global-map (kbd "C-x s")     #'helm-git-grep)
(define-key global-map (kbd "C-x i")     #'helm-imenu-in-all-buffers)
(define-key global-map (kbd "C-x o")     #'helm-occur)
(define-key global-map (kbd "C-x k")     #'helm-show-kill-ring)
(define-key global-map (kbd "C-x f")     #'fzf-git)
(define-key global-map (kbd "C-x w")     #'close-other)
(define-key global-map (kbd "C-x q")     #'kill-this-buffer)
(define-key global-map (kbd "C-x v d")   #'magit-diff-popup)
(define-key global-map (kbd "C-x v =")   #'magit-diff-buffer-file)
(define-key global-map (kbd "C-x C-b")   #'projectile-ibuffer)
(define-key global-map (kbd "C-x C-i")   #'projectile-find-tag)
(define-key global-map (kbd "C-x C-h")   #'helm-resume)
(define-key global-map (kbd "C-x C-r")   #'helm-mini)
(define-key global-map (kbd "C-x C-l")   #'helm-bm)
(define-key global-map (kbd "C-x C-m")   #'bm-toggle)
(define-key global-map (kbd "C-S-h")     #'highlight-symbol-at-point)
(define-key global-map (kbd "M-RET")     #'smart-sexp-open-line)

(define-key global-map [C-S-down]        #'highlight-symbol-next)
(define-key global-map [C-S-up]          #'highlight-symbol-prev)
(define-key global-map [(C-backspace)]   #'backward-kill-word)
(define-key global-map [(C-S-return)]    #'er/expand-region)
(define-key global-map [(control ?.)]    #'goto-last-change)
(define-key global-map [(control ?,)]    #'goto-last-change-reverse)
(define-key global-map [(shift return)]  #'crux-smart-open-line)

(smartrep-define-key global-map "C-x v"
  '(("]"   . git-gutter:next-hunk)
    ("["   . git-gutter:previous-hunk)
    ("|"   . git-gutter:stage-hunk)
    ("\\"  . git-gutter:revert-hunk)))

(smartrep-define-key global-map "C-x"
  '(("]" . highlight-symbol-next)
    ("[" . highlight-symbol-prev)))
