;;; -*- no-byte-compile: t -*-

;; This is your user keys file, here you can configure key bindings that will
;; get added to Corgi. You can also override Corgi's default bindings this way.
;;
;; Bindings here are nested, e.g. `("SPC" ("b" ("k" kill-buffer)))' means that
;; "space" followed by "b" and then "k" will invoke `M-x kill-buffer'.
;;
;; You can add a descriptions before the command, this will show up in a pop-up
;; when you press the prefix key and wait a bit. (This uses which-key)
;;
;; `("SPC" ("b" ("k" "Choose a buffer to kill" kill-buffer)))'
;;
;; Instead of a prefix key you can use a symbol like `normal' or `insert', which
;; designates the Evil state (what vim calls the mode). `global' means any
;; state, `normal|visual' means either normal or visual.
;;
;; Instead of a command like `kill-buffer' you can put a keyword like
;; `:eval/buffer'. This is called a "signal". In the `corgi-signals' (or
;; `user-signals') file these are bound to specific commands based on the major
;; mode. E.g. in Emacs Lisp `:eval/buffer' means `eval-buffer', whereas in
;; Clojure it means `cider-eval-buffer'.

(bindings
 ;; "global" bindings are always active regardless of Evil's "state" (= vim mode)
 ;; If you don't provide this the default is `normal'.
 (global
  ;; ("M-x" "meta-x" counsel-M-x)
  )

 (motion
  (":" "Swap" evil-repeat-find-char)
  (";" "Swap" evil-ex))

 ;; Bindings for commands are usually only active in normal and visual state.
 (normal|visual
  ;; ("I" "Start insert on forms" evil-cp-insert-at-beginning-of-form)
  ("[" "unimpared behind"
   ("c" "Next hunk of git change" git-gutter:previous-hunk))

  ("]" "unimpared ahead"
   ("c" "Next hunk of git change" git-gutter:next-hunk))

  ("<backspace>" "Switch to previous buffer" corgi/switch-to-previous-buffer)
  ("z" "zzzzz"
   ("z" "Fold toggle" evil-toggle-fold))

  ("SPC" "leader"
   ("a" "End append on forms" evil-cp-insert-at-end-of-form)
   ("0" "Select Treemacs" treemacs-select-window)
   ("=" "Zoom in" default-text-scale-increase)
   ("-" "Zoom out" default-text-scale-decrease)
   ("[" "Prev error" flycheck-previous-error)
   ("]" "Next error" flycheck-next-error)
   ;; ("`" "Switch to from cider" :switch-to-from-cider-repl)

   ("c" "clojure"
    ("a" "add arity" clojure-add-arity))

   ("f"
    ("f" "Projectile file" projectile-find-file)
    ("t" "Turn Treemacs on/off" treemacs)
    ("T" "Focus current file in file tree" treemacs-find-file)
    ("e" "edit"
     ("o" "open init.org" ox/open-init-org)))

   ("g" "Git"
    ("s" "Magit Status" magit-status)
    ("r" "Git repo home" git-link-homepage)
    ("l" "Git repo link" git-link)
    ("g" "Git status" magit-status))

   ("r" "(w)rap"
    ("r" "round insert" ox/open-round-insert)
    ("w" "wrap" paredit-wrap-round))

   ("t" "Toggle"
    ("t" "parens toggle" ox/toggle-parens))

   ("y" "Yank"
    ("p" "Yank pop list" consult-yank-pop))

   ("o" "Go to other window" other-window)

   ("w" "Windows"
    ("w" "Go to other window" other-window)
    ("r" "Rotate / swap windows" window-swap-states)
    ("s" "Rotate / swap windows" window-swap-states))

   ("e" "Evaluate expressions"
    ("d" "Eval defun at point" :eval/outer-sexp)
    ("D" "Eval defun at point to a comment" :eval/outer-sexp-comment)
    ("b" "Eval buffer" :eval/buffer)
    ("e" "Eval form before cursor" :eval/last-sexp)
    ("p" "Eval and pretty print" :eval/last-sexp-pprint)
    ;; TODO: make this consistent, in clojure buffers it prints to a comment, in elisp it inserts directly
    ("P" "Eval to comment" :eval/last-sexp-pprint-comment)
    ("n" "Eval ns form" :eval/ns-form)
    ("r" "Eval region" :eval/region)
    ("i" "Interrupt eval" :eval/interrupt)
    ("-" "Eval up to point" :eval/up-to-point))

   ("SPC" "Switch buffer" consult-buffer)
   ("/" "Search in project" consult-ripgrep)
   ("9" "Clever slurp backwd" evil-cp-<)
   ("0" "Clever barf backwd" evil-cp->)
   ("RET" "Resume last sess" ivy-resume)
   ("." "Find file" :file/open)

   )))
