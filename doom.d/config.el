;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Mitesh Shah"
      user-mail-address "mitesh@miteshshah.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Iosevka" :size 18))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-journal-file-type 'monthly
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq display-line-numbers-type nil)
(global-linum-mode 0)
(global-display-line-numbers-mode 0)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (setq browse-url-browser-function 'browse-url-firefox)

(setq-default
 delete-by-moving-to-trash t)

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(advice-add #'turn-on-evil-mode :before
            (lambda (&optional args)
              (when (eq major-mode 'fundamental-mode)
                (hack-local-variables))))

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; clojure
(after! cider
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-display-in-current-window t))

(map! :leader
      :desc "Slurp from forward"
      ;; "0" #'paredit-forward-slurp-sexp)
      "0" #'evil-cp->)

(map! :leader
      :desc "Slurp from backword"
      ;; "9" #'paredit-backward-slurp-sexp)
      "9" #'evil-cp-<)

(map! :leader
      :desc "Barf from forward"
      "2" #'paredit-forward-barf-sexp)

(map! :leader
      :desc "Barf from backword"
      "1" #'paredit-backward-barf-sexp)

;; (map! :localleader
;;       :desc "Eval list at point"
;;       "ew" #'cider-eval-list-at-point)

(defun cider-switch-to-repl-buffer-same-window-force ()
  (interactive)
  (let ((repl (cider-current-repl nil nil)))
    (if repl
        (switch-to-buffer repl)
        (switch-to-buffer (cider-current-repl 'any 'ensure)))))

(map! :localleader
      :map (clojure-mode-map clojurescript-mode-map)
      "n a" #'cljr-add-require-to-ns
      "n i" #'cljr-add-import-to-ns
      "n m" #'cljr-add-missing-libspec
      "n c" #'cljr-clean-ns
      "," #'cider-switch-to-repl-buffer-same-window-force)

(map! :localleader
      :map cider-repl-mode-map
      "," #'evil-switch-to-windows-last-buffer)

(map! :localleader
      :map cider-repl-mode-map
      :nvm "<tab>" #'cider-repl-switch-to-other)

(defun ox/cider-pprint-eval-toggle-defun-at-point ()
  (interactive)
  (cider-pprint-eval-defun-at-point)
  (switch-to-buffer-other-window "*cider-result*"))

(map! :leader
      :map (clojure-mode-map clojurescript-mode-map)
      :prefix "e"
      "e" #'cider-eval-last-sexp
      "d" #'cider-eval-defun-at-point
      "b" #'cider-eval-buffer
      "D" #'cider-pprint-eval-defun-to-comment
      "p" #'ox/cider-pprint-eval-toggle-defun-at-point
      "P" #'cider-pprint-eval-last-sexp)

(setq cider-known-endpoints
      '(("pitch-app/desktop-app" "localhost" "7888")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq ob-mermaid-cli-path "~/mermaid/node_modules/.bin/mmdc")
(setq ivy-extra-directories nil)

(after! evil
  (setq evil-move-beyond-eol t
        evil-kill-on-visual-paste nil)
  (map!
   :nvm ";" #'evil-ex
   :nvm ":" #'evil-repeat-find-char
   ;; :nvm "L" #'forward-sexp
   ;; :nvm "H" #'backward-sexp
   (:after org
    :map org-mode-map
    :leader
    :nv "j" #'org-metaup
    :nv "k" #'org-metadown)))

(undefine-key! global-map "M-8" "M-9" "M-0")

;; better terminal toggling
(after! vterm
  (set-popup-rule! "*doom:vterm-popup:*" :size 0.6 :vslot -4 :select t :quit nil :ttl 0))
(map! "<f8>" #'+vterm/toggle
      (:map vterm-mode-map
       :nvmi "<f8>" #'+vterm/toggle))

(defun ox/cider-quit-all ()
  "Quit all current CIDER REPLs. Thanks to @plexus"
  (interactive)
  (let ((repls (seq-remove (lambda (r)
                             (equal r (get-buffer "*babashka-repl*")))
                           (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
    (seq-do #'cider--close-connection repls))
  ;; if there are no more sessions we can kill all ancillary buffers
  (cider-close-ancillary-buffers)
  ;; need this to refresh sesman browser
  (run-hooks 'sesman-post-command-hook))

(after! projectile
  (setq projectile-sort-order 'recently-active))

(setq lsp-ui-sideline-actions-icon nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-position 'at-point)

(advice-add 'risky-local-variable-p :override #'ignore)

;; (use-package! lsp
;;   :config
;;   (let ((lsp-dirs-to-ignore
;;          '("[/\\\\]\\.cpcache\\'"
;;            "[/\\\\]\\.datomic\\'"
;;            "[/\\\\]cljs-runtime\\'"
;;            "[/\\\\]\\.lsp\\'"
;;            "[/\\\\]\\.store\\'"
;;            "[/\\\\]\\.shadow-cljs\\'")))
;;     (dolist (item lsp-dirs-to-ignore)
;;       (add-to-list 'lsp-file-watch-ignored-directories item))))

;; (mapcar (lambda (f) (set-face-foreground f "dim gray"))
;;         '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info))

;; (use-package clockify
;;   :load-path "~/projects/emacs-clockify"
;;   :init
;;   (setq clockify-api-key "")
;;   (setq clockify-workspace "5e86dee9c170232ca4be1432")
;; )

;; (use-package html-to-hiccup
;;   :load-path "~/projects/html-to-hiccup")

;; (use-package counsel
;;   :bind
;;   (("M-y" . counsel-yank-pop)
;;    :map ivy-minibuffer-map
;;    ("M-y" . ivy-next-line)))

(setq org-image-actual-width (/ (display-pixel-width) 3))

;; Allows to eval sexp with cursor "on" the last char instead of
;; "after" the last char. This is super helpful in vim normal / motion
;; states
;; https://github.com/syl20bnr/spacemacs/issues/646#issuecomment-106037404
(defadvice cider-last-sexp (around evil activate)
  "In normal-state or motion-state, last sexp ends at point."
  (if (or (evil-normal-state-p) (evil-motion-state-p))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        ad-do-it)
    ad-do-it))

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (inhibit-field-text-motion t)
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (and (not (bobp))
                            (= end (save-excursion
                                     (comment-forward (point-max))
                                     (point))))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (ignore-errors
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1)))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (eq 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(map! :localleader
      :map (clojure-mode-map clojurescript-mode-map)
      "=" #'zprint)

(defun html-to-hiccup-buffer ()
  (interactive)
  ;; (evil-visual-select (point-min) (point-max))
  (html-to-hiccup-convert-region (point-min) (point-max))
  (zprint))

(setq display-time-world-list
      '(("America/Chicago" "Wisconsin")
        ("America/Recife" "Recife")
        ("Europe/Berlin" "Berlin")
        ("Asia/Jerusalem" "Israel")
        ("Asia/Kolkata" "Mumbai")))
(setq display-time-world-time-format "%a, %l:%M %p")

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

(setq eros-eval-result-prefix "⟹ ")

;; copied from corgi
(setq evil-want-C-u-scroll t)

(setq ring-bell-function 'ignore
      display-time-world-list '(("America/Chicago" "Wisconsin")
                                ("America/Recife" "Recife")
                                ("Europe/Berlin" "Berlin")
                                ("Asia/Jerusalem" "Israel")
                                ("Asia/Kolkata" "Mumbai"))
      display-time-world-time-format "%a, %l:%M %p"
      dired-recursive-copies (quote always) ; “always” means no asking
      dired-recursive-deletes (quote top) ; “top” means ask once
      dired-dwim-target t ; Copy from one dired dir to the next dired dir shown in a split window
      split-height-threshold nil
      split-width-threshold 0)

(defun custom-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'custom-dired-mode-setup)

(with-eval-after-load 'dired
  (put 'dired-find-alternate-file 'disabled nil) ; disables warning
  (evil-define-key '(normal) dired-mode-map
    (kbd "RET") 'dired-find-alternate-file ; was dired-advertised-find-file
    (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ; was dired-up-directory

;; (setq ivy-initial-inputs-alist nil)

;; (global-superword-mode t)
;; (and window-system (server-start))
;; (desktop-save-mode 1)
;; (show-paren-mode 1)
;; (set-frame-font "Iosevka 20")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (use-package dired-subtree)
(use-package! org
  :config
  (setq org-startup-indented t))
;; (use-package counsel-projectile
;;   :init
;;   (setq projectile-indexing-method 'hybrid)
;;   ;; (setq projectile-sort-order 'recently-active)
;;   (setq projectile-sort-order 'recentf))
;; (use-package cherry-blossom-theme
;;   :config
;;   (load-theme 'cherry-blossom t))
;; (use-package! evil-smartparens)

(use-package! evil-cleverparens
  :commands evil-cleverparens-mode
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :config
  (map! :textobj "f" #'evil-cp-inner-form #'evil-cp-a-form)
  (setq evil-cleverparens-use-s-and-S nil)
  (evil-define-key '(normal visual) evil-cleverparens-mode-map
    "s" nil
    "S" nil
    "{" nil
    "}" nil
    "[" nil
    "]" nil
    ;;(kbd "<tab>") 'evil-jump-item)
    ))

(use-package! zprint-mode)
(use-package! evil-escape
  :config
  (setq-default evil-escape-key-sequence "qp")
  (evil-escape-mode))
;; (use-package html-to-hiccup
;;   :load-path "~/projects/html-to-hiccup")
(use-package! git-link
  :config
  (setq git-link-open-in-browser t
        git-link-use-commit t))
;; (use-package magit-delta
;;   :after (magit)
;;   :config
;;   (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1))))
;; (use-package diff-hl
;;   :config
;;   (evil-set-initial-state 'diff-hl-show-hunk-posframe--transient-mode 'motion)
;;   (global-diff-hl-mode))
;; (with-eval-after-load 'evil
;;   (fset 'evil-visual-update-x-selection 'ignore)
;;   (setq evil-kill-on-visual-paste nil)
;;   (setq evil-insert-state-cursor '(bar "green"))
;;   (setq-default evil-symbol-word-search t))

;; (with-eval-after-load 'diff-hl
;;   '(progn
;;      ;; let diff-hl inline popup keymaps take priority over evil
;;      (evil-make-overriding-map diff-hl-show-hunk--inline-popup-map 'normal)
;;      ;; force update evil keymaps after diff-hl-mode loaded
;;      (add-hook 'diff-hl-mode-hook #'evil-normalize-keymaps)))

;; (with-eval-after-load 'evil-maps
;;   (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
;;   (define-key evil-motion-state-map (kbd ";") 'evil-ex))

(defun html-to-hiccup-buffer ()
  (interactive)
  ;; (evil-visual-select (point-min) (point-max))
  (html-to-hiccup-convert-region (point-min) (point-max))
  (zprint))

;; (map! :localleader
;;       :map (clojure-mode-map clojurescript-mode-map)
;;       "=" #'zprint)

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

(defun ox/cider-eval-defun-at-point-and-run-test ()
  (interactive)
  (cider-eval-defun-at-point)
  (cider-test-run-test))

(defun ox/cider-switch-to-repl-buffer-same-window-force ()
  (interactive)
  (let ((repl (cider-current-repl nil nil)))
    (if repl
        (switch-to-buffer repl)
      (switch-to-buffer (cider-current-repl 'any 'ensure)))))


(defun ox/toggle-parens--replace (pair start end)
  "Replace parens with a new PAIR at START and END in current buffer.
   A helper function for `toggle-parens'."
  (goto-char start)
  (delete-char 1)
  (insert (substring pair 0 1))
  (goto-char end)
  (delete-char 1)
  (insert (substring pair 1 2))
  (goto-char start))

(defun ox/toggle-parens ()
  "Toggle parens () <> [] at cursor.

Turn on `show-paren-mode' to see matching pairs of parentheses
and other characters in buffers. This function then uses the same
function `show-paren-data-function' to find and replace them with
the other pair of brackets.

This function can be easily modified and expanded to replace
other brackets. Currently, mismatch information is ignored and
mismatched parens are changed based on the left one."
  (interactive)
  (let* ((parens (funcall show-paren-data-function))
         (start (if (< (nth 0 parens) (nth 2 parens))
                    (nth 0 parens) (nth 2 parens)))
         (end (if (< (nth 0 parens) (nth 2 parens))
                  (nth 2 parens) (nth 0 parens)))
         (startchar (buffer-substring-no-properties start (1+ start)))
         (mismatch (nth 4 parens)))
    (when parens
      (case startchar
        ("(" (ox/toggle-parens--replace "[]" start end))
        ("[" (ox/toggle-parens--replace "{}" start end))
        ("{" (ox/toggle-parens--replace "()" start end))))))

(defun ox/refresh-projects-dir ()
  (interactive)
  (projectile-discover-projects-in-directory "~/projects"))

(defun ox/open-round-insert ()
  (interactive)
  (paredit-open-round)
  (evil-insert 0))

(map! :map vertico-map
      "<tab>" #'vertico-next
       "<backtab>" #'vertico-previous
       ";" #'vertico-insert)

(setq deft-directory "~/journal"
      deft-default-extension "org"
      deft-extensions '("md" "org" "txt")
      deft-recursive t
      deft-use-filename-as-title t)

;; (setq save-interprogram-paste-before-kill t)

;; (evil-define-key '(normal visual) cider-repl-mode-map
;;   (kbd "SPC,") 'evil-switch-to-windows-last-buffer)

(use-package! smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; Dashboard quick actions
(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'doom/open-private-config
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)

;; (map! )
