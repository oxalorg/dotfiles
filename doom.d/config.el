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
(setq doom-theme 'doom-outrun-electric)
;; (setq doom-font (font-spec :family "Fira Code" :size 19))
(setq doom-font (font-spec :family "Iosevka" :size 21))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-journal-file-type 'monthly
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
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

(advice-add #'turn-on-evil-mode :before
            (lambda (&optional args)
              (when (eq major-mode 'fundamental-mode)
                (hack-local-variables))))

;; (add-hook 'lisp-mode-hook #'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)

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
      "," #'cider-switch-to-repl-buffer-same-window-force)

(map! :localleader
      :map cider-repl-mode-map
      "," #'evil-switch-to-windows-last-buffer)

(map! :localleader
      :map cider-repl-mode-map
      :nvm "<tab>" #'cider-repl-switch-to-other)

(map! :leader
      :map (clojure-mode-map clojurescript-mode-map)
      :prefix "e"
      "e" #'cider-eval-last-sexp
      "d" #'cider-eval-defun-at-point
      "b" #'cider-eval-buffer
      "D" #'cider-pprint-eval-defun-to-comment
      "p" #'cider-pprint-eval-last-sexp
      "P" #'cider-pprint-eval-last-sexp-to-comment)

(setq cider-known-endpoints
      '(("pitch-app/desktop-app" "localhost" "7888")))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq ob-mermaid-cli-path "~/mermaid/node_modules/.bin/mmdc")
(setq ivy-extra-directories nil)

(after! evil
  (setq evil-move-beyond-eol t)
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
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.9 :vslot -4 :select t :quit nil :ttl 0))
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

(use-package! lsp
  :config
  (let ((lsp-dirs-to-ignore
         '("[/\\\\]\\.cpcache\\'"
           "[/\\\\]\\.datomic\\'"
           "[/\\\\]cljs-runtime\\'"
           "[/\\\\]\\.lsp\\'"
           "[/\\\\]\\.store\\'"
           "[/\\\\]\\.shadow-cljs\\'")))
    (dolist (item lsp-dirs-to-ignore)
      (add-to-list 'lsp-file-watch-ignored-directories item))))

;; (mapcar (lambda (f) (set-face-foreground f "dim gray"))
;;         '(lsp-ui-sideline-code-action lsp-ui-sideline-current-symbol lsp-ui-sideline-symbol lsp-ui-sideline-symbol-info))


;; (use-package counsel
;;   :bind
;;   (("M-y" . counsel-yank-pop)
;;    :map ivy-minibuffer-map
;;    ("M-y" . ivy-next-line)))

(setq org-image-actual-width (/ (display-pixel-width) 3))
