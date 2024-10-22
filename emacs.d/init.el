(use-package json)
(use-package request)

(message "ox's emacs initializing...")

;; Allow Ctrl-u to scroll up a page like vim
(setq evil-want-C-u-scroll t)

(setq warning-minimum-level :error)
(setq warning-minimum-log-level :error)

(defun gaiwan-filter-local (files)
  (seq-filter
   (lambda (file)
     (file-exists-p (expand-file-name file user-emacs-directory)))
   files))

(message "[ox] Loading corgi")

(use-package corgi-defaults)

;; can use it without straight
;; (use-package corgi-defaults
;;   :straight nil
;;   :load-path "~/projects/corgi-packages/corgi-defaults")

(use-package corgi-editor
  :load-path "~/projects/corgi-packages/corgi-editor")

(use-package corgi-commands
  :load-path "~/projects/corgi-packages/corgi-commands")
(use-package corgi-clojure
  :load-path "~/projects/corgi-packages/corgi-clojure")
(use-package corgi-emacs-lisp
  :load-path "~/projects/corgi-packages/corgi-emacs-lisp")
(use-package corgi-stateline
  :load-path "~/projects/corgi-packages/corgi-stateline")
(use-package corgi-bindings
  :load-path "~/projects/corgi-packages/corgi-bindings")

(use-package corkey
  :straight (corkey
             :type git
             :host github
             :repo "corgi-emacs/corkey")
  :config
  (corkey-mode 1)
  (corkey/reload))

(message "[ox] Corgi loaded.")

(use-package magit)
(use-package org
  :config
  (require 'org-tempo))
(use-package markdown-mode)
(use-package yaml-mode)
(use-package inf-clojure)
(use-package hcl-mode)
(use-package typescript-mode)
(use-package dockerfile-mode)
(use-package groovy-mode)
(use-package buttercup)
(use-package rainbow-mode)
(use-package pkg-info)
;;(use-package clj-refactor)

(server-start)
(global-display-line-numbers-mode 1)

;; use with ,,<letter>, e.g. `,,g' runs (user/go)
(set-register ?k "#_clj (do (require 'kaocha.repl) (kaocha.repl/run))")
(set-register ?K "#_clj (do (require 'kaocha.repl) (kaocha.repl/run-all))")
(set-register ?r "#_clj (do (require 'user :reload) (user/reset))")
(set-register ?g "#_clj (user/go)")
(set-register ?b "#_clj (user/browse)")
(set-register ?c "#_clj ((requiring-resolve 'nextjournal.clerk/serve!) {})")
(set-register ?, "#_clj (nextjournal.clerk/show! \"{{buffer-file-name}}\")")
(set-register ?p "#_clj (user/portal)")
(set-register ?P "#_cljs (user/portal)")

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;;(require 'corgi-clojure-cider-extras)
;;(require 'corgi-cider-connection-indicator)

(setq cider-connection-message-fn
      nil )

(setq recentf-max-saved-items 100)

(when (executable-find "bb")
  (corgi/cider-jack-in-babashka))
(run-at-time nil (* 5 60) 'recentf-save-list)
(corgi/enable-cider-connection-indicator)

cider-connected-hook

(use-package visual-fill-column)

(with-current-buffer (get-buffer-create "*scratch-clj*")
  (clojure-mode))

(with-current-buffer (get-buffer-create "*scratch*")
  (lisp-interaction-mode))

(use-package html-to-hiccup)

(setq warning-minimum-level :warning)
(setq warning-minimum-log-level :warning)

(put-clojure-indent 'reflect/extend-signatures '(1 :form (1)))
(put-clojure-indent 'sc.api/letsc '(1))

;; (eval-after-load 'projectile
;;   (setq projectile-project-root-files-bottom-up
;;         (cons "deps.edn"
;;               projectile-project-root-files-bottom-up)))

;; Allow Ctrl-u to scroll up a page like vim
(setq evil-want-C-u-scroll t)

(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)
;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)
;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)

(global-auto-revert-mode t)

(defun ox/open-init-el ()
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; You will most likely need to adjust this font size for your system!
(defvar ox/default-font-size 280)
(defvar ox/default-variable-font-size 280)
(set-face-attribute 'default nil :font "Iosevka" :height ox/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height ox/default-font-size)

;; Unfortunately emacs launched from `.app` launcher does not get the full exec path which our shell has. Let's fix that
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (fset 'evil-visual-update-x-selection 'ignore)
  (setq evil-kill-on-visual-paste nil)
  (setq-default evil-symbol-word-search t)
  :config
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (require 'evil-maps)
  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Use another key to go into normal / escape mode. I have it configured as `qp`
(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "qp")
  (evil-escape-mode))

(use-package evil-cleverparens
  :after (evil smartparens)
  :commands evil-cleverparens-mode
  :init
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (setq evil-cleverparens-complete-parens-in-yanked-region t)
  :config
  (setq evil-cleverparens-use-s-and-S nil)
  (evil-define-key '(normal visual) evil-cleverparens-mode-map
    "s" nil
    "S" nil
    "{" nil
    "}" nil
    "[" nil
    "]" nil
    (kbd "M-[") nil
    (kbd "<tab>") 'evil-jump-item))

(use-package projectile
  :init
  (setq projectile-create-missing-test-files t)
  (setq projectile-project-search-path '("~/projects/" "~/playground/" "~/projects/lambdaisland"))
  (defun ox/refresh-projects-dir ()
    (interactive)
    ;; (projectile-discover-projects-in-directory "~/projects")
    (projectile-discover-projects-in-search-path))
  :config
  (projectile-global-mode))

;; command-log-mode is useful for displaying a panel showing each key binding
;; you use in a panel on the right side of the frame. Great for live streams and
;; screencasts!
(use-package command-log-mode)

;; (use-package forge
;;   :after magit
;;   :config
;;   (transient-append-suffix 'forge-dispatch '(0)
;;     ["Forge browse"
;;      ("@" "browse" forge-browse)])
;;   (transient-append-suffix 'forge-dispatch '(0)
;;     ["PR"
;;      ("p c" "pullreq checkout" forge-checkout-pullreq)]
;;     )
;;   (transient-append-suffix 'forge-dispatch '(0)
;;     ["Edit"
;;      ("e p" "post" forge-edit-post)
;;      ("e a" "assignees" forge-edit-topic-assignees)
;;      ("e r" "review requests" forge-edit-topic-review-requests)]))

(use-package git-link
  :config
  (setq git-link-open-in-browser t
        git-link-use-commit t))

(use-package emojify)
(use-package gitmoji
  :straight nil
  :load-path "~/projects/emacs-gitmoji")

(use-package default-text-scale)

(use-package html-to-hiccup
  :straight nil
  :load-path "~/projects/html-to-hiccup")

;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(message "loading secrets...")
(load-file (expand-file-name (concat user-emacs-directory "/secrets.el")))
(message "secrets loaded")

(defun send-discord-message-with-webhook (webhook-url message)
  "Send a message to a Discord channel using a webhook URL."
  (request webhook-url
    :type "POST"
    :data (json-encode `(("content" . ,message)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :sync t
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message "Done: %s" (request-response-status-code response))))))

(defun ox-journal-discord-gaiwan ()
  "Interactively send a message to a Discord channel using a webhook URL."
  (interactive)
  (let* ((message (read-string "Enter message: ")))
    (send-discord-message-with-webhook discord-ox-journal-webhook-url message)))

;; (use-package clockify
;;   :ensure nil
;;   :load-path "~/projects/emacs-clockify")

;; Usage example: M-x send-discord-message

(setq create-lockfiles nil)

(defun css-region-to-garden (start end)
  (interactive "r")
  (replace-regexp "\\([a-z-]+\\): \\(.*\\);" ":\\1 \"\\2\"" nil start end))

(defun projectile-kill-all-repl-buffers ()
  "Kill all repls in the current project"
  (interactive)
  (dolist (buffer (buffer-list))
    (when (or (string-match-p (concat "cider-repl projects/" (projectile-project-name) ":localhost") (buffer-name buffer)))
      (kill-buffer buffer))))

;; (use-package piglet-emacs)
(use-package adoc-mode)

(use-package just-mode)
(use-package web-mode)
(use-package dotenv-mode)

(use-package git-timemachine)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init 'git-timemachine))

(use-package coverlay)
(use-package origami)
(use-package css-in-js-mode
  :straight '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))

;; (use-package corfu)

(use-package tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

;; (evil-define-key '(normal visual insert operator)
;;   global-map
;;   (kbd "C-v") 'evil-paste-after
;;   (kbd "C-V") 'evil-paste-before)

;; (define-key input-decode-map (kbd "M-v") 'my-paste-from-clipboard)
;; (defun my-paste-from-clipboard ()
;;   "Paste from clipboard in Evil normal and insert modes."
;;   (interactive)
;;   (if (not (evil-insert-state-p))
;;       (progn
;;         (evil-normal-state)
;;         (evil-paste-after))
;;     (yank)))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; ;; Define a function to paste text
;; (defun my-paste ()
;;   "Paste text."
;;   (interactive)
;;   (yank))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; ;; (define-key input-decode-map (kbd "M-v") 'my-paste-from-clipboard)
;; ;; (defun my-paste-from-clipboard ()
;; ;;   "Paste from clipboard in Evil normal and insert modes."
;; ;;   (interactive)
;; ;;   (if (not (evil-insert-state-p))
;; ;;       (progn
;; ;;         (evil-normal-state)
;; ;;         (evil-paste-after))
;; ;;     (yank)))

;; (use-package lsp-mode
;;   :ensure t
;;   ;; :hook ( (clojure-mode . lsp)
;;   ;;         (clojurec-mode . lsp)
;;   ;;         (clojurescript-mode . lsp))
;;   :config
;;   ;; add paths to your local installation of project mgmt tools, like lein
;;   (setenv "PATH" (concat
;;                   "/usr/local/bin" path-separator
;;                   (getenv "PATH")))
;;   (setq lsp-enable-indentation nil)
;;   (dolist (m '(clojure-mode
;;                clojurec-mode
;;                clojurescript-mode
;;                clojurex-mode))
;;     (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   (setq lsp-ui-doc-show-with-cursor t)
;;   )

;; ;; (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))                                                                                    (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\033[0 q")))

;; ;; `evil-terminal-cursor-changer' utilizes custom terminal escape sequences
;; ;; (which work in most, but not all, terminals) to adjust the appearance of the
;; ;; Emacs cursor based on which Vim mode is currently active. Note that this
;; ;; package is only required when running in a terminal (hence the `unless').
;; (use-package evil-terminal-cursor-changer
;;   :config
;;   (unless (display-graphic-p)
;;     (require 'evil-terminal-cursor-changer)
;;     (evil-terminal-cursor-changer-activate)
;;     (setq evil-insert-state-cursor 'bar)
;;     ))

(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)

;; ;; (use-package company
;; ;;   :disabled t)

(use-package cider
  :ensure t
  :init
  (setq cider-clojure-cli-global-options ""))

;; (defun ox/counsel-rg-change-dir (arg)
;;   (let ((current-prefix-arg '(4)))
;;     (counsel-rg ivy-text nil "")))

;; (with-eval-after-load 'counsel
;;   (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;; (with-eval-after-load 'ivy
;;   (with-eval-after-load 'counsel
;;     (ivy-add-actions
;;      'counsel-rg
;;      '(("r" ox/counsel-rg-change-dir "change root directory")))))

(use-package avy
  :straight t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-ignored-modes
        '(image-mode magit-mode doc-view-mode pdf-view-mode exwm-mode))
  (evil-define-key nil evil-normal-state-map
    "s" 'avy-goto-char-2))

;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
;;   :ensure t
;;   :config
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
;; ;; you can utilize :map :hook and :config to customize copilot

(message "[ox] loading tailwind cheatsheet...")
(load-file (expand-file-name (concat user-emacs-directory "/tailwind_cheatsheet.el")))
(message "[ox] tailwind cheatsheet loaded")

(message "[ox] init.el finished loading.")

(provide 'init)
;;; local.ox.el ends here
