(setq package-enable-at-startup nil)
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
 (setq elpaca-use-package-by-default t))

;; (setq use-package-always-ensure t)
(elpaca-wait)

;; (use-package corgi-packages
;;              :ensure (corgi-packages
;;                        :host github
;;                        :repo "corgi-emacs/corgi-packages"
;;                        :branch "ox/separate-completion-ui"))

(use-package transient)
(use-package json :ensure nil)
(use-package request)

(message "ox's emacs initializing...")

;; Allow Ctrl-u to scroll up a page like vim
(setq evil-want-C-u-scroll t)

;; (setq warning-minimum-level :error)
;; (setq warning-minimum-log-level :error)

(message "[ox] Loading corgi")

;; Loading corgi deps
(use-package clj-ns-name
  :ensure (clj-ns-name
	   :type git
	   :host github
	   :files ("clj-ns-name.el")
	   :repo "corgi-emacs/clj-ns-name"))

(use-package walkclj
  :ensure
  (walkclj
   :type git
   :host github
   :files ("walkclj.el")
   :repo "corgi-emacs/walkclj"))

(use-package pprint-to-buffer
  :ensure
  (pprint-to-buffer
   :type git
   :host github
   :files ("pprint-to-buffer/pprint-to-buffer.el")
   :repo "plexus/plexmacs"))

(defmacro corgi-use-package (package)
  `(use-package ,package
     :ensure (,package
              :host github
              :repo "corgi-emacs/corgi-packages"
              :local-repo ,(symbol-name package)
              :files (,(concat (symbol-name package) "/" (symbol-name package) ".el"))
              :branch "ox/separate-completion-ui")))

(corgi-use-package corgi-editor)
(corgi-use-package corgi-commands)
(corgi-use-package corgi-clojure)
(corgi-use-package corgi-emacs-lisp)
(corgi-use-package corgi-stateline)
(use-package corgi-bindings
  :ensure
  (corgi-bindings
   :type git
   :host github
   :branch "main"
   :files ("corgi-bindings/corgi-bindings.el"
           "corgi-bindings/corgi-keys.el"
           "corgi-bindings/corgi-signals.el"
           "corgi-bindings/user-keys-template.el"
           "corgi-bindings/user-signals-template.el")
   :repo "corgi-emacs/corgi-packages"))

(use-package corkey
  :ensure (corkey
           :type git
           :host github
           :repo "corgi-emacs/corkey")
  :config
  (corkey-mode 1)
  (corkey/reload))

(message "[ox] Corgi loaded.")

(setq backup-directory-alist
      `(("." . "~/.emacs-saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))

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
(use-package clj-refactor)

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
(set-register ?z "#_clj (do (user/pathom-reload-env) nil)")

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

(use-package cherry-blossom-theme
  :ensure t
  :config
  (load-theme 'cherry-blossom t))

(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (delete-trailing-whitespace))))

;;(require 'corgi-clojure-cider-extras)
;;(require 'corgi-cider-connection-indicator)

(setq cider-connection-message-fn
      nil )

(setq recentf-max-saved-items 100)

;; (when (executable-find "bb")
;;   (corgi/cider-jack-in-babashka))
;; (run-at-time nil (* 5 60) 'recentf-save-list)
;; (corgi/enable-cider-connection-indicator)

;; cider-connected-hook

(use-package visual-fill-column)

(use-package flycheck-clj-kondo
  :ensure t)

(with-eval-after-load 'clojure-mode
  (with-current-buffer (get-buffer-create "*scratch-clj*")
    (clojure-mode))

  (with-current-buffer (get-buffer-create "*scratch*")
    (lisp-interaction-mode))

  (require 'flycheck-clj-kondo)

  (put-clojure-indent 'lambdaisland.morf/deform 1)
  (put-clojure-indent 'reflect/extend-signatures '(1 :form (1)))
  (put-clojure-indent 'sc.api/letsc '(1)))

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
(defvar ox/default-font-size 160)
(defvar ox/default-variable-font-size 160)
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
;; (use-package gitmoji
;;   :ensure nil
;;   :load-path "~/projects/emacs-gitmoji")

(use-package default-text-scale
  :config
  (setq default-text-scale-amount 20))

(use-package html-to-hiccup
  :ensure (:host github :repo "plexus/html-to-hiccup"))

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

(defun send-slack-message-with-webhook (webhook-url message)
  "Send a message to a Slack channel using a webhook URL."
  (request webhook-url
    :type "POST"
    :data (json-encode `(("message" . ,message)))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :sync t
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message "Done: %s" (request-response-status-code response))))))

(defun ox/journal-discord-gaiwan ()
  "Interactively send a message to a Discord channel using a webhook URL."
  (interactive)
  (let* ((message (read-string "Enter message: ")))
    (send-discord-message-with-webhook discord-ox-journal-webhook-url message)
    ))

(defun ox/journal-slack-gaiwan ()
  "Interactively send a message to a Discord channel using a webhook URL."
  (interactive)
  (let* ((message (read-string "Enter message: ")))
    (send-slack-message-with-webhook discord-slack-journal-webhook-url message)))

;; (use-package clockify
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
  :ensure '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-popupinfo-delay '(0.2 . 0.2))

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  (corfu-echo-mode)
  (corfu-popupinfo-mode)
  )

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package tsx-mode
  :ensure '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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
  (setq cider-dynamic-indentation nil
	cider-font-lock-dynamically nil
	cider-font-lock-reader-conditionals nil)
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
  :ensure t
  :config
  (setq avy-timeout-seconds 0.5)
  (setq avy-ignored-modes
        '(image-mode magit-mode doc-view-mode pdf-view-mode exwm-mode))
  (evil-define-key nil evil-normal-state-map
    "s" 'avy-goto-char-2))

(use-package devdocs)

(with-eval-after-load 'require
  (add-to-list 'load-path (expand-file-name "~/projects/clojuredocs.el"))
  (require 'clojuredocs))

(use-package difftastic
  :demand t
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))


(use-package harpoon)

;; (use-package magit-delta)

;; (defun aankh/toggle-magit-delta ()
;;   (interactive)
;;   (magit-delta-mode
;;    (if magit-delta-mode
;;        -1
;;      1))
;;   (magit-refresh))

;; ;; For some reason, this was being called twice without the guard.
;; (with-eval-after-load 'magit-diff
;;   (unless (boundp 'aankh/added-magit-diff-suffixes)
;;     (transient-append-suffix 'magit-diff '(-1 -1)
;;       [("l" "Toggle magit-delta" aankh/toggle-magit-delta)
;;        ;; ("D" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
;;        ;; ("S" "Difftastic Show" th/magit-show-with-difftastic)
;;        ]))
;;   (setf aankh/added-magit-diff-suffixes t))


(message "[ox] loading tailwind cheatsheet...")
(load-file (expand-file-name (concat user-emacs-directory "/tailwind_cheatsheet.el")))
(message "[ox] tailwind cheatsheet loaded")

(message "[ox] init.el finished loading.")

(provide 'init)
;;; local.ox.el ends here
