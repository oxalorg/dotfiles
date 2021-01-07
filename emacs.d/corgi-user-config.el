(set-face-attribute 'default nil :font "Fira Code" :height 135)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
  (define-key evil-motion-state-map (kbd ";") 'evil-ex))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  ;;(doom-themes-org-config)
  )

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; TODO: SPC W W keybindings

;; Ranger
(use-package ranger)
;; Expand region

;; Git gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:update-interval 2)))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package super-save
  :ensure t
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setq display-time-world-list
      '(("America/Chicago" "Wisconsin")
        ("America/Recife" "Recife")
        ("Europe/Berlin" "Berlin")
        ("Asia/Jerusalem" "Israel")
        ("Asia/Kolkata" "Mumbai")))
(setq display-time-world-time-format "%a, %l:%M %p")

;; (use-package ivy-posframe
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (ivy-posframe-mode 1))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
