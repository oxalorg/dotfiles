(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Looks and Feel
(set-face-attribute 'default nil :height 180)
(load-theme 'deeper-blue t)
(tool-bar-mode -1)
(toggle-frame-maximized)

;; Basic editior stuff
(setq-default indent-tabs-mode nil)

;; All packages which are installed
;; evil
;; helm
;; evil-escape
;; use-package
;; dashboard
;; evil-leader
;; projectile
;; helm-projectile
;; magit
;; elpy
;; flycheck
;; blacken
;; treemacs
;; treemacs-projectile

;; Evil Mode
(setq evil-want-C-u-scroll t)
(setq-default evil-escape-delay 0.7)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(require 'evil)
(evil-mode t)
(evil-escape-mode t)
(setq-default evil-escape-key-sequence "qp")
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
  (define-key evil-motion-state-map (kbd ";") 'evil-ex))

(evil-leader/set-key
  "f" 'helm-projectile-find-file
  "s" 'save-buffer
  "b" 'helm-mini
  "d" 'kill-this-buffer
  "k" 'kill-buffer-and-window
  "p" 'projectile-command-map
  "g" 'magit-status
  "m" 'list-bookmarks
  "t" 'treemacs)

;; Helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

(helm-mode 1)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-items '((recents  . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (agenda . 5)
			  (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; (use-package treemacs-evil
;;   :after treemacs evil
;;   :ensure t)

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;; ORG Mode
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Elpy & Python
(use-package elpy
  :ensure t
  :config
  (if (executable-find "python3")
      (progn
        (setq elpy-rpc-python-command "python3")
        (setq python-shell-interpreter "python3")))

  (setq elpy-rpc-python-command "/usr/local/bin/python3")
  ;; (use-package pyvenv
  ;;   :ensure t
  ;;   :config
  ;;   (pyvenv-workon "dotfiles"))

  ;; (use-package jedi
  ;;   :ensure t)

  ;; Automatically run Black on buffer save
  (add-hook 'elpy-mode-hook
            '(lambda ()
               (when (eq major-mode 'python-mode)
                 (add-hook 'before-save-hook 'elpy-black-fix-code))))

  ;; Use flycheck instead of flymake
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (elpy-enable)

  ;;(setq elpy-rpc-backend "jedi")
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (treemacs-projectile treemacs ## zones blacken flycheck elpy magit projectile evil-leader dashboard use-package helm evil-visual-mark-mode evil-escape))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
