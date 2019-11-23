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

;; All packages which are installed
;; evil
;; helm
;; evil-escape
;; use-package
;; dashboard
;; evil-leader
;; projectile
;; helm-projectile

;; Evil Mode
(setq evil-want-C-u-scroll t)
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
  "f" 'helm-find-files
  "s" 'save-buffer
  "b" 'helm-mini
  "d" 'kill-this-buffer
  "k" 'kill-buffer-and-window
  "p" 'projectile-command-map)

;; Helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-b") #'helm-buffers-list)

(helm-mode 1)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; ORG Mode
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (projectile evil-leader dashboard use-package helm evil-visual-mark-mode evil-escape))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
