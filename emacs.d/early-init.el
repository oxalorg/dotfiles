(message "Early init loading...")

(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 33554432 ; 32mb
                  gc-cons-percentage 0.1)
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(when (not (file-exists-p (expand-file-name "straight/versions/default.el" straight-base-dir)))
  (straight-freeze-versions))

;; (use-package corgi-packages
;;   :straight (corgi-packages
;;              :type git
;;              :local "~/projects/corgi-packages"
;;              ;; :host github
;;              ;; :repo "corgi-emacs/corgi-packages"
;;              ))

;; (add-to-list #'straight-recipe-repositories 'corgi-packages)

(message "Early init finished loading...")

(provide 'early-init.el)
