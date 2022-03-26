(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#ff6c60" "#A8FF60" "#FFFFB6" "#96CBFE" "#FF73FD" "#C6C5FE" "#f6f3e8"])
 '(custom-safe-themes
   '("d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" default))
 '(exwm-floating-border-color "#5B6268")
 '(fci-rule-color "#5B6268")
 '(highlight-tail-colors
   ((("#101909" "#A8FF60" "green")
     . 0)
    (("#131319" "#C6C5FE" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#96CBFE"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A8FF60"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(objed-cursor-color "#ff6c60")
 '(pdf-view-midnight-colors (cons "#f6f3e8" "#000000"))
 '(rustic-ansi-faces
   ["#000000" "#ff6c60" "#A8FF60" "#FFFFB6" "#96CBFE" "#FF73FD" "#C6C5FE" "#f6f3e8"])
 '(safe-local-variable-values
   '((cider-save-file-on-load)
     (cider-auto-track-ns-form-changes)
     (cider-clojure-cli-global-options . "-A:dev")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend")
     (clojure-toplevel-inside-comment-form . t)
     (cider-redirect-server-output-to-repl . t)
     (cider-preferred-build-tool . clojure-cli)
     (cider-default-cljs-repl . custom)
     (cider-custom-cljs-repl-init-form . "(user/cljs-repl)")
     (cider-clojure-cli-global-options . "-A:dev:test")
     (eval define-clojure-indent
           (assoc 0)
           (ex-info 0))
     (eval progn
           (make-variable-buffer-local 'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-repl-display-help-banner)
     (eval define-clojure-indent
           (reg-cofx :defn)
           (reg-event-db :defn)
           (reg-event-fx :defn)
           (reg-fx :defn)
           (reg-sub :defn)
           (reg-event-domain :defn)
           (reg-block-event-fx :defn)
           (reg-event-domain-fx :defn)
           (this-as 0))))
 '(vc-annotate-background "#000000")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A8FF60")
    (cons 40 "#c5ff7c")
    (cons 60 "#e2ff99")
    (cons 80 "#FFFFB6")
    (cons 100 "#f7ea9a")
    (cons 120 "#f0d57e")
    (cons 140 "#E9C062")
    (cons 160 "#f0a695")
    (cons 180 "#f78cc9")
    (cons 200 "#FF73FD")
    (cons 220 "#ff70c8")
    (cons 240 "#ff6e94")
    (cons 260 "#ff6c60")
    (cons 280 "#cf615a")
    (cons 300 "#9f5855")
    (cons 320 "#6f4e4f")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
