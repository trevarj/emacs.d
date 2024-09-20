;;; early-init.el --- Trev's Early Init Config -*- lexical-binding: t -*-

;; Startup hacks
(setq gc-cons-threshold (* 1024 1024 100))
;; (setq file-name-handler-alist-original file-name-handler-alist)
;; (setq file-name-handler-alist nil)

(setq package-enable-at-startup nil)        ; Disable emacs package manager
(scroll-bar-mode -1)                        ; Disable visible scrollbar
(tool-bar-mode -1)                          ; Disable the toolbar
(menu-bar-mode -1)                          ; Disable the menubar
(tooltip-mode -1)                           ; Disable tooltips
(modify-all-frames-parameters
 '((internal-border-width . 40)))           ; Frame padding
(setq-default left-fringe-width 4
              left-margin-width 1
              right-fringe-width 4
              right-margin-width 1)


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(provide 'early-init)

;;; early-init.el ends here
