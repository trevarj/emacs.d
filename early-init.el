;;; early-init.el --- Trev's Early Init Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq
 ;; Startup hacks
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 vc-handled-backends '(Git)
 ;; Disable site files
 inhibit-default-init t
 site-run-file nil
 ;; Don't forget to run `package-quickstart-refresh'!
 package-quickstart t)

;; After Emacs has completely started, reset the values to more sensible ones.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
         gc-cons-percentage 0.1)))

(scroll-bar-mode -1)                        ; Disable visible scrollbar
(tool-bar-mode -1)                          ; Disable the toolbar
(menu-bar-mode -1)                          ; Disable the menubar
(tooltip-mode -1)                           ; Disable tooltips
(modify-all-frames-parameters
 '((internal-border-width . 40)))           ; Frame padding
(setq-default
 left-fringe-width 4
 left-margin-width 1
 right-fringe-width 4
 right-margin-width 1)


;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t)

(provide 'early-init)

;;; early-init.el ends here
