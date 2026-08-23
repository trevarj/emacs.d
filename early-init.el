;;; early-init.el --- Trev's Early Init Config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Startup

;; Defer garbage collection during startup, then restore a high enough
;; threshold to avoid visible pauses during sustained input in a loaded session.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook
 'after-init-hook
 (lambda ()
   (setq gc-cons-threshold (* 128 1024 1024)
         gc-cons-percentage 0.2)))

;; Keep startup deterministic and avoid scanning site files.
(setq inhibit-default-init t
      site-run-file nil
      user-lisp-directory (expand-file-name "lisp" user-emacs-directory))

;; Restrict VC work during early startup.
(setq vc-handled-backends '(Git))

;;; UI Defaults

;; Disable chrome before the initial frame is created.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; Avoid expensive implied frame resizes when fonts and toolkit widgets settle.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t)

;; Match the frame layout that `spacious-padding' will later use for faces and
;; hooks, but apply frame parameters before the initial frame is created.
(defconst trev/spacious-padding-widths
  '( :internal-border-width 40
     :header-line-width 6
     :mode-line-width 6
     :custom-button-width 6
     :tab-width 6
     :right-divider-width 32
     :scroll-bar-width 10
     :fringe-width 12)
  "Spacing values shared with `spacious-padding'.")

;; Later frames are handled by `spacious-padding-mode' itself.
(dolist (parameter
         `((internal-border-width
            . ,(plist-get trev/spacious-padding-widths :internal-border-width))
           (right-divider-width
            . ,(plist-get trev/spacious-padding-widths :right-divider-width))
           (left-fringe . ,(plist-get trev/spacious-padding-widths :fringe-width))
           (right-fringe . ,(plist-get trev/spacious-padding-widths :fringe-width))
           (scroll-bar-width
            . ,(plist-get trev/spacious-padding-widths :scroll-bar-width))))
  (add-to-list 'initial-frame-alist parameter)
  (add-to-list 'default-frame-alist parameter))

;;; Responsiveness

;; Bias redisplay and fontification toward accepting new input quickly.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default cursor-in-non-selected-windows nil)

(setq bidi-inhibit-bpa t
      redisplay-skip-fontification-on-input t
      read-process-output-max (* 4 1024 1024)
      highlight-nonselected-windows nil)

(provide 'early-init)

;;; early-init.el ends here
