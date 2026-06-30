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

(defun trev/spacious-padding-frame-parameters ()
  "Return frame parameters derived from `trev/spacious-padding-widths'."
  `((internal-border-width
     . ,(or (plist-get trev/spacious-padding-widths :internal-border-width) 15))
    (right-divider-width
     . ,(or (plist-get trev/spacious-padding-widths :right-divider-width) 30))
    (left-fringe
     . ,(or (plist-get trev/spacious-padding-widths :left-fringe-width)
            (plist-get trev/spacious-padding-widths :fringe-width)
            8))
    (right-fringe
     . ,(or (plist-get trev/spacious-padding-widths :right-fringe-width)
            (plist-get trev/spacious-padding-widths :fringe-width)
            8))
    (scroll-bar-width
     . ,(or (plist-get trev/spacious-padding-widths :scroll-bar-width) 8))))

(dolist (parameter (trev/spacious-padding-frame-parameters))
  (add-to-list 'initial-frame-alist parameter)
  (add-to-list 'default-frame-alist parameter))

(defun trev/spacious-padding-apply-frame-parameters (frame)
  "Apply spacious frame parameters to FRAME."
  (modify-frame-parameters frame (trev/spacious-padding-frame-parameters)))

(add-hook 'after-make-frame-functions
          #'trev/spacious-padding-apply-frame-parameters)

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
