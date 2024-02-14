;;; early-init.el --- Trev's Early Init Config -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)        ; Disable emacs package manager
(scroll-bar-mode -1)                        ; Disable visible scrollbar
(tool-bar-mode -1)                          ; Disable the toolbar
(menu-bar-mode -1)                          ; Disable the menubar
(tooltip-mode -1)                           ; Disable tooltips
(modify-all-frames-parameters
 '((internal-border-width . 40)))           ; Frame padding
