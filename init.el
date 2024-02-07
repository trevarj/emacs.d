;;; init.el --- Trev's Emacs Config -*- lexical-binding: t -*-

;; Startup 

;; straight.el package manager
(defvar bootstrap-version)
(setq straight-use-package-by-default t)
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

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'display-startup-time)

;; Global modes
(save-place-mode)
(recentf-mode)
;; Session saving
(desktop-save-mode)

;; Fonts
(set-face-attribute 'default nil :font "Iosevka JBM" :height 150 :weight 'medium)
(set-face-attribute 'mode-line nil :font "Iosevka JBM" :height 130 :weight 'bold)
(set-face-attribute 'fixed-pitch nil :font "Iosevka JBM" :height 150)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 150 :weight 'regular)
(setq use-default-font-for-symbols nil)
;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
(defvar nerdfont-code-points
  '((#xE6FA . #xE6B2)
    (#xE700 . #xE7C5)
    (#xF000 . #xF2E0)
    (#xE200 . #xE2A9)
    (#xF500 . #xFD46)
    (#xE300 . #xE3EB)
    (#xF400 . #xF532)
    #x2665
    #x26A1
    #xF2A1
    #xF27C
    #xE0A3
    #xE0CA
    (#xE0B4 . #xE0C8)
    (#xF0001 . #xF1AF0)
    (#xE0CC . #xE0D4)
    (#x23FB . #x23FE)
    (#xF300 . #xF372)
    (#xE000 . #xE00A)
    (#xEA60 . #xEBEB)))
(dolist (code-point nerdfont-code-points)
  (set-fontset-font t code-point (font-spec :family "Symbols Nerd Font Mono")))

;; Ligatures
(use-package ligature
  :straight t
  :config
  (ligature-set-ligatures 'prog-mode
                          '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                            "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                            "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                            "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                            "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                            "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                            ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                            "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                            "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                            "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                            "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
  (global-ligature-mode))

;; Colorscheme
(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "doomemacs/themes")
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; UI elements
(column-number-mode)                        ; Line number mode
(global-display-line-numbers-mode t)        ; Globally display line numbers
(global-hl-line-mode t)
(setq confirm-kill-emacs #'y-or-n-p)
(setq inhibit-startup-message t             ; no startup screen
      display-line-numbers-type 'relative   ; relative line numbers
      scroll-step 1                         ; vim style scrolling
      scroll-margin 8) ; vim style scroll off
(setq-default indent-tabs-mode nil) ; use spaces only

;; Diminish minor modes
(use-package diminish
  :straight t
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

;; Keybinding help
(use-package which-key
  :straight t
  :diminish which-key-mode
  :config
  (setq which-key-popup-type 'minibuffer)
  :init
  (which-key-mode))

(use-package undo-fu
  :straight t)

;; Minibuffer 
(use-package vertico ; completion
  :straight t
  :init
  (vertico-mode)) 
(use-package marginalia ; completion definitions
  :straight t
  :init
  (marginalia-mode)) 
(use-package consult ; useful commands
  :straight t
  ;; https://github.com/minad/consult?tab=readme-ov-file#use-package-example
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c r" . consult-recent-file)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)) 
  :init
  ;; TODO: https://github.com/minad/consult?tab=readme-ov-file#custom-variables
  :config)

;; Completion
(use-package corfu
  :straight t
  :init (global-corfu-mode)
  :config
  (corfu-popupinfo-mode)
  (add-to-list 'corfu--frame-parameters '(internal-border-width . 2))
  (setq corfu-auto t
        corfu-popupinfo-delay 0
        corfu-quit-no-match 'separator))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu-terminal
  :straight
  (corfu-terminal
   :type git
   :host codeberg
   :repo "akib/emacs-corfu-terminal")
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode)))

(use-package corfu-doc-terminal
  :straight
  (corfu-doc-terminal
   :type git
   :host codeberg
   :repo "akib/emacs-corfu-doc-terminal")
  :after corfu-terminal
  :init
  (unless (display-graphic-p)
    (corfu-doc-terminal-mode)))

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico is not active."
  (unless (or (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

;; Treesitter
;; (use-package treesit-auto
;;   :straight t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; (add-to-list 'major-mode-remap-alist
;;              '(rust-mode . rust-ts-mode)
;;              '(c-mode . c-ts-mode))

;; Editing
(setq backup-directory-alist `(("." . "~/.cache/emacs/backups")))
(electric-pair-mode)

;; Formatting
(use-package apheleia
  :straight t
  :diminish apheleia-mode
  :init
  (apheleia-global-mode))

;; Languages & LSPs
(setq eldoc-echo-area-use-multiline-p nil)

;; Language mode hooks
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)

;; Git
(setq epg-pinentry-mode 'loopback) ; pinentry on minibuffer
(use-package magit
  :straight t
  :config
  (setq magit-define-global-key-bindings 'recommended))
(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
