;;; init.el --- Trev's Emacs Config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; A minimalistic development environment

(use-package emacs
  :demand t
  :preface
  (defun display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

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

  (defun header-line-file-path ()
    "Set `header-line-format' if symbol `buffer-file-name' is not nil."
    (when buffer-file-name
      (setq header-line-format '("%f"))))

  (defun sudo-save ()
    (interactive)
    (if (not buffer-file-name)
        (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
      (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

  :hook
  ((emacs-startup . display-startup-time)
   (buffer-list-update . header-line-file-path))
  :init
  (save-place-mode)
  (recentf-mode)
  (desktop-save-mode)                           ; Session saving
  (column-number-mode)                          ; Column number mode
  (global-display-line-numbers-mode)            ; Globally display line numbers
  (global-auto-revert-mode)                     ; Auto-refresh buffers
  (auto-fill-mode)                              ; Autofill mode
  ;; Fonts
  (set-face-attribute 'default nil :family "Iosevka JBM" :height 150 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka JBM" :height 150)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans" :height 120 :weight 'regular)
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
    (set-fontset-font t code-point (font-spec :family "Symbols Nerd Font
  Mono")))

  ;; Miscellaneous Options
  (setq-default fill-column 80                   ; 80 width pages
                auto-fill-function 'do-auto-fill ; always autofill
                indent-tabs-mode nil             ; Use spaces only
                major-mode                       ; Guess major mode from file name
                (lambda ()
                  (unless buffer-file-name
                    (let ((buffer-file-name (buffer-name)))
                      (set-auto-mode)))))
  (setq
   use-package-always-defer t              ; always defer packages, use :demand instead
   custom-file "/tmp/custom.el"            ; customization file
   desktop-load-locked-desktop 'check-pid  ; load if lock pid doesn't exist
   display-line-numbers-grow-only t        ; Never shrink the linum width
   display-line-numbers-width-start t      ; Calculate linum width at start
   confirm-kill-emacs nil
   use-dialog-box nil                      ; Bye
   global-auto-revert-non-file-buffers t   ; Auto-refresh buffers like dired
   auto-revert-verbose nil                 ; But silence it
   enable-recursive-minibuffers t          ; Recursive mini-buffers
   inhibit-startup-message t               ; No startup screen
   scroll-step 1                           ; Vim style scrolling
   scroll-margin 10                        ; Vim style scroll off
   fill-column 80                          ; Line width 80 chars
   comment-auto-fill-only-comments t       ; Autofill comments only
   backup-directory-alist '(("." . "~/.cache/emacs/backups"))
   auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t))
   undo-limit 67108864                     ; Undo limit of 64mb.
   undo-strong-limit 100663296             ;               96mb.
   undo-outer-limit 1006632960             ;               960mb.
   eldoc-echo-area-use-multiline-p nil
   epg-pinentry-mode 'loopback             ; pinentry on minibuffer
   mouse-wheel-progressive-speed nil
   mouse-wheel-scroll-amount
   '(3
     ((shift) . 1)
     ((control) . nil))                    ; Mouse wheel scrolling
   scroll-preserve-screen-position 1       ; PgUp/PgDown hold
   gnus-init-file (expand-file-name
                   "gnus/gnus.el"
                   user-emacs-directory)
   send-mail-function
   'message-send-mail-with-sendmail)       ; Use sendmail
  (defalias 'yes-or-no #'y-or-n-p)         ; Easier question

  ;; Mode maps
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

  ;; Generic keybindings
  :bind
  (("C-c b" . ibuffer)
   ("C-x C-z" . nil) ; disable suspend-frame
   ("C-c c" . (lambda () (interactive) (find-file user-init-file)))))

;; Ligatures
(use-package ligature
  :demand
  :config
  (ligature-set-ligatures
   'prog-mode
   '(
     "-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
     "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
     "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__"
     "<~~" "</" "</>" "/>" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!="
     "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
     "(*" "*)" "/*" "*/" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!---"))
  (global-ligature-mode))

;; Colorscheme
(use-package doom-themes
  :straight (doom-themes :type git :host github :repo "doomemacs/themes")
  :demand
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil
        doom-nord-padded-modeline t
        doom-nord-brighter-modeline t)
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (custom-set-faces
   `(cursor ((t (:background "#d08770"))))
   `(font-lock-function-name-face ((t (:foreground "#d08770" :weight extra-bold))))
   '(erc-current-nick-face ((t (:foreground "#bf616a" :weight bold))))
   `(erc-nick-default-face ((t (:foreground "#ebcb8b" :weight bold))))
   `(erc-pal-face ((t (:foreground "#d08770" :weight bold))))
   `(corfu-border ((t (:background "#d08770"))))))

;; Diminish minor modes
(use-package diminish
  :demand)

(use-package autorevert
  :demand
  :diminish auto-revert-mode) ; doesn't work in :config above

;; Keybinding
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-description-order)
  :init
  (which-key-mode))

;; Window navigation
(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package undo-fu
  :init
  (global-unset-key (kbd "C-z"))
  :bind
  (("C-z" . 'undo-fu-only-undo)
   ("C-S-z" . 'undo-fu-only-redo)))

;; Minibuffer
(use-package vertico ; completion
  :init
  (vertico-mode))

(use-package marginalia ; completion definitions
  :init
  (marginalia-mode))
(use-package consult ; useful commands
  ;; https://github.com/minad/consult?tab=readme-ov-file#use-package-example
  :bind (("C-M-#" . consult-register)
         ("C-c /" . consult-ripgrep)
         ("C-c <SPC>" . consult-fd)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c i" . consult-info)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c r" . consult-recent-file)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("M-g I" . consult-imenu-multi)
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g i" . consult-imenu)
         ("M-g k" . consult-global-mark)
         ("M-g m" . consult-mark)
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-s G" . consult-git-grep)
         ("M-s L" . consult-line-multi)
         ("M-s c" . consult-locate)
         ("M-s d" . consult-fd)                    ;; Alternative: consult-find
         ("M-s e" . consult-isearch-history)
         ("M-s g" . consult-grep)
         ("M-s k" . consult-keep-lines)
         ("C-s" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-s u" . consult-focus-lines)
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ([remap Info-search] . consult-info)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-r" . consult-history)
         ("M-s" . consult-history))                 ;; orig. next-matching-history-element
  ;; https://github.com/minad/consult?tab=readme-ov-file#custom-variables
  :config
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t))

;; Completion
(use-package corfu
  :preface
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active."
    (unless (or (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode)))
  :init
  (global-corfu-mode)
  :hook
  ('minibuffer-setup . #'corfu-enable-always-in-minibuffer)
  :config
  (corfu-popupinfo-mode)
  (add-to-list 'corfu--frame-parameters '(internal-border-width . 2))
  (setq corfu-auto t
        corfu-popupinfo-delay 0
        corfu-quit-no-match 'separator
        corfu-preview-current nil
        tab-always-indent 'complete))

(use-package orderless
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

;; Completion-at-point helper
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Treesitter
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Whitespace handling
(use-package ws-butler
  :diminish
  :config
  (ws-butler-global-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Editorconfig
(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode))

;; Buffer specific direnv
(use-package envrc
  :diminish
  :config (envrc-global-mode))

;; Avy navigation
(use-package avy
  :bind
  (("C-c w" . #'avy-goto-word-1)
   ("C-c s" . #'avy-goto-char)))

(use-package casual-avy
  :bind ("C-c a" . casual-avy-tmenu))

;; Casual / Transient UIs
(use-package casual-dired
  :bind (:map dired-mode-map
              ("C-o" . #'casual-dired-tmenu)
              ("s" . #'casual-dired-sort-by-tmenu)
              ("/" . #'casual-dired-search-replace-tmenu)))

(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-package casual-ibuffer
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu))
  :after (ibuffer))

;; Formatting
(use-package apheleia
  :diminish apheleia-mode
  :init
  (apheleia-global-mode)
  :config
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-i" "2" "-"))
  (setf (alist-get 'sh-mode apheleia-mode-alist)
        '(shfmt)))

;; Colorize hex color codes
(use-package rainbow-mode)

;; Git
(use-package magit
  :config
  (setq magit-define-global-key-bindings 'recommended
        magit-diff-refine-hunk t))

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (magit-pre-refresh . diff-hl-magit-pre-refresh))
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Terminal
(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (display-line-numbers-mode 0)))
  :config
  (custom-set-faces
   '(term-color-bright-black ((t (:foreground "#4C566A"))))))

;; Eglot LSP
(use-package eglot
  :bind
  (:map eglot-mode-map
        :prefix-map eglot-prefix-keymap
        :prefix "C-c c"
        ("a" . eglot-code-actions)
        ("o" . eglot-code-actions-organize-imports)
        ("r" . eglot-rename)
        ("f" . eglot-format)
        ("d" . eglot-find-declaration)
        ("r" . eglot-find-implementation)
        ("t" . eglot-find-typeDefinition))
  :config
  ;; Server customization
  (setf eglot-server-programs
        '(((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
          (rust-ts-mode . ("rust-analyzer"
                           :initializationOptions
                           (:check
                            (:command "clippy")
                            :procMacro
                            (:enable :json-false))))))
  (defalias 'start-lsp-server #'eglot)
  :hook
  (c-ts-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure))

;; Lisps
(use-package parinfer-rust-mode
  :preface
  (defun safe-parinfer-rust-mode nil
    "Safely turn on `parinfer-rust-mode'."
    (electric-pair-mode nil) ; conflicts with parinfer
    (parinfer-rust-mode))
  :config
  (setq
   parinfer-rust-troublesome-modes '())
  (set-face-attribute
   'parinfer-rust-dim-parens nil
   :foreground (cadr (assoc 'base6 doom-themes--colors)))
  :hook
  ((emacs-lisp-mode sly-mode geiser-mode) . safe-parinfer-rust-mode))

;; Rust
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Guile
(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/Workspace/guix"))

;; Markdown
(use-package markdown-mode)

;; IRC/ERC
(let ((erc-init (expand-file-name "erc-init.el" user-emacs-directory)))
  (when (file-exists-p erc-init)
    (load-file erc-init)))

(provide 'init)

;;; init.el ends here
