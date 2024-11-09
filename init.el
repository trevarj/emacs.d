;;; init.el --- Trev's Emacs Config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; A minimalistic development environment
;;;
;;; Code:

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package emacs
  :demand t
  :preface
  (defun display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

  (defun header-line-file-path ()
    "Set `header-line-format' if symbol `buffer-file-name' is not nil."
    (when buffer-file-name
      (setq header-line-format '("%f"))))

  (defun sudo-save ()
    (interactive)
    (if (not buffer-file-name)
        (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
      (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (defun open-user-config ()
    "Opens user's config file"
    (interactive) (find-file user-init-file))

  (defun switch-to-buffer-last ()
    "Toggles back to previously visited buffer"
    (interactive) (switch-to-buffer nil))

  :hook
  ((emacs-startup . display-startup-time)
   (buffer-list-update . header-line-file-path)
   (prog-mode . display-line-numbers-mode))
  :init
  (add-to-list 'load-path (concat user-emacs-directory "/lisp"))
  (save-place-mode)
  (recentf-mode)
  (desktop-save-mode)                           ; Session saving
  (column-number-mode)                          ; Column number mode
  (global-auto-revert-mode)                     ; Auto-refresh buffers
  (auto-fill-mode)                              ; Autofill mode
  (window-divider-mode)                         ; Gap between splits
  ;; Fonts
  (set-face-attribute 'default nil :family "Iosevka JBM" :height 150 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka JBM" :height 150)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans")
  (setopt use-default-font-for-symbols nil)
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
  :custom
  (user-full-name "Trevor Arjeski")
  (user-mail-address "tmarjeski@gmail.com")
  (use-package-always-defer t)              ; always defer packages, use :demand instead
  (use-package-always-ensure t)             ; always ensure installation of packages
  (custom-file "/tmp/custom.el")            ; customization file
  (desktop-load-locked-desktop 'check-pid)  ; load if lock pid doesn't exist
  (display-line-numbers-grow-only t)        ; Never shrink the linum width
  (display-line-numbers-width-start t)      ; Calculate linum width at start
  (confirm-kill-emacs nil)
  (use-dialog-box nil)                      ; Bye
  (use-short-answers t)                     ; y/n
  (global-auto-revert-non-file-buffers t)   ; Auto-refresh buffers like dired
  (auto-revert-verbose nil)                 ; But silence it
  (enable-recursive-minibuffers t)          ; Recursive mini-buffers
  (inhibit-startup-message t)               ; No startup screen
  (scroll-step 1)                           ; Vim style scrolling
  (scroll-margin 10)                        ; Vim style scroll off
  (fill-column 80)                          ; Line width 80 chars
  (comment-auto-fill-only-comments t)       ; Autofill comments only
  (mode-line-front-space nil)               ; Nicer -nw mode line
  (mode-line-end-spaces nil)                ; ^
  (window-divider-default-right-width 16)   ; Padding between splits
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t)))
  (undo-limit 67108864)                     ; Undo limit of 64mb.
  (undo-strong-limit 100663296)             ;               96mb.
  (undo-outer-limit 1006632960)             ;               960mb.
  (eldoc-echo-area-use-multiline-p nil)
  (epg-pinentry-mode 'loopback)             ; pinentry on minibuffer
  (mouse-wheel-progressive-speed nil)
  (scroll-preserve-screen-position 1)       ; PgUp/PgDown hold
  (text-mode-ispell-word-completion nil)
  (gnus-init-file (expand-file-name
                   "gnus/gnus.el"
                   user-emacs-directory))
  (send-mail-function 'message-send-mail-with-sendmail) ; Use sendmail
  (fill-column 80)                                      ; 80 width pages
  (auto-fill-function 'do-auto-fill)                    ; always autofill
  (indent-tabs-mode nil)                                ; Use spaces only
  (treesit-font-lock-level 4)                           ; More treesitter faces
  (major-mode                                           ; Guess major mode from file name
   (lambda ()
     (unless buffer-file-name
       (let ((buffer-file-name (buffer-name)))
         (set-auto-mode)))))
  ;; Generic keybindings
  :bind
  (("C-x b" . ibuffer)
   ("C-'" . 'switch-to-buffer-last)
   ("C-x C-z" . nil) ; disable suspend-frame
   ("C-c !" . 'open-user-config)))

;; Ligatures
(use-package ligature
  :defer 2
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
  :demand
  :preface
  (defun get-doom-theme-color (name)
    (car (alist-get name doom-themes--colors)))
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic nil)
  (doom-nord-padded-modeline t)
  (doom-nord-brighter-modeline t)
  :config
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (let ((bg (get-doom-theme-color 'base2))
        (bg-light (get-doom-theme-color 'base3))
        (yellow (get-doom-theme-color 'yellow))
        (orange (get-doom-theme-color 'orange)))
    (set-face-background 'cursor orange)
    (set-face-background 'highlight yellow)
    (set-face-attribute 'mode-line nil
                        :background bg-light
                        :foreground (get-doom-theme-color 'base6)
                        :box `(:line-width (6 . 6) :color ,bg-light))
    (set-face-attribute 'mode-line-active nil
                        :background bg-light
                        :box `(:line-width (6 . 6) :color ,bg-light))
    (set-face-attribute 'mode-line-inactive nil
                        :background bg
                        :foreground (get-doom-theme-color 'base5)
                        :box `(:line-width (6 . 6) :color ,bg))
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'font-lock-keyword-face nil
                        :foreground orange :weight 'bold)
    (set-face-attribute 'window-divider nil :inherit 'ansi-color-black)))

;; Diminish minor modes
(use-package diminish
  :demand
  :config (diminish 'auto-revert-mode))

(use-package xref
  :custom
  (xref-auto-jump-to-first-definition 'show)
  (xref-search-program 'ripgrep))

;; Keybinding
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-description-order)
  :init
  (which-key-mode)
  :bind
  (("C-c K" . 'which-key-show-major-mode)))

;; Window navigation
(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package undo-fu
  :init
  (global-unset-key (kbd "C-z"))
  :bind
  (("C-z" . 'undo-fu-only-undo)
   ("C-S-z" . 'undo-fu-only-redo)))

(use-package undo-fu-session
  :demand
  :config
  (undo-fu-session-global-mode))

;; Minibuffer
(use-package vertico ; completion
  :init
  (vertico-mode))

;; Save minibuffer history between restarts
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia ; completion definitions
  :init
  (marginalia-mode))

(use-package consult
  ;; https://github.com/minad/consult?tab=readme-ov-file#use-package-example
  :bind (("C-c c r" . consult-register)
         ("C-c /" . consult-ripgrep)
         ("C-c <SPC>" . consult-fd)
         ("C-c c i" . consult-info)
         ("C-c c m" . consult-man)
         ("C-c r" . consult-recent-file)
         ("C-c b" . consult-buffer)                ; orig. switch-to-buffer
         ("C-c c p" . consult-project-buffer)      ; orig. project-switch-to-buffer
         ("C-c c b" . consult-bookmark)            ; orig. bookmark-jump
         ("M-g M-g" . consult-goto-line)           ; orig. goto-line
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ; Alternative: consult-flymake
         ("M-g g" . consult-goto-line)             ; orig. goto-line
         ("M-g k" . consult-global-mark)
         ("M-g m" . consult-mark)
         ("C-s" . consult-line)
         ("C-c c s" . isearch-forward)
         ("M-y" . consult-yank-pop)                ; orig. yank-pop
         ([remap Info-search] . consult-info)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ; orig. isearch-edit-string
         ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch
         ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-r" . consult-history)
         ("M-s" . consult-history))                 ; orig. next-matching-history-element
  ;; https://github.com/minad/consult?tab=readme-ov-file#custom-variables
  :config
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t))

(use-package helpful
  :bind
  (("C-h f" . #'helpful-callable)
   ("C-h v" . #'helpful-variable)
   ("C-h k" . #'helpful-key)
   ("C-h x" . #'helpful-command)
   ("C-h o" . #'helpful-symbol)
   ("C-c C-d" . #'helpful-at-point)))

;; Completion
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay 0)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (tab-always-indent 'complete)
  :config
  (corfu-popupinfo-mode)
  (add-to-list 'corfu--frame-parameters '(internal-border-width . 2))
  (set-face-background 'corfu-border (get-doom-theme-color 'orange))
  :bind
  (:map corfu-map
        ("RET" . nil)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion-at-point helper
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package treesit-auto
  :demand
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :config
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

(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode))

;; Formatting
(use-package format-all
  :diminish
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Shell" (shfmt "-i" "2" "-ci")))))

;; Colorize hex color codes
(use-package rainbow-mode)

;; Git
(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk t))

;; Git modes
(use-package git-modes)

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
  :after project
  :preface
  (defun project-vterm ()
    "Start a vterm shell in the current project's root directory."
    (interactive)
    (require 'comint)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-shell-name (project-prefixed-buffer-name "vterm"))
           (shell-buffer (get-buffer default-project-shell-name)))
      (if (and shell-buffer (not current-prefix-arg))
          (if (comint-check-proc shell-buffer)
              (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
            (vterm shell-buffer))
        (vterm (generate-new-buffer-name default-project-shell-name)))))
  :init
  (add-to-list 'project-switch-commands '(project-vterm "Vterm" "t"))
  :config
  (set-face-foreground 'term-color-bright-black "#4C566A"))

;; Eglot LSP
(use-package eglot
  :bind
  (:map eglot-mode-map
        :prefix-map eglot-prefix-keymap
        :prefix "C-c l"
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
                            (:enable :json-false))))
          ((c-ts-mode c++-ts-mode) . ("clangd"))))
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
  :custom
  (parinfer-rust-troublesome-modes '())
  :config
  (set-face-attribute
   'parinfer-rust-dim-parens nil
   :foreground (cadr (assoc 'base6 doom-themes--colors)))
  :hook
  ((emacs-lisp-mode sly-mode geiser-mode) . #'safe-parinfer-rust-mode))

;; Rust
(use-package rust-mode
  :custom
  (rust-mode-treesitter-derive t)
  (rust-format-on-save t))

;; Guile
(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/Workspace/guix"))

;; Common Lisp
(use-package sly
  :custom
  (inferior-lisp-program (executable-find "sbcl")))

;; Markdown
(use-package markdown-mode)

;; Secrets
(use-package my-secrets
  :ensure nil)

;; IRC/ERC
(use-package erc
  :load my-secrets
  :preface
  (defun erc-connect ()
    (interactive)
    (erc :server "orangepi"
         :port 7777
         :user "trev"
         :password my/erc-password))
  (defun erc-clear-query-buffer ()
    (when (erc-query-buffer-p)
      (erc-send-input-line "*status" (format "clearbuffer %s" (erc-target)))))
  (let ((hidden-fools t))
    (defun erc-toggle-fools ()
      (interactive)
      (setq hidden-fools (not hidden-fools))
      (erc-match-toggle-hidden-fools hidden-fools)
      (message "hidden fools: %s" (if hidden-fools "on" "off"))
      (set-buffer-modified-p t)))
  :config
  (setopt erc-modules (seq-union '(nicks scrolltobottom spelling) erc-modules)
          erc-nicks-colors (mapcar 'get-doom-theme-color
                                   '(orange yellow teal blue dark-blue cyan violet)))
  ;; for more "themed" erc-nicks-colors
  (setq erc-nicks--create-pool-function 'erc-nicks--create-culled-pool)

  ;; Fix for restoring query buffers with self-messages
  (advice-add #'erc-login
              :before (lambda ()
                        (erc-server-send "CAP REQ :znc.in/self-message")
                        (erc-server-send "CAP END")))
  ;; Clear out query bufs when using using `AutoClearQueryBuffer = false`
  (add-to-list 'erc-kill-buffer-hook 'erc-clear-query-buffer)
  (erc-spelling-mode)
  (erc-scrolltobottom-mode)
  (defun erc-match-directed-at-fool-p (_msg) nil)
  :custom
  (erc-server "orangepi")
  (erc-port "7777")
  (erc-nick "trev")
  (erc-user-full-name "trev")
  (erc-prompt 'erc-prompt-format)
  (erc-prompt-format (propertize "%n:" 'font-lock-face 'erc-input-face))
  (erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART")
                           "353" "324" "332" "329" "333" "477")
  (erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART" "353"))
  (erc-track-exclude-server-buffer t)
  (erc-track-exclude '("#emacs" "#systemcrafters-live" "*status"))
  ;; works with bug#67767 v2 patch
  (erc-nicks-track-faces t)
  (erc-fools my/erc-fools)
  (erc-fool-highlight-type 'all)
  (erc-current-nick-highlight-type 'all)
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-timestamp-format "%H:%M")
  (erc-receive-query-display 'bury)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 16)
  (erc-track-faces-priority-list
   '(erc-error-face erc-current-nick-face erc-keyword-face erc-pal-face
                    erc-nick-msg-face erc-direct-msg-face
                    erc-dangerous-host-face erc-nick-default-face
                    (erc-button-nick-default-face erc-nick-default-face)
                    (erc-button-nick-default-face erc-default-face) erc-default-face
                    erc-action-face erc-fool-face erc-notice-face erc-input-face
                    erc-prompt-face))
  :hook
  ((erc-mode . (lambda ()
                 (auto-fill-mode -1)
                 (setq-local scroll-margin 0)))
   (erc-text-matched . erc-hide-fools)
   (erc-match-mode . (lambda ()
                       (set-face-attribute 'erc-current-nick-face nil
                                           :foreground (get-doom-theme-color 'red)
                                           :slant 'italic :weight 'heavy))))
  :bind
  (("C-c e" . 'erc-connect)
   :map erc-mode-map
   ("C-c -" . 'erc-toggle-fools)))

(use-package elfeed
  :load my-secrets
  :preface
  (defun elfeed-show-visit-eww ()
    (interactive)
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit)))
  :config
  (setopt elfeed-feeds my/elfeed-feeds)
  :bind
  (("C-c @" . #'elfeed)
   :map elfeed-show-mode-map
   ("e" . 'elfeed-show-visit-eww)))

(use-package leetcode
  :vc
  (:url "https://github.com/trevarj/leetcode.el"
        :rev "9c5bd70")
  :load my-secrets
  :config
  (setopt leetcode-session-cookie my/leetcode-session-cookie)
  :custom
  (leetcode-prefer-language "cpp")
  (leetcode-save-solutions t)
  (leetcode-directory "~/Workspace/leetcode/"))

(use-package aoc
  :vc t
  :load-path "~/Workspace/advent-of-code"
  ;; (:url "https://github.com/trevarj/advent-of-code"
  ;;       :rev "9c5bd70")
  :load my-secrets
  :config
  (setopt aoc-session-cookie my/aoc-session-cookie
          savehist-additional-variables
          (append savehist-additional-variables '(aoc-year aoc-day-level))))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

;;; init.el ends here
