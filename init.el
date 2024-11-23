;;; init.el --- Trev's Emacs Config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; A minimalistic development environment
;;;
;;; Code:

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package use-package
  :preface
  (defun trev/use-package-ensure (name args _state &optional _no-refresh)
    "Checks for local package before checking remote archives."
    (if-let* ((not-feature (not (featurep name)))
              (path (locate-library (symbol-name name)))
              (_ (not (package-installed-p name))))
        (package-install-file path)
      (when not-feature (use-package-ensure-elpa name args _state _no-refresh))))
  :custom
  (use-package-expand-minimally t)
  (use-package-always-defer t)              ; always defer packages, use :demand instead
  (use-package-always-ensure t)             ; always ensure packages
  (use-package-ensure-function #'trev/use-package-ensure))

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
  (desktop-save-mode)                   ; Session saving
  (column-number-mode)                  ; Column number mode
  (global-auto-revert-mode)             ; Auto-refresh buffers
  (electric-pair-mode)                  ; Pair the pairs
  (auto-fill-mode)                      ; Autofill mode
  (window-divider-mode)                 ; Gap between splits
  (put 'suspend-frame 'disabled t)      ; Disable suspend-frame
  ;; Fonts
  (set-face-attribute 'default nil :family "Iosevka JBM" :height 150 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans")
  ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
  (let ((nerdfont-code-points
         '((#xE6FA . #xE6B2) (#xE700 . #xE7C5) (#xF000 . #xF2E0) (#xE200 . #xE2A9)
           (#xF500 . #xFD46) (#xE300 . #xE3EB) (#xF400 . #xF532) (#xE0B4 . #xE0C8)
           (#xF0001 . #xF1AF0) (#xE0CC . #xE0D4) (#x23FB . #x23FE) (#xF300 . #xF372)
           (#xE000 . #xE00A) (#xEA60 . #xEBEB) #x2665 #x26A1 #xF2A1 #xF27C #xE0A3 #xE0CA)))
    (dolist (code-point nerdfont-code-points)
      (set-fontset-font t code-point (font-spec :family "Symbols Nerd Font Mono"))))

  ;; Miscellaneous Options
  :custom
  (auto-fill-function 'do-auto-fill)
  (auto-revert-verbose nil)
  (auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t)))
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (comment-auto-fill-only-comments t)                   ; Autofill comments only
  (confirm-kill-emacs nil)
  (custom-file "/tmp/custom.el")                        ; customization file
  (desktop-load-locked-desktop 'check-pid)              ; load if lock pid doesn't exist
  (display-buffer-alist                                 ; Prefer right split for matched buffers
   '(("\\*\\(Help\\|helpful\\|Customize\\|info\\|xref\\).*\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (slot . 0)
      (window-width .5))))
  (display-line-numbers-grow-only t)                    ; Never shrink the linum width
  (display-line-numbers-width-start t)                  ; Calculate linum width at start
  (eldoc-echo-area-use-multiline-p nil)
  (enable-recursive-minibuffers t)                      ; Recursive mini-buffers
  (epg-pinentry-mode 'loopback)                         ; pinentry on minibuffer
  (fill-column 80)                                      ; 80 width pages
  (global-auto-revert-non-file-buffers t)               ; Auto-refresh buffers like dired
  (indent-tabs-mode nil)                                ; Use spaces only
  (inhibit-startup-message t)                           ; No startup screen
  (kill-buffer-quit-windows t)
  (mode-line-end-spaces nil)
  (mode-line-front-space nil)                           ; Nicer -nw mode line
  (mouse-wheel-progressive-speed nil)
  (quit-restore-window-no-switch 'skip-first)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (scroll-preserve-screen-position 1)                   ; PgUp/PgDown hold
  (send-mail-function 'message-send-mail-with-sendmail) ; Use sendmail
  (tab-always-indent 'complete)                         ; TAB to complete
  (treesit-font-lock-level 4)                           ; More treesitter faces
  (undo-limit 67108864)                                 ; 64mb.
  (undo-outer-limit 1006632960)                         ; 960mb.
  (undo-strong-limit 100663296)                         ; 96mb.
  (use-default-font-for-symbols nil)                    ; For nerd fonts
  (use-dialog-box nil)                                  ; Bye
  (use-short-answers t)                                 ; y/n
  (user-full-name "Trevor Arjeski")
  (user-mail-address "tmarjeski@gmail.com")
  (window-divider-default-right-width 16)               ; Padding between splits
  ;; Generic keybindings
  :bind
  (("C-c b" . ibuffer)
   ("C-'" . 'switch-to-buffer-last)
   ("C-c !" . 'open-user-config)))

;; Ligatures
(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '(
     "-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
     "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
     "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__" "--"
     "<~~" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!="
     "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
     "(*" "*)" "++" "+++" "|-" "-|" "<!--" "<!---")))

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

;; Project.el

(use-package project
  :bind
  (:map project-prefix-map
        (("t" . 'eat-project-other-window)))
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (consult-ripgrep "Find regexp" ?g)
     (magit-project-status "Magit" ?m)
     (eat-project-other-window "Term" ?t)
     (project-any-command "Other" ?o))))

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

;; Searching
(use-package isearch
  :custom
  (isearch-lazy-count t)
  :bind
  (:map isearch-mode-map
        ("C-n" . isearch-repeat-forward)
        ("C-p" . isearch-repeat-backward)))

(use-package grep
  :custom
  (grep-use-headings t)
  (grep-command "rg -nHS0 --no-heading "))

;; Ibuffer
(use-package ibuffer
  :preface
  (defun ibuffer-mark-unimportant-for-delete ()
    (interactive)
    (ibuffer-mark-special-buffers)
    (ibuffer-mark-read-only-buffers)
    (ibuffer-change-marks ?> ?D))
  :bind
  (:map ibuffer-mode-map
        ("* D" . #'ibuffer-mark-unimportant-for-delete)))

;; Minibuffer
(use-package vertico
  :custom
  (vertico-scroll-margin 5)
  (vertico-count 10)
  (vertico-cycle t)
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package icomplete
  :disabled
  :init (fido-vertical-mode)
  :custom
  (icomplete-compute-delay 0.05)
  (icomplete-delay-completions-threshold 2000)
  (completion-styles '(flex partial-completion emacs22))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completions-max-height 10)
  :custom-face
  (icomplete-selected-match ((t (:background ,(get-doom-theme-color 'base4)))))
  :bind
  (:map icomplete-fido-mode-map
        (("TAB" . 'icomplete-force-complete)))
  (:map completion-list-mode-map
        (("C-p" . 'minibuffer-previous-completion)
         ("C-n" . 'minibuffer-next-completion))))

;; Save minibuffer history between restarts
(use-package savehist
  :init
  (savehist-mode))

;; Minibuffer annotations
(use-package marginalia
  :init (marginalia-mode))

;; Useful functions
(use-package consult
  :bind (("C-c c r" . consult-register)
         ("C-c /" . consult-ripgrep)
         ("C-<return>" . consult-line)
         ("C-c <SPC>" . consult-fd)
         ("C-c c i" . consult-info)
         ("C-c c m" . consult-man)
         ("C-c r" . consult-recent-file)
         ("C-x b" . consult-buffer)                ; orig. switch-to-buffer
         ("C-c c p" . consult-project-buffer)      ; orig. project-switch-to-buffer
         ("C-c c b" . consult-bookmark)            ; orig. bookmark-jump
         ("M-g M-g" . consult-goto-line)           ; orig. goto-line
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g k" . consult-global-mark)
         ("M-g m" . consult-mark)
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
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t))

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
  :bind
  (:map corfu-map
        ("RET" . nil))
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay 0)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-left-margin-width 4)
  (corfu-right-margin-width 4)
  :config
  (corfu-popupinfo-mode)
  (add-to-list 'corfu--frame-parameters '(internal-border-width . 4))
  (set-face-background 'corfu-border (get-doom-theme-color 'orange)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion-at-point helper
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  :custom
  (text-mode-ispell-word-completion 'cape-dict))

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
   ("C-c s" . #'avy-goto-char-timer)))

(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode))

;; Formatting
(use-package format-all
  :diminish
  :commands format-all-mode
  :hook
  (prog-mode . format-all-mode)
  (prog-mode . format-all-ensure-formatter)
  :config
  (setq-default format-all-formatters
                '(("Shell" (shfmt "-i" "2" "-ci")))))

;; Colorize hex color codes
(use-package rainbow-mode)

;; Git
(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk t)
  :hook (git-commit-setup . git-commit-turn-on-flyspell))

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
(use-package eat
  :commands (eat)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size 1200000))

;; Eglot LSP
(use-package eglot
  :hook
  (c-ts-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
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
  (defalias 'start-lsp-server #'eglot))

;; Paredit
(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . enable-paredit-mode)
  :bind (:map paredit-mode-map (("M-?" . nil))))

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
         :password trev/irc-password))
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

  (defun erc-track-external-notification ()
    "Run on `erc-track-list-changed-hook' and silently writes to a named
fifo /tmp/erc-track.fifo."
    (when-let* ((fifo "/tmp/erc-track.fifo")
                (file-exists-p fifo))
      (write-region
       (format "%s\n" (or (mapcar 'buffer-name (mapcar #'car erc-modified-channels-alist)) ""))
       nil fifo t 'quiet)))
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
  (erc-fools trev/erc-fools)
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
                    erc-default-face erc-action-face erc-fool-face
                    erc-notice-face erc-input-face erc-prompt-face))
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
  :hook
  (erc-mode . (lambda ()
                (auto-fill-mode -1)
                (add-to-list 'completion-at-point-functions 'cape-emoji)
                (setq-local scroll-margin 0)))
  (erc-text-matched . erc-hide-fools)
  (erc-match-mode . (lambda ()
                      (set-face-attribute 'erc-current-nick-face nil
                                          :foreground (get-doom-theme-color 'red)
                                          :slant 'italic :weight 'heavy)))
  (erc-track-list-changed . erc-track-external-notification)
  :bind
  (("C-c #" . 'erc-connect)
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
  (setopt elfeed-feeds trev/elfeed-feeds)
  :bind
  (("C-c @" . #'elfeed)
   :map elfeed-show-mode-map
   ("e" . 'elfeed-show-visit-eww)))

;; GNU Bug tracker
(use-package debbugs)

(use-package gnus
  ;; Gmail integration taken from https://github.com/kensanata/ggg
  :preface
  (defun my-gnus-summary-keys ()
    (local-set-key "y" 'gmail-archive)
    (local-set-key "d" 'gmail-trash)
    (local-set-key "$" 'gmail-report-spam))

  (defun gmail-archive ()
    "Archive the current or marked mails.
This moves them into the All Mail folder."
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

  (defun gmail-trash ()
    "Trash the current or marked mails.
This moves them into the Trash folder."
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Trash"))

  (defun gmail-report-spam ()
    "Report the current or marked mails as spam.
This moves them into the Spam folder."
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))

  :custom
  (gnus-init-file nil)
  (gnus-use-dribble-file nil)
  ;; You need to replace this key ID with your own key ID!
  (mml-secure-openpgp-signers '("A52D68794EBED758"))
  ;; This tells Gnus to get email from Gmail via IMAP.
  (gnus-select-method
   '(nnimap "gmail"
            ;; It could also be imap.googlemail.com if that's your server.
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)))
  (gnus-secondary-select-methods '((nntp "news.gmane.io")))
  ;; This tells Gnus to use the Gmail SMTP server. This
  ;; automatically leaves a copy in the Gmail Sent folder.
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  ;; Tell message mode to use SMTP.
  (message-send-mail-function 'smtpmail-send-it)
  ;; Gmail system labels have the prefix [Gmail], which matches
  ;; the default value of gnus-ignored-newsgroups. That's why we
  ;; redefine it.
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  ;; The agent seems to confuse nnimap, therefore we'll disable it.
  (gnus-agent nil)
  ;; We don't want local, unencrypted copies of emails we write.
  (gnus-message-archive-group nil)
  ;; We want to be able to read the emails we wrote.
  (mml-secure-openpgp-encrypt-to-self t)

  ;; https://www.emacswiki.org/emacs/GnusFormatting
  (gnus-summary-line-format "%U%R%z %([%&user-date;]  %-20,20f  %B%s%)\n")
  (gnus-group-line-format "%M%S%p%P%B%(%G%) (%y)\n")
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today %10R")
     ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday %6R")
     (t . "%Y-%m-%d %R")))
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  ;; Only show first message in list
  (gnus-thread-hide-subtree t)
  ;; ignore the subject and look at 'In-Reply-To:' and 'References:' headers
  (gnus-thread-ignore-subject t)
  (gnus-thread-indent-level 2)
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")
  :hook
  ;; Encrypt all messages
  (message-setup . mml-secure-message-encrypt)
  (gnus-summary-mode . my-gnus-summary-keys))

;;; Local Packages

(use-package leetcode
  :vc
  (:url "https://github.com/trevarj/leetcode.el"
        :rev "9c5bd70")
  :load my-secrets
  :config
  (setopt leetcode-session-cookie trev/leetcode-session-cookie)
  :custom
  (leetcode-prefer-language "cpp")
  (leetcode-save-solutions t)
  (leetcode-directory "~/Workspace/leetcode/"))

(use-package aoc
  :load my-secrets
  :config
  (setopt aoc-session-cookie trev/aoc-session-cookie
          savehist-additional-variables
          (append savehist-additional-variables '(aoc-year aoc-day-level))))

(use-package launch-program
  :bind (("s-SPC" . launch-program-launch)))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

;;; init.el ends here
