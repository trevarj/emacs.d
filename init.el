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
  (use-package-always-defer t)
  (use-package-always-ensure t)
  (use-package-ensure-function #'trev/use-package-ensure)
  (use-package-expand-minimally t))

(use-package autothemer)

(use-package emacs
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
  (load-theme 'nord t)
  (add-to-list 'load-path (concat user-emacs-directory "/lisp"))
  (save-place-mode)
  (recentf-mode)
  (desktop-save-mode)                   ; Session saving
  (column-number-mode)                  ; Column number mode
  (electric-pair-mode)                  ; Pair the pairs
  (auto-fill-mode)                      ; Autofill mode
  (window-divider-mode)                 ; Gap between splits
  (put 'suspend-frame 'disabled t)      ; Disable suspend-frame
  ;; Fonts
  (set-face-attribute 'default nil :family "Iosevka JBM" :height 168 :weight 'medium)
  (set-face-attribute 'variable-pitch nil :family "Noto Sans")
  ;; https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
  (dolist (code-point
           '((#xE6FA . #xE6B2) (#xE700 . #xE7C5) (#xF000 . #xF2E0) (#xE200 . #xE2A9)
             (#xF500 . #xFD46) (#xE300 . #xE3EB) (#xF400 . #xF532) (#xE0B4 . #xE0C8)
             (#xF0001 . #xF1AF0) (#xE0CC . #xE0D4) (#x23FB . #x23FE) (#xF300 . #xF372)
             (#xE000 . #xE00A) (#xEA60 . #xEBEB) #x2665 #x26A1 #xF2A1 #xF27C #xE0A3 #xE0CA))
    (set-fontset-font t code-point (font-spec :family "Symbols Nerd Font Mono")))

  ;; Generic keybindings
  :bind
  (("C-x K" . 'kill-current-buffer)
   ("C-c b" . 'ibuffer-other-window)
   ("C-c r" . 'recentf-open)
   ("C-c SPC" . 'project-find-file)
   ("C-c /" . 'project-find-regexp)
   ("C-'" . 'switch-to-buffer-last)
   ("C-<return>" . 'occur)
   ("C-c !" . 'open-user-config)
   ("M-o" . 'other-window))
  ;; Miscellaneous Options
  :custom
  (auto-fill-function 'do-auto-fill)
  (auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t)))
  (browse-url-browser-function 'browse-url-firefox)
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (comment-auto-fill-only-comments t)                   ; Autofill comments only
  (confirm-kill-emacs nil)
  (custom-file "/tmp/custom.el")                        ; customization file
  (custom-safe-themes t)
  (desktop-load-locked-desktop 'check-pid)              ; load if lock pid doesn't exist
  (dictionary-server "dict.org")
  (display-buffer-alist                                 ; Prefer right split for matched buffers
   '(("\\*\\(Help\\|helpful\\|Customize\\|info\\|Ibuffer\\|.*eshell\\).*\\*"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right) (slot . 0) (window-width . .5))
     ("\\*\\(xref\\|Occur\\|.*diagnostics\\|.*vterm\\).*\\*"
      (display-buffer-below-selected display-buffer-at-bottom)
      (window-height . .2))))
  (display-line-numbers-grow-only t)                    ; Never shrink the linum width
  (display-line-numbers-width-start t)                  ; Calculate linum width at start
  (eldoc-echo-area-use-multiline-p nil)
  (enable-recursive-minibuffers t)                      ; Recursive mini-buffers
  (epg-pinentry-mode 'loopback)                         ; pinentry on minibuffer
  (fill-column 80)                                      ; 80 width pages
  (indent-tabs-mode nil)                                ; Use spaces only
  (inhibit-startup-message t)                           ; No startup screen
  (kill-buffer-quit-windows t)
  (mode-line-end-spaces nil)
  (mode-line-front-space nil)                           ; Nicer -nw mode line
  (mouse-wheel-progressive-speed nil)
  (quit-restore-window-no-switch 'skip-first)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (ring-bell-function 'ignore)
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
  (window-divider-default-right-width 16))               ; Padding between splits

;; Ligatures
(use-package ligature
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures
   'prog-mode
   '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
     "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
     "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__" "--"
     "<~~" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!="
     "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
     "(*" "*)" "++" "+++" "|-" "-|" "<!--" "<!---")))

;; Project.el
(use-package project
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-find-regexp "Find regexp" ?g)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)
     (project-vterm "Vterm" ?t)
     (project-any-command "Other" ?o)))
  :config
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode)))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :diminish)

;; Diminish minor modes
(use-package diminish
  :demand
  :config (diminish 'auto-revert-mode))

(use-package eldoc :diminish)

(use-package xref
  :custom
  (xref-auto-jump-to-first-definition 'show)
  (xref-search-program 'ripgrep))

;; Keybinding
(use-package which-key
  :init (which-key-mode)
  :bind (("C-c K" . 'which-key-show-major-mode))
  :custom
  (which-key-popup-type 'side-window)
  (which-key-sort-order 'which-key-description-order)
  :diminish)

(use-package repeat
  :defer 1
  :hook (after-init . repeat-mode)
  :bind ("M-RET" . repeat))

(use-package undo-fu
  :init (global-unset-key (kbd "C-z"))
  :bind
  (("C-z" . 'undo-fu-only-undo)
   ("C-S-z" . 'undo-fu-only-redo)))

(use-package undo-fu-session
  :demand
  :config (undo-fu-session-global-mode))

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

(use-package autoinsert :init (auto-insert-mode))

;; Minibuffer
(use-package vertico
  :custom
  (vertico-count 10)
  (vertico-cycle t)
  (vertico-scroll-margin 5)
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-styles '(orderless emacs22)))

;; Save minibuffer history between restarts
(use-package savehist :init (savehist-mode))

;; Minibuffer annotations
(use-package marginalia :init (marginalia-mode))

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
  :init (global-corfu-mode)
  :bind (:map corfu-map ("RET" . nil))
  :custom
  (corfu-left-margin-width 4)
  (corfu-right-margin-width 4)
  (corfu-popupinfo-delay 0)
  (corfu-preview-current nil)
  (corfu-quit-no-match 'separator)
  (global-corfu-minibuffer t)
  :config
  (corfu-popupinfo-mode)
  (add-to-list 'corfu--frame-parameters '(internal-border-width . 4)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Completion-at-point helper
(use-package cape
  :init (add-to-list 'completion-at-point-functions #'cape-file)
  :custom (text-mode-ispell-word-completion 'cape-dict))

(use-package treesit-auto
  :if (treesit-available-p)
  :demand
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :config
  (global-treesit-auto-mode))

;; Whitespace handling
(use-package ws-butler :demand 2 :config (ws-butler-global-mode) :diminish)

;; Rainbow delimiters
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;; Editorconfig
(use-package editorconfig :config (editorconfig-mode) :diminish)

;; Buffer specific direnv
(use-package envrc :diminish :config (envrc-global-mode))

;; Avy navigation
(use-package avy
  :bind
  (("C-c w" . #'avy-goto-word-0)
   ("C-c s" . #'avy-goto-char-timer)))

(use-package ibuffer :hook (ibuffer-mode . ibuffer-auto-mode))

;; Formatting
(use-package format-all
  :commands format-all-mode
  :hook
  (prog-mode . format-all-mode)
  (prog-mode . format-all-ensure-formatter)
  :config
  (setq-default format-all-formatters
                '(("Shell" (shfmt "-i" "2" "-ci"))))
  :diminish)

;; Spelling
(use-package ispell
  :custom
  (ispell-local-dictionary "english")
  (ispell-alternate-dictionary "/usr/lib/aspell/english.alias"))

;; Colorize hex color codes
(use-package rainbow-mode)

;; Git
(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk t)
  (magit-blame-echo-style 'headings)
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

;; Terminal
(use-package vterm
  :bind (:map project-prefix-map
              ("t" . project-vterm))
  :preface
  (defun project-vterm ()
    (interactive)
    (defvar vterm-buffer-name)
    (let* ((default-directory (project-root     (project-current t)))
           (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer vterm-buffer-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
        (vterm))))
  :config
  (setq vterm-copy-exclude-prompt t)
  (setq vterm-max-scrollback 100000)
  (setq vterm-tramp-shells '(("ssh" "/bin/bash"))))

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-g f" . 'flymake-show-project-diagnostics)))

;; Eglot LSP
(use-package eglot
  :custom
  (eglot-code-action-indications '(eldoc-hint))
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
  (setf eglot-server-programs
        '(((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
          (rust-ts-mode . ("rust-analyzer"
                           :initializationOptions
                           (:check
                            (:command "clippy"))))
          ((c-ts-mode c++-ts-mode) . ("clangd"))))
  (defalias 'start-lsp-server #'eglot))

;; Paredit
(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . enable-paredit-mode)
  :bind (:map paredit-mode-map (("M-?" . nil))))

;; Rust
(use-package rust-mode
  :custom
  (rust-mode-treesitter-derive (treesit-available-p))
  (rust-format-on-save t))

;; Guile
(use-package geiser-guile
  :config
  (add-to-list 'geiser-guile-load-path "~/Workspace/guix"))

;; Common Lisp
(use-package sly :custom (inferior-lisp-program (executable-find "sbcl")))

;; Markdown
(use-package markdown-mode)

;; Secrets
(use-package my-secrets :ensure nil)

;; IRC/ERC
(use-package erc
  :load my-secrets
  :preface
  (defun erc-connect ()
    (interactive)
    (erc :server erc-server :port erc-port
         :user erc-nick :full-name erc-user-full-name))

  (defun erc-clear-query-buffer ()
    (when (erc-query-buffer-p)
      (erc-send-input-line "*status" (format "clearbuffer %s" (erc-target)))))

  (defun erc-toggle-fools ()
    (interactive)
    (message "erc: hidden fools %s"
             (if (equal '(t) (erc-match-toggle-hidden-fools nil)) "OFF" "ON"))
    (set-buffer-modified-p t))
  :bind
  (("C-c #" . 'erc-connect)
   :map erc-mode-map
   ("C-c -" . 'erc-toggle-fools))
  :custom
  (erc-current-nick-highlight-type 'all)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 16)
  (erc-fool-highlight-type 'all)
  (erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART" "353" "324" "329"))
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-nick "trev")
  (erc-nicks-track-faces t)
  (erc-nicks-colors 'font-lock)
  (erc-port 7777)
  (erc-prompt 'erc-prompt-format)
  (erc-prompt-format (propertize "%n:" 'font-lock-face 'erc-input-face))
  (erc-receive-query-display 'bury)
  (erc-server "znc")
  (erc-server-reconnect-function 'erc-server-delayed-check-reconnect)
  (erc-scrolltobottom-all t)
  (erc-spelling-dictionaries '(("Libera.Chat" "english")))
  (erc-timestamp-format "%H:%M")
  (erc-track-exclude '("#emacs" "#systemcrafters-live" "*status"))
  (erc-track-exclude-server-buffer t)
  (erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART")
                           "353" "324" "332" "329" "333" "477")
  (erc-user-full-name user-full-name)
  :config
  (setopt erc-modules (seq-union '(nicks scrolltobottom spelling) erc-modules)
          erc-track-faces-priority-list
          (seq-remove
           (lambda (elt) (and (listp elt) (seq-contains-p elt 'erc-default-face)))
           erc-track-faces-priority-list))
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
                (add-to-list 'completion-at-point-functions 'cape-emoji)))
  (erc-text-matched . erc-hide-fools))

(use-package elfeed
  :load my-secrets
  :preface
  (defun elfeed-show-visit-eww ()
    (interactive)
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit)))
  :bind
  (("C-c @" . #'elfeed)
   :map elfeed-show-mode-map
   ("e" . 'elfeed-show-visit-eww)))

;; GNU Bug tracker
(use-package debbugs)

(use-package gnus
  ;; Gmail integration taken from https://github.com/kensanata/ggg
  :preface
  (defun gmail-archive ()
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/All Mail"))

  (defun gmail-trash ()
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Trash"))

  (defun gmail-report-spam ()
    (interactive)
    (gnus-summary-move-article nil "nnimap+imap.gmail.com:[Gmail]/Spam"))
  :bind
  (:map
   gnus-article-mode-map
   (("q" . 'kill-current-buffer)))
  (:map
   gnus-summary-mode-map
   :prefix-map gmail-prefix-map
   :prefix "C-c"
   (("y" . 'gmail-archive)
    ("d" . 'gmail-trash)
    ("$" . 'gmail-report-spam)))
  :hook
  ;; Encrypt all messages
  (message-setup . mml-secure-message-encrypt)
  :custom
  (gnus-agent nil)
  (gnus-check-new-newsgroups nil)
  (gnus-group-line-format "%M%S%p%P%B%(%G%) (%y)\n")
  (gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  (gnus-init-file nil)
  (gnus-logo-colors
   `(,(face-attribute font-lock-builtin-face :foreground)
     ,(face-attribute font-lock-keyword-face :foreground)))
  (gnus-message-archive-group nil)
  (gnus-secondary-select-methods
   '((nntp "news.gmane.io"
           (nntp-connection-timeout 5))))
  (gnus-select-method
   '(nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl)))
  (gnus-sum-thread-tree-false-root "")
  (gnus-sum-thread-tree-indent " ")
  (gnus-sum-thread-tree-leaf-with-other "├► ")
  (gnus-sum-thread-tree-root "")
  (gnus-sum-thread-tree-single-leaf "╰► ")
  (gnus-sum-thread-tree-vertical "│")
  (gnus-summary-line-format "%U%R%z %([%&user-date;]  %-20,20f  %B%s%)\n")
  (gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references)
  (gnus-thread-hide-subtree t)
  (gnus-thread-ignore-subject t)
  (gnus-thread-indent-level 2)
  (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date))
  (gnus-use-dribble-file nil)
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today %10R")
     ((+ (* 60 60 24) (gnus-seconds-today)) . "Yesterday %6R")
     (t . "%Y-%m-%d %R")))
  (message-send-mail-function 'smtpmail-send-it)
  (mml-secure-openpgp-encrypt-to-self t)
  (mml-secure-openpgp-signers '("A52D68794EBED758"))
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587))

;;; Local Packages

(use-package aoc
  :load my-secrets
  :config
  (setopt savehist-additional-variables
          (append savehist-additional-variables '(aoc-year aoc-day-level))))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

;;; init.el ends here
