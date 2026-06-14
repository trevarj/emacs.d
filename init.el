;;; init.el --- Trev's Emacs Config -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; My living Emacs config.
;;;
;;; Code:

(use-package package
  :custom
  (package-install-upgrade-built-in t)
  ;; Don't forget to run `package-quickstart-refresh'!
  (package-quickstart t)
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-mirror" . "https://www.mirrorservice.org/sites/melpa.org/packages/") t))

(use-package use-package
  :preface
  (defun trev/use-package-trusted-loadable-p (name)
    "Return non-nil if NAME is available from a trusted local source."
    (or (package-built-in-p name)
        (when-let* ((path (locate-library (symbol-name name)))
                    (file (expand-file-name path)))
          (or (and (boundp 'user-lisp-directory)
                   (file-in-directory-p file user-lisp-directory))
              (and (boundp 'lisp-directory)
                   (file-in-directory-p file lisp-directory))
              (string-match-p "/share/emacs/site-lisp/" file)
              (file-in-directory-p file (expand-file-name "~/Workspace/"))))))

  (defun trev/use-package-ensure (name args state &optional no-refresh)
    "Check installed and loadable packages before checking remote archives."
    (let ((not-feature (not (featurep name))))
      (cond
       ((not not-feature) nil)
       ((not (equal '(t) args))
        (use-package-ensure-elpa name args state no-refresh))
       ((trev/use-package-trusted-loadable-p name)
        ;; Local, built-in, and site-lisp packages are already in `load-path'.
        nil)
       (t
        (use-package-ensure-elpa name args state no-refresh)))))
  :custom
  (use-package-always-defer t)
  (use-package-always-ensure t)
  (use-package-ensure-function #'trev/use-package-ensure)
  (use-package-expand-minimally t))

(use-package auth-source :ensure nil :custom (auth-sources '("~/.authinfo.gpg")))

(use-package autothemer
  :disabled t
  :demand t)

(use-package modus-themes
  :demand t
  :preface
  (defconst trev/modus-theme-cache-file
    (expand-file-name
     "emacs/modus-theme.el"
     (or (getenv "XDG_CACHE_HOME")
         (expand-file-name ".cache" "~")))
    "File where the last selected Modus theme is persisted.")

  (defun trev/modus-theme-valid-p (theme)
    "Return non-nil when THEME belongs to the Modus themes collection."
    (and (boundp 'modus-themes-items)
         (memq theme modus-themes-items)))

  (defun trev/modus-theme-read-cache ()
    "Return the cached Modus theme, or nil if the cache is invalid."
    (when (file-readable-p trev/modus-theme-cache-file)
      (with-temp-buffer
        (insert-file-contents trev/modus-theme-cache-file)
        (condition-case nil
            (let ((theme (read (current-buffer))))
              (when (trev/modus-theme-valid-p theme)
                theme))
          (error nil)))))

  (defun trev/modus-theme-save-current ()
    "Persist the active Modus theme."
    (when-let* ((theme (car custom-enabled-themes))
                (_ (trev/modus-theme-valid-p theme)))
      (make-directory (file-name-directory trev/modus-theme-cache-file) t)
      (with-temp-file trev/modus-theme-cache-file
        (prin1 theme (current-buffer))
        (terpri (current-buffer)))))

  (defun trev/modus-theme-load-startup ()
    "Load the cached Modus theme, falling back to a dark Modus variant."
    (modus-themes-load-theme
     (or (trev/modus-theme-read-cache) 'modus-vivendi-deuteranopia)))

  (defun trev/modus-theme-set-extra-faces (&rest _)
    "Apply extra face tweaks after Modus theme changes."
    (modus-themes-with-colors
      (custom-set-faces
       `(margin ((,c :background ,bg-main :foreground ,fg-main)))
       `(diff-hl-insert ((,c :foreground ,bg-added-fringe :background ,bg-main)))
       `(diff-hl-change ((,c :foreground ,bg-changed-fringe :background ,bg-main)))
       `(diff-hl-delete ((,c :foreground ,bg-removed-fringe :background ,bg-main)))
       `(modus-themes-ui-variable-pitch ((,c :height 0.9)))
       `(erc-current-nick-face ((,c :foreground ,red-intense :background ,bg-main))))))
  :hook
  ((modus-themes-after-load-theme . trev/modus-theme-save-current)
   (modus-themes-after-load-theme . trev/modus-theme-set-extra-faces))
  :bind
  (("C-c T" . modus-themes-select)
   ("C-c t" . modus-themes-toggle)
   :repeat-map trev/modus-themes-repeat-map
   ("t" . modus-themes-toggle))
  :custom
  (modus-themes-include-derivatives-mode t)
  (modus-themes-to-toggle
   '(modus-vivendi-deuteranopia modus-operandi-tinted))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-ui nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-common-palette-overrides
   '((bg-search-current bg-yellow-intense)))
  (modus-themes-headings
   '((0 . (variable-pitch light 1.6))
     (1 . (variable-pitch light 1.5))
     (2 . (variable-pitch regular 1.4))
     (3 . (variable-pitch regular 1.3))
     (4 . (variable-pitch semibold 1.25))
     (5 . (variable-pitch semibold 1.15))
     (6 . (semibold 1.1))
     (7 . (semibold 1.05))
     (t . (semibold))))
  :config
  (with-eval-after-load 'diff-hl
    (trev/modus-theme-set-extra-faces))
  (trev/modus-theme-load-startup))

(use-package standard-themes :demand t)

(use-package spacious-padding
  :demand t
  :preface
  (defvar trev/spacious-padding-faces-enabled nil
    "Non-nil when spacious face padding is enabled.")

  (defun trev/spacious-padding-toggle-faces ()
    "Toggle spacious face padding without rewriting frame parameters."
    (interactive)
    (if trev/spacious-padding-faces-enabled
        (progn
          (spacious-padding-unset-invisible-dividers)
          (setq trev/spacious-padding-faces-enabled nil))
      (spacious-padding-set-faces)
      (setq trev/spacious-padding-faces-enabled t)))
  :bind ("<f8>" . trev/spacious-padding-toggle-faces)
  :config
  (setq spacious-padding-widths trev/spacious-padding-widths
        spacious-padding-subtle-frame-lines nil)
  (advice-add #'spacious-padding-set-faces :after #'trev/modus-theme-set-extra-faces)
  (add-hook 'enable-theme-functions #'spacious-padding-set-faces)
  (spacious-padding-set-faces)
  (setq trev/spacious-padding-faces-enabled t))

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

  (defun trev/keyboard-quit-dwim ()
    "Do-What-I-Mean behavior for `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((get-buffer-window "*Completions*" 0)
      (delete-completion-window))
     ((active-minibuffer-window)
      (abort-recursive-edit))
     ((and (derived-mode-p 'special-mode)
           (not (one-window-p)))
      (quit-window))
     (t
      (keyboard-quit))))

  (define-advice message-make-date
      (:around (orig-fun &rest args) messages-force-utc-tz)
    "Force UTC timezone generation for the message date header."
    (with-environment-variables
        (("TZ" "Etc/UTC"))
      (let ((res (apply orig-fun args)))
        (set-time-zone-rule nil)
        res)))

  :hook
  ((emacs-startup . display-startup-time)
   (buffer-list-update . header-line-file-path)
   ;; (prog-mode . display-line-numbers-mode)
   )
  :init
  (save-place-mode)
  ;; `recentf-mode' loads and cleans the recentf file; defer it out of the
  ;; critical startup path while keeping `consult-recent-file' ready shortly
  ;; after the first frame appears.
  (run-with-idle-timer 1 nil #'recentf-mode)
  (column-number-mode)                  ; Column number mode
  (delete-selection-mode)
  (electric-pair-mode)                  ; Pair the pairs
  (refill-mode)
  (put 'suspend-frame 'disabled t)      ; Disable suspend-frame
  (setq auto-fill-function 'do-auto-fill
        auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t))
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "xdg-open"
        backup-directory-alist '(("." . "~/.cache/emacs/backups"))
        comment-auto-fill-only-comments t
        confirm-kill-emacs nil
        custom-file "/tmp/custom.el"
        custom-safe-themes t
        dictionary-server "dict.org"
        display-buffer-alist
        '(((category . xref-jump)
           (display-buffer-reuse-window display-buffer-use-some-window)
           (some-window . mru))
          ("\\*\\(Help\\|helpful\\|Customize\\|info\\|Ibuffer\\|.*eshell\\).*\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right) (slot . 0) (window-width . .5))
          ("\\*\\(xref\\|Occur\\|.*diagnostics\\|.*vterm\\).*\\*"
           (display-buffer-below-selected display-buffer-at-bottom)
           (window-height . .2)))
        display-line-numbers-grow-only t
        display-line-numbers-width-start t
        eldoc-echo-area-use-multiline-p nil
        enable-recursive-minibuffers t
        epg-pinentry-mode 'ask
        eval-expression-print-length 30
        fill-column 80
        help-window-select t
        indent-tabs-mode nil
        inhibit-startup-message t
        kill-buffer-quit-windows t
        kill-do-not-save-duplicates t
        mode-line-end-spaces nil
        mode-line-front-space nil
        mode-line-compact 'long
        mode-line-collapse-minor-modes
        '(auto-revert-mode ws-butler-mode editorconfig-mode apheleia-mode
                           eldoc-mode envrc-mode yas-minor-mode which-key-mode)
        mouse-wheel-progressive-speed nil
        quit-restore-window-no-switch 'skip-first
        read-extended-command-predicate #'command-completion-default-include-p
        ring-bell-function 'ignore
        save-interprogram-paste-before-kill t
        safe-local-variable-directories '("~/Workspace/guix")
        scroll-preserve-screen-position 1
        send-mail-function 'message-send-mail-with-sendmail
        tab-always-indent 'complete
        trusted-content '("~/Workspace/" "./lisp/")
        undo-limit 67108864
        undo-outer-limit 1006632960
        undo-strong-limit 100663296
        use-default-font-for-symbols nil
        use-dialog-box nil
        use-short-answers t
        user-full-name "Trevor Arjeski"
        user-mail-address "tmarjeski@gmail.com"
        warning-minimum-level :error
        window-combination-resize t
        window-divider-default-right-width
        (or (plist-get trev/spacious-padding-widths :right-divider-width) 30))
  (window-divider-mode)                 ; Gap between splits
  ;; Fonts
  (set-face-attribute 'default nil :family "Iosevka JBM" :height 166 :weight 'medium)
  (set-face-attribute 'fixed-pitch nil :family "Iosevka JBM" :height 166)
  (set-face-attribute 'fixed-pitch-serif nil :family "Noto Serif" :height 166)
  (set-face-attribute 'variable-pitch nil :family "Cantarell" :weight 'normal)
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
   ("C-'" . 'switch-to-buffer-last)
   ("C-c !" . 'open-user-config)
   ("M-o" . 'other-window)
   ("C-g" . 'trev/keyboard-quit-dwim)))

(use-package desktop
  :custom
  (desktop-load-locked-desktop 'check-pid)
  :config
  (add-to-list 'desktop-minor-mode-table '(envrc-mode nil))
  (add-to-list 'desktop-minor-mode-table '(envrc-global-mode nil))
  :init (desktop-save-mode))

(use-package fringe
  :custom (fringes-outside-margins t)
  :init
  (define-fringe-bitmap 'right-curly-arrow
    [0 0 0 0 28672 30720 15360 7680 3840 52992 65280 65024 64512 64512 65024 65024])
  (define-fringe-bitmap 'left-curly-arrow
    [0 0 0 0 14 30 60 120 240 243 255 127 63 63 127 127]))

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
  (project-mode-line 'non-remote)
  (project-switch-commands
   '((consult-fd "Find file" ?f)
     (consult-ripgrep "Find regexp" ?g)
     (consult-project-buffer "Buffers" ?b)
     (project-find-matching-buffer "Matching buffer" ?B)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)
     (project-vterm "Vterm" ?t)
     (project-any-command "Other" ?o)))
  :config
  (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode)))

(use-package dired
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-clean-up-buffers-too t)
  (dired-clean-confirm-killing-deleted-buffers t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (wdired-create-parent-directories t))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t))

(use-package xref
  :defer t
  :custom
  (xref-auto-jump-to-first-definition 'show)
  (xref-search-program 'ripgrep))

(use-package eldoc
  :custom (eldoc-help-at-pt t))

(use-package elisp-mode
  :ensure nil
  :preface
  (defun trev/elisp-enable-docstring-eldoc ()
    "Show Emacs Lisp function docstrings in ElDoc."
    (add-hook 'eldoc-documentation-functions
              #'elisp-eldoc-funcall-with-docstring nil t))
  :hook (emacs-lisp-mode . trev/elisp-enable-docstring-eldoc))

;; Keybinding
(use-package which-key
  :init (which-key-mode)
  :bind (("C-c K" . 'which-key-show-major-mode))
  :custom
  (which-key-max-description-length nil)
  (which-key-popup-type 'side-window)
  (which-key-sort-order 'which-key-description-order))

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
  :defer t
  :init
  (setq grep-use-headings t
        grep-command "rg -nHS0 --no-heading "))

;; Ibuffer
(use-package ibuffer
  :preface
  (defun ibuffer-mark-unimportant-for-delete ()
    (interactive)
    (ibuffer-mark-special-buffers)
    (ibuffer-mark-read-only-buffers)
    (ibuffer-change-marks ?> ?D))
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind
  (:map ibuffer-mode-map
        ("* D" . #'ibuffer-mark-unimportant-for-delete)))

(use-package autoinsert :init (auto-insert-mode))

;; Minibuffer
(use-package vertico
  :custom
  (vertico-count 6)
  (vertico-cycle t)
  (vertico-scroll-margin 5)
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-styles '(orderless basic)))

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

;; Completion-at-point helper
(use-package cape
  :init (add-to-list 'completion-at-point-functions #'cape-file)
  :bind ("C-c p" . cape-prefix-map)
  :custom (text-mode-ispell-word-completion 'cape-dict))

;; Snippets
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil)
        ("<tab>" . nil)
        ("C-c y" . 'yas-insert-snippet))
  :config
  (add-to-list 'yas-snippet-dirs "~/Workspace/guix/etc/snippets/yas"))

(use-package yasnippet-snippets
  :after yasnippet)

;; Consult functions
(use-package consult
  :preface
  (defun consult-flymake-project ()
    "Jump to project Flymake diagnostics."
    (interactive)
    (consult-flymake t))
  :custom
  (consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                     "--full-path --color=never --hidden --no-ignore-vcs"))
  :bind (("C-c /" . consult-ripgrep)
         ("C-c SPC" . consult-fd)
         ("C-c r" . consult-recent-file)
         ("C-<return>" . consult-line)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake-project)
         ("M-g M-g" . consult-goto-line))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Embark for collecting completions / results
(use-package embark
  :bind
  (("C-c e" . embark-act)
   ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package treesit
  :ensure nil
  :preface
  (defun treesit-standard-langs (langs)
    "Convert LANGS into `treesit-language-source-alist' entries."
    (seq-map
     (lambda (entry)
       (pcase entry
         ((pred symbolp)
          `(,entry ,(format "https://github.com/tree-sitter/tree-sitter-%s" entry)))
         (`(,lang ,repo . ,rest)
          `(,lang ,(format "https://github.com/%s" repo) ,@rest))
         (_ (error "Invalid entry in treesit-standard-langs: %S" entry))))
     langs))

  :custom
  (treesit-auto-install-grammar 'ask)
  (treesit-font-lock-level 4)
  (treesit-enabled-modes t)
  (treesit-language-source-alist
   (treesit-standard-langs
    '(
      bash c cpp css go html jsdoc json python rust toml
      ;; custom repo / branch / src-dir
      (dockerfile      "camdencheek/tree-sitter-dockerfile")
      (gomod           "camdencheek/tree-sitter-go-mod")
      (javascript      "tree-sitter/tree-sitter-javascript"
                       :revision "master" :source-dir "src")
      (typescript      "tree-sitter/tree-sitter-typescript"
                       :revision "master" :source-dir "typescript/src")
      (tsx             "tree-sitter/tree-sitter-typescript"
                       :revision "master" :source-dir "tsx/src")
      (kdl             "tree-sitter-grammars/tree-sitter-kdl")
      (lua             "tree-sitter-grammars/tree-sitter-lua")
      (yaml            "tree-sitter-grammars/tree-sitter-yaml")
      (markdown        "tree-sitter-grammars/tree-sitter-markdown"
                       :revision "split_parser" :source-dir "tree-sitter-markdown/src")
      (markdown-inline "tree-sitter-grammars/tree-sitter-markdown"
                       :revision "split_parser" :source-dir "tree-sitter-markdown-inline/src")))))

;; Whitespace handling
(use-package ws-butler :demand 2 :config (ws-butler-global-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;; Editorconfig
(use-package editorconfig
  :hook (after-init . editorconfig-mode))

;; Buffer specific direnv
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Avy navigation
(use-package avy
  :bind
  (("C-c w" . #'avy-goto-word-0)
   ("C-c s" . #'avy-goto-char-timer)))

;; Formatting
(use-package apheleia
  :demand t
  :config
  (apheleia-global-mode)
  (dolist (formatter-cmd
           '((guix-style . ("guix" "style" "-f" inplace))
             (shfmt . ("shfmt" "-i" "2" "-ci" "-kp" "-sr"))
             (rustfmt . ("rustfmt" "--edition" "2024" "--quiet" "--emit" "stdout"))))
    (add-to-list 'apheleia-formatters formatter-cmd))
  (add-to-list 'apheleia-mode-alist '(guix-scheme-mode . guix-style)))

;; Spelling
(use-package ispell
  :custom
  (ispell-local-dictionary "english")
  (ispell-alternate-dictionary "english.alias"))

(use-package flyspell
  :hook
  ((message-mode . flyspell-mode)
   (markdown-ts-mode . flyspell-mode)))

;; Colorize hex color codes
(use-package rainbow-mode)

;; Git
(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk t)
  (magit-blame-echo-style 'headings)
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config (add-to-list 'magit-git-environment "TZ=UTC"))

(use-package ghub
  :init
  (with-eval-after-load 'ghub
    ;; Guix Home links authinfo.gpg into /gnu/store without suffix; Ghub's
    ;; symlink cache workaround prevents Auth-Source from finding entries there.
    (advice-remove #'auth-source-netrc-parse
                   #'auth-source-netrc-parse@handle-symlink)))

(use-package forge
  :after magit)

;; Git modes
(use-package git-modes)

(use-package diff-hl
  :preface
  (define-fringe-bitmap 'vertical-bar-large (make-vector 60 #xf0))
  (define-fringe-bitmap 'horizontal-bar-large (make-vector 4 #xff) nil nil 'top)
  (defun diff-hl-bmp-fn-override (type _pos)
    "Override diff-hl bitmaps."
    (pcase type
      ('delete 'horizontal-bar-large)
      (_ 'vertical-bar-large)))
  :custom
  (diff-hl-fringe-bmp-function #'diff-hl-bmp-fn-override)
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

;; Terminal
(use-package vterm
  :ensure nil
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
  :custom
  (flymake-fringe-indicator-position nil)
  (flymake-show-diagnostics-at-end-of-line t)
  :hook (prog-mode . flymake-mode))

;; Eglot LSP
(use-package eglot
  :custom
  (eglot-extend-to-xref t)
  (eglot-code-action-indications '(eldoc-hint))
  :hook
  (c-ts-mode . eglot-ensure)
  (c++-ts-mode . eglot-ensure)
  (bash-ts-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  :bind
  (:map eglot-mode-map
        :prefix-map eglot-prefix-keymap
        :prefix "C-c l"
        ("a" . eglot-code-actions)
        ("o" . eglot-code-actions-organize-imports)
        ("r" . eglot-rename)
        ("f" . eglot-format)
        ("d" . eglot-find-declaration)
        ("i" . eglot-find-implementation)
        ("t" . eglot-find-typeDefinition))
  :config
  (setf eglot-server-programs
        '(((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
          (rust-ts-mode . ("rust-analyzer"
                           :initializationOptions
                           (:check
                            (:command "clippy")
                            :cargo
                            (:features "all"))))
          ((c-ts-mode c++-ts-mode) . ("clangd"))
          ((tsx-ts-mode typescript-ts-mode) . ("typescript-language-server" "--stdio"))
          (python-ts-mode . ("pylsp"))))
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
  :custom
  (geiser-repl-per-project-p t)
  :config
  ;; (add-to-list 'geiser-guile-load-path "~/Workspace/guix")
  (add-to-list 'geiser-guile-load-path "~/Workspace/nonguix"))

;; Guix
(use-package guix
  :mode
  (("/home/trev/Workspace/trev-guix/.*\\.scm\\'" . guix-scheme-mode)
   ("/home/trev/Workspace/guix/.*\\.scm\\'" . guix-scheme-mode)))

;; Common Lisp
(use-package sly :custom (inferior-lisp-program (executable-find "sbcl")))

;; Secrets
(use-package my-secrets
  :ensure nil)

;; IRC/ERC
(use-package erc
  :preface
  (defvar erc-znc-server)

  (defun erc-connect ()
    (interactive)
    (require 'my-secrets)
    (unless (bound-and-true-p erc-znc-server)
      (user-error "`erc-znc-server' is not set"))
    (setq erc-server erc-znc-server)
    (erc-tls :server erc-server :port erc-port
             :user erc-nick :full-name erc-user-full-name))

  (defun erc-clear-query-buffer ()
    (when (erc-query-buffer-p)
      (erc-send-input-line "*status" (format "clearbuffer %s" (erc-target)))))

  (defun erc-toggle-fools ()
    (interactive)
    (message "erc: hidden fools %s"
             (if (equal '(t) (erc-match-toggle-hidden-fools nil)) "OFF" "ON"))
    (set-buffer-modified-p t))

  (defun trev/erc-nicks-refresh-buffers ()
    "Refresh ERC nick colors against the current theme background."
    (setq erc-nicks-bg-color (face-background 'default nil t))
    (when (fboundp 'erc-nicks-refresh)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'erc-mode)
                     (bound-and-true-p erc-nicks-mode))
            (erc-with-server-buffer
              (setq erc-nicks--bg-luminance nil
                    erc-nicks--bg-mode-value nil
                    erc-nicks--fg-rgb
                    (or (color-name-to-rgb
                         (face-foreground 'erc-default-face nil 'default))
                        (color-name-to-rgb
                         (readable-foreground-color erc-nicks-bg-color)))))
            (erc-nicks-refresh nil))))))

  (defvar erc--msg-props)
  (defvar erc-networks--id)

  (defvar my-erc-server-time-watermarks nil
    "Alist of latest seen ERC server-time stamps by network and target.")

  (defun my-erc-server-time--tag-value (tags name)
    "Return NAME from ERC message TAGS."
    (when-let* ((tag (seq-find (lambda (tag)
                                 (equal (format "%s" (car-safe tag)) name))
                               tags))
                (value (cdr tag)))
      (if (listp value) (car value) value)))

  (defun my-erc-server-time--parsed ()
    "Return the inserted message's parsed ERC response."
    (when-let* ((pos (erc-find-parsed-property)))
      (erc-get-parsed-vector pos)))

  (defun my-erc-server-time--message-time ()
    "Return the inserted message's IRCv3 server-time value."
    (when-let* ((parsed (my-erc-server-time--parsed))
                (stamp (my-erc-server-time--tag-value
                        (erc-response.tags parsed) "time")))
      (ignore-errors (date-to-time stamp))))

  (defun my-erc-server-time--target-key ()
    "Return the watermark key for the current ERC buffer."
    (let ((network (or (and (boundp 'erc-networks--id)
                            erc-networks--id
                            (erc-networks--id-string erc-networks--id))
                       (erc-with-server-buffer
                         (or erc-server-announced-name erc-session-server))
                       erc-session-server))
          (target (or (erc-default-target) (buffer-name))))
      (list network target)))

  (defun my-erc-server-time--watermark ()
    "Return the latest seen server-time for the current ERC buffer."
    (alist-get (my-erc-server-time--target-key)
               my-erc-server-time-watermarks nil nil #'equal))

  (defun my-erc-server-time--set-watermark (time)
    "Record TIME as seen for the current ERC buffer."
    (let* ((key (my-erc-server-time--target-key))
           (watermark (alist-get key my-erc-server-time-watermarks
                                 nil nil #'equal)))
      (when (or (null watermark) (time-less-p watermark time))
        (setf (alist-get key my-erc-server-time-watermarks nil nil #'equal)
              time))))

  (defun my-erc-server-time-apply-message-time ()
    "Make ERC's displayed timestamp use the IRCv3 time tag."
    (when-let* ((time (my-erc-server-time--message-time))
                ((boundp 'erc--msg-props))
                ((hash-table-p erc--msg-props)))
      (puthash 'erc--ts time erc--msg-props)))

  (defun my-erc-server-time-track-modified-channels (orig &rest args)
    "Suppress `erc-track' for messages at or before the seen watermark."
    (let* ((time (my-erc-server-time--message-time))
           (watermark (and time (my-erc-server-time--watermark))))
      (if (and watermark (not (time-less-p watermark time)))
          nil
        (prog1 (apply orig args)
          (when time
            (my-erc-server-time--set-watermark time))))))
  :bind
  (("C-c #" . 'erc-connect)
   :map erc-mode-map
   ("C-c -" . 'erc-toggle-fools)
   ("C-c g" . 'erc-track-clear))
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
  (erc-nicks-color-adjustments
   '(erc-nicks-invert erc-nicks-add-contrast erc-nicks-cap-contrast
     erc-nicks-ensaturate))
  (erc-nicks-contrast-range '(7.0 . 18.0))
  (erc-nicks-saturation-range '(0.35 . 0.85))
  (erc-port 7777)
  (erc-prompt 'erc-prompt-format)
  (erc-prompt-format (propertize "%n:" 'font-lock-face 'erc-input-face))
  (erc-receive-query-display 'bury)
  (erc-server-reconnect-function 'erc-server-delayed-check-reconnect)
  (erc-scrolltobottom-all t)
  (erc-spelling-dictionaries '(("Libera.Chat" "english")))
  (erc-timestamp-format "%H:%M")
  (erc-track-exclude-server-buffer t)
  (erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY" "PART"
                             "353" "324" "332" "329" "333" "477"))
  (erc-track-shorten-start 6)
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
                        (erc-server-send "CAP REQ :server-time")
                        (erc-server-send "CAP END")))
  (add-hook 'erc-insert-modify-hook #'my-erc-server-time-apply-message-time -90)
  (with-eval-after-load 'erc-track
    (advice-add #'erc-track-modified-channels
                :around #'my-erc-server-time-track-modified-channels))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables
                 'my-erc-server-time-watermarks))
  ;; Clear out query bufs when using using `AutoClearQueryBuffer = false`
  (add-to-list 'erc-kill-buffer-hook 'erc-clear-query-buffer)
  (erc-spelling-mode)
  (erc-scrolltobottom-mode)
  (defun erc-match-directed-at-fool-p (_msg) nil)
  :hook
  (erc-mode . (lambda ()
                (auto-fill-mode -1)
                (add-to-list 'completion-at-point-functions 'cape-emoji)))
  (erc-text-matched . erc-hide-fools)
  (modus-themes-after-load-theme . trev/erc-nicks-refresh-buffers))

(use-package elfeed
  :preface
  (defun trev/elfeed-load-secrets (&rest _)
    "Load secrets before opening Elfeed."
    (require 'my-secrets))

  (defun elfeed-show-visit-eww ()
    (interactive)
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit)))
  :custom
  (elfeed-use-libxml t)
  :init
  (advice-add 'elfeed :before #'trev/elfeed-load-secrets)
  :bind
  (("C-c @" . #'elfeed)
   :map elfeed-show-mode-map
   ("e" . 'elfeed-show-visit-eww)))

;; GNU Bug tracker
(use-package debbugs)

(use-package org
  :custom
  (org-directory "~/.local/share/org/")
  (org-default-notes-file "~/.local/share/org/notes.org")
  (org-agenda-files '("~/.local/share/org/notes.org"))
  (org-capture-templates
   '(("n" "Note" entry (file+headline "~/.local/share/org/notes.org" "Notes")
      "* %U %?\n")))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c n" . (lambda ()
                (interactive)
                (find-file org-default-notes-file)))
   ("C-c N" . consult-org-heading))
  :config
  (make-directory org-directory t))

(use-package notmuch
  :defer t
  :preface
  (defun trev/notmuch-mark-all-read ()
    "Mark every message in the current notmuch view as read."
    (interactive)
    (let ((query (pcase major-mode
                   ('notmuch-search-mode (notmuch-search-get-query))
                   ('notmuch-tree-mode   (notmuch-tree-get-query))
                   ('notmuch-show-mode   (notmuch-show-get-query))
                   (_ (user-error "Not in a notmuch search/tree/show buffer")))))
      (when (yes-or-no-p (format "Mark [%s] read?" query))
        (notmuch-tag query '("-unread"))
        (notmuch-refresh-this-buffer))))
  :bind
  (("C-c m" . notmuch)
   :map notmuch-search-mode-map
   ("M" . trev/notmuch-mark-all-read)
   :map notmuch-tree-mode-map
   ("M" . trev/notmuch-mark-all-read)
   :map notmuch-show-mode-map
   ("M" . trev/notmuch-mark-all-read))
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (notmuch-archive-tags '("-unread"))
  (notmuch-fcc-dirs nil)
  (notmuch-identities '("tmarjeski@gmail.com"))
  (notmuch-saved-searches
   '((:name "unread [u]" :query "path:main/INBOX/** and tag:unread and not tag:deleted" :key "u")
     (:name "inbox [i]" :query "path:main/INBOX/** and not tag:deleted" :key "i")
     (:name "github [G]" :query "tag:github and tag:unread and not tag:deleted" :key "G")
     (:name "guix-devel [g]" :query "tag:guix-devel and tag:unread" :key "g" :search-type tree)
     (:name "guix-help [h]" :query "tag:guix-help and tag:unread" :key "h" :search-type tree)
     (:name "emacs-devel [e]" :query "tag:emacs-devel and tag:unread" :key "e" :search-type tree)
     (:name "emacs-bugs [b]" :query "tag:emacs-bugs and tag:unread" :key "b" :search-type tree)
     (:name "sent [s]" :query "path:\"main/[Gmail]/Sent Mail/**\"" :key "s")
     (:name "trash [T]" :query "path:main/[Gmail]/Trash/** or tag:deleted" :key "T")
     (:name "all-mail-6m [a]" :query "tag:account-main and date:6M.." :key "a")))
  (notmuch-hello-sections
   '(notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-alltags))
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-indent 0)
  (notmuch-column-control 1.0)
  (notmuch-search-oldest-first nil)
  (notmuch-show-logo nil)
  (notmuch-show-relative-dates t)
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-indent-multipart nil)
  (notmuch-message-headers '("To" "Cc" "Subject" "Date"))
  (notmuch-message-headers-visible t)
  (notmuch-wash-wrap-lines-length 120)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (smtpmail-smtp-user "tmarjeski@gmail.com")
  :hook ((notmuch-mua-send . notmuch-mua-attachment-check)))

;;; Local Packages

(use-package aoc
  :ensure nil
  :commands (aoc-fetch-input aoc-view-problem aoc-submit-answer)
  :config
  (setopt savehist-additional-variables
          (append savehist-additional-variables '(aoc-year aoc-day-level))))

(use-package termbin
  :ensure nil)

(use-package guix-lsp-eglot
  :load-path "/home/trev/Workspace/rust-projects/guix-lsp/editors/emacs"
  :commands (guix-lsp-eglot-ensure guix-lsp-eglot-start guix-lsp-eglot-setup)
  :custom
  (guix-lsp-executable "/home/trev/Workspace/rust-projects/guix-lsp/target/debug/guix-lsp")
  (guix-lsp-extra-roots '("/home/trev/Workspace/guix" "/home/trev/Workspace/trev-guix"))
  (guix-lsp-command-timeout 120)
  :hook
  (guix-scheme-mode . guix-lsp-eglot-ensure))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

;;; init.el ends here
