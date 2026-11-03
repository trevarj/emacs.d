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
  (defun trev/use-package-ensure (name args state &optional no-refresh)
    "Skip remote archives when NAME is already loadable from `load-path'."
    (unless (or (package-built-in-p name) (locate-library (symbol-name name)))
      (use-package-ensure-elpa name args state no-refresh)))
  :custom
  (use-package-always-defer t)
  (use-package-always-ensure t)
  (use-package-ensure-function #'trev/use-package-ensure)
  (use-package-expand-minimally t))

(use-package auth-source :ensure nil :custom (auth-sources '("~/.authinfo.gpg")))

(use-package modus-themes
  :demand t
  :preface
  (defvar trev/theme--changing nil
    "Non-nil while replacing or recovering a desktop theme.")

  (defun trev/desktop-theme-private-p (path directory)
    "Whether PATH is an owned, private real DIRECTORY or regular file."
    (let ((attributes (file-attributes path 'integer)))
      (and attributes
           (eq (file-attribute-type attributes) (and directory t))
           (= (file-attribute-user-id attributes) (user-uid))
           (zerop (logand (file-modes path) #o077))
           (equal path (directory-file-name (file-truename path)))
           (if directory
               (file-directory-p path)
             (and (file-regular-p path) (file-readable-p path)
                  (> (file-attribute-size attributes) 0))))))

  (defun trev/theme-set-fonts ()
    "Apply font preferences without overriding theme palette colors."
    (set-face-attribute 'default nil :family "Iosevka" :height 166 :weight 'medium)
    (set-face-attribute 'fixed-pitch nil :family "Iosevka Fixed" :height 166)
    (set-face-attribute 'fixed-pitch-serif nil :family "Noto Serif" :height 166)
    (set-face-attribute 'variable-pitch nil :family "Noto Sans" :weight 'normal))

  (defun trev/desktop-theme-load ()
    "Load the theme selected by the shared desktop theme bundle."
    (interactive)
    (let* ((case-fold-search nil)
           (root (expand-file-name "~/.local/share/trev-themes/"))
           ;; Capture the link once: a publication during loading must not mix
           ;; metadata from one generation with another generation's Lisp.
           (active (expand-file-name "active" root))
           (target (file-symlink-p active))
           (runtime (and target (file-name-absolute-p target)))
           (bundle
            (cond
             (target
              (unless (and (not (file-remote-p target))
                           (or runtime (equal target "current")))
                (error "Invalid active desktop theme link: %s" target))
              (file-truename (expand-file-name target root)))
             ((file-exists-p active)
              (error "Active desktop theme is not a managed symlink"))
             ;; Only pre-handoff deployments may lack active.  A dangling or
             ;; malformed active link is an error, never a reason to use current.
             (t (file-truename (expand-file-name "current" root)))))
           (_validated
            (when runtime
              (let* ((xdg-runtime (getenv "XDG_RUNTIME_DIR"))
                     (directory (directory-file-name
                                 (if (and xdg-runtime
                                          (not (string-empty-p xdg-runtime)))
                                     xdg-runtime
                                   (format "/run/user/%s" (user-uid)))))
                     (runtime-root (expand-file-name "trev-themes" directory)))
                (unless (and (file-name-absolute-p directory)
                             (equal target bundle)
                             (string-match-p
                              (concat "\\`" (regexp-quote runtime-root)
                                      "/noctalia\\.[a-zA-Z0-9]\\{6\\}\\'")
                              bundle)
                             (trev/desktop-theme-private-p directory t)
                             (trev/desktop-theme-private-p runtime-root t)
                             (trev/desktop-theme-private-p bundle t))
                  (error "Invalid private desktop theme generation: %s" bundle))
                (dolist (asset '("umbriel.toml" "noctalia-theme.el"
                                 "emacs-theme" "mode"))
                  (unless (trev/desktop-theme-private-p
                           (expand-file-name asset bundle) nil)
                    (error "Invalid private desktop theme asset: %s" asset)))
                (unless (with-temp-buffer
                          (insert-file-contents (expand-file-name "mode" bundle))
                          (member (buffer-string) '("dark\n" "light\n")))
                  (error "Invalid Noctalia desktop theme mode"))
                (unless (with-temp-buffer
                          (insert-file-contents
                           (expand-file-name "noctalia-theme.el" bundle))
                          (and (not (re-search-forward "{{\\|}}" nil t))
                               (string-match-p
                                "\n;;; noctalia-theme\\.el ends here\n?\\'"
                                (buffer-string))))
                  (error "Incomplete Noctalia desktop theme source")))))
           (selection (with-temp-buffer
                        (insert-file-contents (expand-file-name "emacs-theme" bundle))
                        (buffer-string)))
           (name (string-trim selection))
           (theme (intern-soft name)))
      (when runtime
        (unless (equal selection "noctalia\n")
          (error "Invalid Noctalia desktop theme selection")))
      (unless (memq theme '(modus-vivendi-deuteranopia
                            modus-operandi-tinted
                            modus-operandi
                            modus-operandi-deuteranopia
                            modus-operandi-tritanopia
                            modus-vivendi
                            modus-vivendi-tinted
                            modus-vivendi-tritanopia
                            trev-ayu-dark
                            trev-ayu-mirage
                            trev-ayu-light
                            trev-nord
                            trev-nord-light
                            trev-osaka-jade
                            trev-everforest
                            trev-everforest-light
                            trev-kanagawa
                            trev-kanagawa-dragon
                            trev-kanagawa-lotus
                            trev-flexoki-dark
                            trev-flexoki-light
                            trev-catppuccin-latte
                            trev-catppuccin-frappe
                            trev-catppuccin-macchiato
                            trev-catppuccin-mocha
                            noctalia))
        (error "Invalid desktop theme: %s" name))
      (unless (eq (eq theme 'noctalia) (and runtime t))
        (error "Noctalia requires a private runtime desktop theme generation"))
      (let ((native (memq theme '(modus-vivendi-deuteranopia
                                  modus-operandi-tinted
                                  modus-operandi
                                  modus-operandi-deuteranopia
                                  modus-operandi-tritanopia
                                  modus-vivendi
                                  modus-vivendi-tinted
                                  modus-vivendi-tritanopia))))
        (unless native
          (let ((source (expand-file-name (concat name "-theme.el") bundle)))
            (unless (file-readable-p source)
              (error "Desktop theme source is not readable: %s" source))))
        (let ((old-themes (copy-sequence custom-enabled-themes))
              (old-plist (copy-tree (symbol-plist theme) t))
              (was-known (custom-theme-p theme)))
          (unwind-protect
              (let ((trev/theme--changing t))
                (condition-case condition
                    (progn
                      ;; NO-ENABLE does not protect an already active target.
                      (when (memq theme custom-enabled-themes)
                        (disable-theme theme))
                      ;; Local declarations work without preloading Org, even
                      ;; when the loader's source form is evaluated on its own.
                      (defvar org-hide-leading-stars)
                      (defvar org-startup-indented)
                      (let ((custom-theme-load-path
                             (if native
                                 (copy-sequence custom-theme-load-path)
                               (list bundle)))
                            ;; The unchanged upstream footer must not install
                            ;; delayed Org work or retain its source directory.
                            (after-load-alist (copy-tree after-load-alist))
                            (org-hide-leading-stars
                             (and (boundp 'org-hide-leading-stars)
                                  org-hide-leading-stars))
                            (org-startup-indented
                             (and (boundp 'org-startup-indented)
                                  org-startup-indented)))
                        (load-theme theme t t))
                      (unless (and (get theme 'theme-feature)
                                   (get theme 'theme-settings))
                        (error "Desktop theme has no definition: %s" name))
                      (enable-theme theme)
                      (dolist (old old-themes)
                        (unless (eq old theme)
                          (disable-theme old))))
                  ((error quit)
                   ;; Enabling can fail after applying faces but before
                   ;; registering the target in `custom-enabled-themes'.
                   ;; Clean its new settings before restoring the old plist.
                   (when (custom-theme-p theme)
                     (unless (memq theme custom-enabled-themes)
                       (push theme custom-enabled-themes))
                     (disable-theme theme))
                   (setplist theme old-plist)
                   (if was-known
                       (unless (custom-theme-p theme)
                         (push theme custom-known-themes))
                     (setq custom-known-themes
                           (remq theme custom-known-themes)))
                   (dolist (old (reverse old-themes))
                     (enable-theme old))
                   (signal (car condition) (cdr condition)))))
            (trev/theme-set-extra-faces))))))

  (defun trev/desktop-theme-initialize ()
    "Load shared state at startup, warning and falling back when necessary."
    (trev/desktop-theme-start-server)
    (let* ((active (expand-file-name "~/.local/share/trev-themes/active"))
           (current (expand-file-name "~/.local/share/trev-themes/current"))
           (has-state (or (file-exists-p active) (file-symlink-p active)
                          (file-exists-p current) (file-symlink-p current))))
      (when has-state
        (condition-case condition
            (trev/desktop-theme-load)
          ((error quit)
           (display-warning 'desktop-theme (error-message-string condition)
                            :warning))))
      (when (or (not has-state) (not custom-enabled-themes))
        (condition-case condition
            (load-theme 'modus-vivendi-deuteranopia t)
          ((error quit)
           (display-warning 'desktop-theme (error-message-string condition)
                            :warning))))))

  (defun trev/desktop-theme-start-server ()
    "Start the default server independently of desktop theme initialization."
    (condition-case condition
        (progn
          (require 'server)
          (unless (server-running-p)
            (server-start)))
      ((error quit)
       (display-warning 'desktop-theme (error-message-string condition)
                        :warning))))

  (defun trev/desktop-theme-select ()
    "Select a shared desktop theme without blocking emacsclient callbacks."
    (interactive)
    (let ((program (executable-find "theme-switch")))
      (unless program
        (user-error "theme-switch is not available"))
      (make-process
       :name "desktop-theme-picker"
       :buffer "*Desktop theme picker*"
       :command (list program)
       :noquery t
       :sentinel
       (lambda (process event)
         (when (and (memq (process-status process) '(exit signal))
                    (not (zerop (process-exit-status process))))
           (message "Theme picker failed (%s); see *Desktop theme picker*"
                    (string-trim event)))))))

  (defun trev/theme-set-extra-faces (&optional theme)
    "Refresh local faces after THEME changes, outside intermediate reloads."
    (unless (or trev/theme--changing (memq theme '(user spacious-padding)))
      (let* ((background (face-background 'default nil t))
             (foreground (face-foreground 'default nil t))
             (nick (face-foreground 'error nil 'default))
             (modus (and custom-enabled-themes
                         (eq (car custom-enabled-themes)
                             (modus-themes-get-current-theme))))
             (insert (if modus
                         (modus-themes-get-color-value 'bg-added-fringe t)
                       (face-foreground 'diff-added nil 'default)))
             (change (if modus
                         (modus-themes-get-color-value 'bg-changed-fringe t)
                       (face-foreground 'diff-changed nil 'default)))
             (delete (if modus
                         (modus-themes-get-color-value 'bg-removed-fringe t)
                       (face-foreground 'diff-removed nil 'default))))
        (custom-set-faces
         `(margin ((t :background ,background :foreground ,foreground)))
         `(diff-hl-insert ((t :foreground ,insert :background ,background)))
         `(diff-hl-change ((t :foreground ,change :background ,background)))
         `(diff-hl-delete ((t :foreground ,delete :background ,background)))
         '(modus-themes-ui-variable-pitch ((t :height 0.9)))
         `(erc-current-nick-face ((t :foreground ,nick :background ,background))))
        ;; Reapply only fonts after faces, leaving the palette's colors intact.
        (trev/theme-set-fonts))))

  (defun trev/theme-set-frame-faces (frame)
    "Refresh local faces using FRAME's active colors."
    (with-selected-frame frame
      (trev/theme-set-extra-faces)))
  :bind ("C-c T" . trev/desktop-theme-select)
  :hook (after-init . trev/desktop-theme-initialize)
  :custom
  (modus-themes-include-derivatives-mode t)
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
  (require 'diff-mode)
  (add-hook 'enable-theme-functions #'trev/theme-set-extra-faces 90)
  (add-hook 'disable-theme-functions #'trev/theme-set-extra-faces 90)
  (add-hook 'spacious-padding-mode-hook #'trev/theme-set-extra-faces)
  (add-hook 'after-make-frame-functions #'trev/theme-set-frame-faces 90))

(use-package standard-themes :demand t)

(use-package spacious-padding
  :demand t
  :bind ("<f8>" . spacious-padding-mode)
  :config
  (setq spacious-padding-widths trev/spacious-padding-widths
        spacious-padding-subtle-frame-lines nil)
  (spacious-padding-mode))

(use-package emacs
  :preface
  (defun header-line-file-path ()
    "Set `header-line-format' if symbol `buffer-file-name' is not nil."
    (when buffer-file-name
      (setq header-line-format '("%f"))))

  (defun open-user-config ()
    "Opens user's config file"
    (interactive) (find-file user-init-file))

  (define-advice message-make-date
      (:around (orig-fun &rest args) messages-force-utc-tz)
    "Force UTC timezone generation for the message date header."
    (with-environment-variables
        (("TZ" "Etc/UTC"))
      (let ((res (apply orig-fun args)))
        (set-time-zone-rule nil)
        res)))

  :hook
  ((find-file . header-line-file-path)
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
          ("\\*\\(xref\\|Occur\\|.*diagnostics\\).*\\*"
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
        safe-local-variable-directories '("~/Workspace/")
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
        (plist-get trev/spacious-padding-widths :right-divider-width))
  (window-divider-mode)                 ; Gap between splits
  (trev/theme-set-fonts)
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
   ("C-'" . 'mode-line-other-buffer)
   ("C-c !" . 'open-user-config)
   ("M-o" . 'other-window)))

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

(use-package visual-fill-column
  :demand t
  :custom (visual-fill-column-width 80)
  :config
  (global-visual-line-mode)
  (global-visual-fill-column-mode))

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
     (herdr-project "Herdr" ?h)
     (project-any-command "Other" ?o))))

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
  :custom
  (ibuffer-human-readable-size t)
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
(use-package savehist
  :demand t
  :config
  (savehist-mode))

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
  :hook ((completion-at-point-functions . cape-file))
  :bind ("C-c p" . cape-prefix-map)
  :custom (text-mode-ispell-word-completion 'cape-dict))

;; Snippets
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :bind
  (:map yas-minor-mode-map
        ("TAB" . nil)
        ("<tab>" . nil)
        ("C-c y" . 'yas-insert-snippet)))

(use-package yasnippet-snippets
  :after yasnippet)

;; Consult functions
(use-package consult
  :custom
  (consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                     "--full-path --color=never --hidden --no-ignore-vcs"))
  :bind (("C-c /" . consult-ripgrep)
         ("C-c SPC" . consult-fd)
         ("C-c r" . consult-recent-file)
         ("C-<return>" . consult-line)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ;; `C-u M-g f' widens to the whole project.
         ("M-g f" . consult-flymake)
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
      (nix             "nix-community/tree-sitter-nix")
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
           '((shfmt . ("shfmt" "-i" "2" "-ci" "-kp" "-sr"))
             (rustfmt . ("rustfmt" "--edition" "2024" "--quiet" "--emit" "stdout"))
             (nixfmt . ("nixfmt"))))
    (add-to-list 'apheleia-formatters formatter-cmd))
  (add-to-list 'apheleia-mode-alist '(nix-ts-mode . nixfmt)))

;; Spelling
(use-package ispell
  :custom
  (ispell-local-dictionary "en")
  (ispell-alternate-dictionary "en_US"))

(use-package flyspell
  :hook
  ((message-mode . flyspell-mode)
   (markdown-ts-mode . flyspell-mode)))

;; Git
(use-package magit
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-refine-hunk t)
  (magit-blame-echo-style 'headings)
  (magit-diff-fontify-hunk t)
  (magit-diff-use-indicator-faces t)
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
  (nix-ts-mode . eglot-ensure)
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
          (python-ts-mode . ("pylsp"))
          (nix-ts-mode . ("nil")))))

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
  :custom (geiser-repl-per-project-p t))

;; Nix
(use-package nix-ts-mode
  :mode "\\.nix\\'")

;; Common Lisp
(use-package sly :custom (inferior-lisp-program (executable-find "sbcl")))

;; Secrets
(use-package my-secrets :ensure nil)

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
  (notmuch-identities '("tmarjeski@gmail.com" "tarjeski@gmail.com"))
  (notmuch-saved-searches
   ;; Lieer syncs each account into one flat maildir and maps Gmail labels to
   ;; notmuch tags, so scope by `tag:account-*' (from the post-new hook) and
   ;; select folders with Gmail's own tags rather than by path.
   '((:name "unread [u]" :query "tag:inbox and tag:account-main and tag:unread" :key "u")
     (:name "inbox [i]" :query "tag:inbox and tag:account-main" :key "i")
     (:name "lists [l]" :query "tag:inbox and tag:account-lists and tag:unread" :key "l" :search-type tree)
     (:name "guix-devel [g]" :query "tag:guix-devel and tag:unread" :key "g" :search-type tree)
     (:name "guix-help [h]" :query "tag:guix-help and tag:unread" :key "h" :search-type tree)
     (:name "emacs-devel [e]" :query "tag:emacs-devel and tag:unread" :key "e" :search-type tree)
     (:name "emacs-bugs [b]" :query "tag:emacs-bugs and tag:unread" :key "b" :search-type tree)
     (:name "flagged [f]" :query "tag:flagged" :key "f")
     (:name "sent [s]" :query "tag:sent and tag:account-main" :key "s")
     (:name "trash [T]" :query "tag:trash or tag:spam" :key "T")
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

(use-package pastes
  :vc (:url "git@github.com:trevarj/pastes.git"
	    :branch "main"
	    :lisp-dir "elisp"
	    :main-file "pastes.el"
	    :rev :newest)
  :commands (pastes-setup
             pastes-region
             pastes-buffer
             pastes-file
             pastes-clipboard-image
             pastes-delete-url))

(use-package trev-clatter :ensure nil :demand t)

(use-package nostr
  :ensure nil
  :load-path "/home/trev/Workspace/nostr.el"
  :bind (("C-c $" . nostr-open))
  :commands (nostr-open
             nostr-open-profile
             nostr-open-notifications
             nostr-open-relays
             nostr-open-id)
  :custom
  (nostr-backend-command
   "/home/trev/Workspace/nostr.el/target/release/nostr-el-backend"))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

;;; init.el ends here
