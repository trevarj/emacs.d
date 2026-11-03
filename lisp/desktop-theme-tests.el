;;; desktop-theme-tests.el --- Shared desktop theme regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l lisp/desktop-theme-tests.el -f ert-run-tests-batch-and-exit
;; Each test gets a fresh Emacs and disposable HOME/runtime/server.  Only the
;; real Modus :preface/:config/:hook are evaluated, never the full init.
;; The font regression calls the same font helper as Emacs :init.
;; Batch checks face attributes, not new-frame rendering (a GUI smoke check).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst desktop-theme-test--file (file-truename load-file-name))
(defconst desktop-theme-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory desktop-theme-test--file))))

(defun desktop-theme-test--run (name)
  (let ((home (make-temp-file "desktop-theme-test-" t))
        (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (setenv "HOME" home)
          (setenv "XDG_CONFIG_HOME" (expand-file-name ".config" home))
          (setenv "XDG_CACHE_HOME" (expand-file-name ".cache" home))
          (setenv "XDG_DATA_HOME" (expand-file-name ".local/share" home))
          (setenv "XDG_RUNTIME_DIR" (expand-file-name "runtime" home))
          (make-directory (getenv "XDG_RUNTIME_DIR"))
          (set-file-modes (getenv "XDG_RUNTIME_DIR") #o700)
          (setenv "TREV_DESKTOP_THEME_TEST" (symbol-name name))
          (let ((default-directory (file-name-as-directory home)))
            (with-temp-buffer
              (let ((status
                     (call-process
                      (expand-file-name invocation-name invocation-directory)
                      nil t nil "-Q" "--batch" "-l" desktop-theme-test--file
                      "--eval" (format "(ert-run-tests-batch-and-exit '%S)" name))))
                (unless (equal status 0)
                  (ert-fail (buffer-string)))))))
      (delete-directory home t))))

(defmacro desktop-theme-test (name &rest body)
  (declare (indent 1))
  `(ert-deftest ,name ()
     (if (equal (getenv "TREV_DESKTOP_THEME_TEST") (symbol-name ',name))
         ;; Select real color face specs on the batch frame, without claiming
         ;; that a display server has rendered them.
         (cl-letf (((symbol-function 'display-color-p) (lambda (&optional _) t))
                   ;; Background changes reclassify batch frames through the TTY API.
                   ((symbol-function 'tty-display-color-p) (lambda (&optional _) t))
                   ((symbol-function 'display-color-cells)
                    (lambda (&optional _) 16777216)))
           (set-frame-parameter nil 'display-type 'color)
           ,@body)
       (desktop-theme-test--run ',name))))

(when (getenv "TREV_DESKTOP_THEME_TEST")
  (dolist (package '("modus-themes" "spacious-padding" "diff-hl"))
    (dolist (directory
             (directory-files (expand-file-name "elpa" desktop-theme-test--root)
                              t (concat "\\`" package "-[0-9]")))
      (when (file-directory-p directory)
        (add-to-list 'load-path directory))))
  (require 'modus-themes)
  (require 'spacious-padding)
  (require 'diff-mode)
  (require 'server)
  (require 'use-package)
  (setq server-socket-dir (expand-file-name "server" "~/")
        server-auth-dir (expand-file-name "auth" "~/")
        server-use-tcp nil
        after-init-hook nil)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "init.el" desktop-theme-test--root))
    (goto-char (point-min))
    (let ((form
           (catch 'found
             (condition-case nil
                 (while t
                   (let ((candidate (read (current-buffer))))
                     (when (and (eq (car-safe candidate) 'use-package)
                                (eq (cadr candidate) 'modus-themes))
                       (throw 'found candidate))))
               (end-of-file (error "No top-level Modus use-package form")))))
          section)
      (dolist (item (cddr form))
        (if (keywordp item)
            (setq section item)
          (cond
           ((memq section '(:preface :config))
            (eval item t))
           ((eq section :hook)
            (eval `(use-package modus-themes :ensure nil :hook ,item) t)))))))
  (defface desktop-theme-test-only
    '((t :foreground "#445566" :background "#223344"))
    "A face not defined by the old theme, used to detect leaked settings."))

(defun desktop-theme-test--write (file text)
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defun desktop-theme-test--bundle (id theme background &optional extra)
  (let ((bundle (expand-file-name (concat "bundles/" id) "~/")))
    (desktop-theme-test--write (expand-file-name "emacs-theme" bundle)
                               (symbol-name theme))
    (desktop-theme-test--write
     (expand-file-name (format "%s-theme.el" theme) bundle)
     (mapconcat
      #'prin1-to-string
      `((deftheme ,theme)
        (custom-theme-set-faces
         ',theme
         '(default ((t :foreground "#eeeeee" :background ,background)))
         '(diff-added ((t :foreground "#00aa44")))
         '(diff-changed ((t :foreground "#aa00dd")))
         '(diff-removed ((t :foreground "#bb3300")))
         '(warning ((t :foreground "#ffff00")))
         '(error ((t :foreground "#ff5566")))
         ,@(mapcar (lambda (spec) (list 'quote spec)) extra))
        (provide-theme ',theme))
      "\n"))
    bundle))

(defun desktop-theme-test--select (bundle)
  (let ((current (expand-file-name "~/.local/share/trev-themes/current")))
    (make-directory (file-name-directory current) t)
    (when (file-symlink-p current) (delete-file current))
    (make-symbolic-link bundle current)))

(defun desktop-theme-test--active (target)
  (let ((active (expand-file-name "~/.local/share/trev-themes/active")))
    (make-directory (file-name-directory active) t)
    (when (file-symlink-p active) (delete-file active))
    (make-symbolic-link target active)))

(defun desktop-theme-test--runtime (background &optional mode extra tail)
  (let* ((root (expand-file-name "trev-themes" (getenv "XDG_RUNTIME_DIR")))
         (_ (make-directory root t))
         (bundle (make-temp-file (expand-file-name "noctalia." root) t))
         (source (expand-file-name "noctalia-theme.el" bundle))
         (catalog (desktop-theme-test--bundle "runtime-source" 'noctalia
                                              background extra)))
    (set-file-modes root #o700)
    (set-file-modes bundle #o700)
    (copy-file (expand-file-name "noctalia-theme.el" catalog) source)
    (with-temp-buffer
      (insert-file-contents source)
      (goto-char (point-max))
      ;; The pinned upstream footer's immediate and delayed side effects.
      (insert "\n(with-eval-after-load 'org\n"
              "  (setq org-hide-leading-stars t)\n"
              "  (setq org-startup-indented t))\n"
              "(when load-file-name\n"
              "  (add-to-list 'custom-theme-load-path\n"
              "    (file-name-as-directory (file-name-directory load-file-name))))\n"
              (or tail "")
              "\n;;; noctalia-theme.el ends here\n")
      (write-region (point-min) (point-max) source nil 'silent))
    (dolist (asset `(("umbriel.toml" . "[colors]\nbackground = \"#112233\"\n")
                     ("emacs-theme" . "noctalia\n")
                     ("mode" . ,(concat (or mode "dark") "\n"))))
      (desktop-theme-test--write (expand-file-name (car asset) bundle) (cdr asset)))
    (dolist (asset (directory-files bundle t directory-files-no-dot-files-regexp))
      (set-file-modes asset #o600))
    bundle))

(defun desktop-theme-test--appearance ()
  (mapcar (lambda (face) (cons face (face-all-attributes face)))
          '(default diff-added diff-changed diff-removed error
            desktop-theme-test-only)))

(defun desktop-theme-test--markers (colors)
  (cl-mapc (lambda (face color)
            (should (equal (face-foreground face nil t) color))
            (should (equal (face-background face nil t)
                           (face-background 'default nil t))))
          '(diff-hl-insert diff-hl-change diff-hl-delete) colors)
  (should (equal (face-foreground 'erc-current-nick-face nil t)
                 (or (face-foreground 'error nil t)
                     (face-foreground 'default nil t))))
  (should-not (equal (face-foreground 'erc-current-nick-face nil t)
                     (face-foreground 'diff-hl-delete nil t)))
  (should (equal (face-background 'erc-current-nick-face nil t)
                 (face-background 'default nil t)))
  (should (equal (face-foreground 'margin nil t)
                 (face-foreground 'default nil t)))
  (should (equal (face-background 'margin nil t)
                 (face-background 'default nil t))))

(desktop-theme-test desktop-theme-startup-and-independent-server
  (let (warnings)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      (run-hooks 'after-init-hook)
      (should (server-running-p))
      (let ((started server-process))
        (trev/desktop-theme-start-server)
        (should (eq started server-process)))
      (should-not warnings)
      (should (equal custom-enabled-themes '(modus-vivendi-deuteranopia)))
      (should (equal (face-background 'default nil t)
                     (modus-themes-get-color-value 'bg-main t)))
      (should-not (file-exists-p "~/.local/share/trev-themes/current"))
      (let ((bad (desktop-theme-test--bundle "bad" 'trev-ayu-dark "#112233"))
            (appearance (desktop-theme-test--appearance)))
        (desktop-theme-test--write (expand-file-name "emacs-theme" bad) "invalid")
        (desktop-theme-test--select bad)
        (let ((failure (should-error (trev/desktop-theme-load))))
          (trev/desktop-theme-initialize)
          (should (equal warnings (list (error-message-string failure)))))
        (should (equal (desktop-theme-test--appearance) appearance))
        (should (equal custom-enabled-themes '(modus-vivendi-deuteranopia)))
        (mapc #'disable-theme (copy-sequence custom-enabled-themes))
        (trev/desktop-theme-initialize)
        (should (equal custom-enabled-themes '(modus-vivendi-deuteranopia)))
        (desktop-theme-test--select (expand-file-name "missing-bundle" "~/"))
        (let ((failure (should-error (trev/desktop-theme-load))))
          (setq warnings nil)
          (trev/desktop-theme-initialize)
          (should (equal warnings (list (error-message-string failure)))))
        (should (equal (desktop-theme-test--appearance) appearance))
        (should (equal (file-symlink-p "~/.local/share/trev-themes/current")
                       (expand-file-name "missing-bundle" "~/")))))))

(desktop-theme-test desktop-theme-after-init-orders-server-before-load
  (let ((bundle (desktop-theme-test--bundle "startup" 'trev-nord "#102030"))
        (start (symbol-function 'trev/desktop-theme-start-server))
        (load (symbol-function 'trev/desktop-theme-load))
        events)
    (desktop-theme-test--select bundle)
    (desktop-theme-test--active "current")
    (cl-letf (((symbol-function 'trev/desktop-theme-start-server)
               (lambda () (push 'server events) (funcall start)))
              ((symbol-function 'trev/desktop-theme-load)
               (lambda ()
                 (push 'load events)
                 (should (server-running-p))
                 (funcall load))))
      (run-hooks 'after-init-hook))
    (should (equal (reverse events) '(server load)))
    (should (equal custom-enabled-themes '(trev-nord)))
    (dolist (condition '(error quit))
      (setq events nil)
      (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) nil))
                ((symbol-function 'server-start)
                 (lambda () (push 'server events)
                   (signal condition '("disposable server failure"))))
                ((symbol-function 'trev/desktop-theme-load)
                 (lambda () (push 'load events) (funcall load))))
        (run-hooks 'after-init-hook))
      (should (equal (reverse events) '(server load)))
      (should (equal custom-enabled-themes '(trev-nord))))))

(desktop-theme-test desktop-theme-active-runtime-dark-light-and-return
  (let ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030")))
    ;; Only the initial deployment may omit active.
    (desktop-theme-test--select catalog)
    (trev/desktop-theme-load)
    (should (equal custom-enabled-themes '(trev-nord)))
    (dolist (revision '(("dark" "#203040") ("light" "#e5e9f0")))
      (desktop-theme-test--active
       (desktop-theme-test--runtime (cadr revision) (car revision)))
      (trev/desktop-theme-load)
      (should (equal custom-enabled-themes '(noctalia)))
      (should (equal (face-background 'default nil t) (cadr revision)))
      (should (equal (file-symlink-p "~/.local/share/trev-themes/current")
                     catalog)))
    (desktop-theme-test--active "current")
    (trev/desktop-theme-load)
    (should (equal custom-enabled-themes '(trev-nord)))
    (should (equal (face-background 'default nil t) "#102030"))))

(desktop-theme-test desktop-theme-after-init-loads-runtime-without-current
  (desktop-theme-test--active (desktop-theme-test--runtime "#203040"))
  (run-hooks 'after-init-hook)
  (should (server-running-p))
  (should (equal custom-enabled-themes '(noctalia)))
  (should (equal (face-background 'default nil t) "#203040"))
  (should-not (file-exists-p "~/.local/share/trev-themes/current")))

(desktop-theme-test desktop-theme-active-rejects-unmanaged-and-incomplete-state
  (let ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030")))
    (desktop-theme-test--select catalog)
    (trev/desktop-theme-load)
    (let ((appearance (desktop-theme-test--appearance)))
      (dolist (fault '(catalog-noctalia absolute-catalog relative remote dangling
                      malformed-leaf uppercase-leaf selection mode footer unresolved))
        (ert-info ((format "Untrusted active state: %s" fault))
          (let ((bundle (desktop-theme-test--runtime "#abcdef")))
            (pcase fault
              ('catalog-noctalia
               (desktop-theme-test--select
                (desktop-theme-test--bundle "untrusted" 'noctalia "#abcdef"))
               (desktop-theme-test--active "current"))
              ('absolute-catalog (desktop-theme-test--active catalog))
              ('relative (desktop-theme-test--active "../../bundles/saved"))
              ('remote (desktop-theme-test--active
                        "/ssh:invalid.example:/tmp/noctalia.ABC123"))
              ('dangling
               (desktop-theme-test--active
                (expand-file-name "trev-themes/noctalia.ABC123"
                                  (getenv "XDG_RUNTIME_DIR"))))
              ('malformed-leaf
               (let ((bad (concat bundle "-extra")))
                 (rename-file bundle bad)
                 (desktop-theme-test--active bad)))
              ('uppercase-leaf
               (let ((bad (expand-file-name
                           (concat "Noctalia." (substring bundle -6))
                           (file-name-directory bundle))))
                 (rename-file bundle bad)
                 (desktop-theme-test--active bad)))
              (_
               (desktop-theme-test--write
                (expand-file-name (pcase fault
                                    ('selection "emacs-theme")
                                    ('mode "mode")
                                    (_ "noctalia-theme.el"))
                                  bundle)
                (pcase fault
                  ('selection "trev-nord\n")
                  ('mode "sepia\n")
                  ('footer "(error \"must not execute incomplete source\")\n")
                  ('unresolved
                   "{{colors.background.default.hex}}\n;;; noctalia-theme.el ends here\n")))
               (desktop-theme-test--active bundle)))
            (should-error (trev/desktop-theme-load))
            (should (equal custom-enabled-themes '(trev-nord)))
            (should (equal (desktop-theme-test--appearance) appearance))
            (desktop-theme-test--select catalog))))
      (delete-file "~/.local/share/trev-themes/active")
      (make-directory "~/.local/share/trev-themes/active")
      (should-error (trev/desktop-theme-load))
      (should (equal (desktop-theme-test--appearance) appearance)))))

(desktop-theme-test desktop-theme-runtime-requires-private-owned-real-assets
  (let ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030"))
        (attributes (symbol-function 'file-attributes)))
    (desktop-theme-test--select catalog)
    (trev/desktop-theme-load)
    (let ((appearance (desktop-theme-test--appearance)))
      (dolist (asset '("umbriel.toml" "noctalia-theme.el" "emacs-theme" "mode"))
        (dolist (fault '(missing empty public symlink directory owner))
          (ert-info ((format "%s asset: %s" fault asset))
            (let* ((bundle (desktop-theme-test--runtime "#abcdef"))
                   (file (expand-file-name asset bundle)))
              (pcase fault
                ('missing (delete-file file))
                ('empty (desktop-theme-test--write file ""))
                ('public (set-file-modes file #o644))
                ('symlink
                 (rename-file file (concat file ".real"))
                 (make-symbolic-link (concat file ".real") file))
                ('directory (delete-file file) (make-directory file)))
              (desktop-theme-test--active bundle)
              (cl-letf (((symbol-function 'file-attributes)
                         (lambda (path &optional format)
                           (let ((value (funcall attributes path format)))
                             (when (and (eq fault 'owner) (equal path file))
                               (setf (nth 2 value) (1+ (user-uid))))
                             value))))
                (should-error (trev/desktop-theme-load)))
              (should (equal custom-enabled-themes '(trev-nord)))
              (should (equal (desktop-theme-test--appearance) appearance))))))
      (dolist (component '(runtime root generation))
        (dolist (fault '(public symlink owner))
          (let* ((bundle (desktop-theme-test--runtime "#abcdef"))
                 (directory (pcase component
                              ('runtime (getenv "XDG_RUNTIME_DIR"))
                              ('root (file-name-directory bundle))
                              (_ bundle)))
                 (directory (directory-file-name directory))
                 (real (concat directory ".real")))
            (desktop-theme-test--active bundle)
            (unwind-protect
                (progn
                  (pcase fault
                    ('public (set-file-modes directory #o755))
                    ('symlink
                     (rename-file directory real)
                     (make-symbolic-link real directory)))
                  (cl-letf (((symbol-function 'file-attributes)
                             (lambda (path &optional format)
                               (let ((value (funcall attributes path format)))
                                 (when (and (eq fault 'owner)
                                            (equal path directory))
                                   (setf (nth 2 value) (1+ (user-uid))))
                                 value))))
                    (should-error (trev/desktop-theme-load)))
                  (should (equal (desktop-theme-test--appearance) appearance)))
              (when (eq fault 'symlink)
                (delete-file directory)
                (rename-file real directory))
              (set-file-modes directory #o700))))))))

(desktop-theme-test desktop-theme-noctalia-same-symbol-error-and-quit-rollback
  (let* ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030"))
         (original (face-foreground 'desktop-theme-test-only nil t))
         (a (desktop-theme-test--runtime
             "#203040" "dark"
             '((desktop-theme-test-only ((t :foreground "#ff00ff"))))))
         (b (desktop-theme-test--runtime "#e5e9f0" "light"))
         (custom-theme-load-path (cons a custom-theme-load-path))
         (search-path (copy-sequence custom-theme-load-path)))
    (desktop-theme-test--select catalog)
    (dolist (revision `((,a "#203040" "#ff00ff")
                        (,b "#e5e9f0" ,original)
                        (,a "#203040" "#ff00ff")))
      (desktop-theme-test--active (car revision))
      (trev/desktop-theme-load)
      (should (equal custom-enabled-themes '(noctalia)))
      (should (equal (face-background 'default nil t) (cadr revision)))
      (should (equal (face-foreground 'desktop-theme-test-only nil t)
                     (caddr revision)))
      (should (equal custom-theme-load-path search-path)))
    (let ((appearance (desktop-theme-test--appearance))
          (plist (copy-tree (symbol-plist 'noctalia) t))
          (after-load (copy-tree after-load-alist)))
      (dolist (phase '(load enable))
        (dolist (condition '(error quit))
          (ert-info ((format "Same Noctalia symbol: %s %s" phase condition))
            (let* ((bundle
                    (desktop-theme-test--runtime
                     "#abcdef" "light"
                     '((desktop-theme-test-only ((t :foreground "#00ffff"))))
                     (when (eq phase 'load)
                       (format "(signal '%s '(\"injected Noctalia failure\"))\n"
                               condition))))
                   (recalc (symbol-function 'custom-theme-recalc-face))
                   caught fired)
              (desktop-theme-test--active bundle)
              (cl-letf (((symbol-function 'custom-theme-recalc-face)
                         (lambda (face)
                           (funcall recalc face)
                           (when (and (eq phase 'enable)
                                      (eq face 'desktop-theme-test-only)
                                      (equal (face-foreground face nil t) "#00ffff")
                                      (not fired))
                             (setq fired t)
                             (signal condition '("injected Noctalia failure"))))))
                (condition-case failure
                    (trev/desktop-theme-load)
                  ((error quit) (setq caught failure))))
              (should (equal caught
                             (list condition "injected Noctalia failure")))
              (should (equal custom-enabled-themes '(noctalia)))
              (should (equal (symbol-plist 'noctalia) plist))
              (should (custom-theme-p 'noctalia))
              (should (equal (desktop-theme-test--appearance) appearance))
              (should (equal custom-theme-load-path search-path))
              (should (equal after-load-alist after-load)))))))
    (desktop-theme-test--active "current")
    (trev/desktop-theme-load)
    (should (equal custom-enabled-themes '(trev-nord)))
    (should (equal (face-background 'default nil t) "#102030"))))

(desktop-theme-test desktop-theme-noctalia-does-not-defer-org-side-effects
  (should-not (featurep 'org))
  (makunbound 'org-hide-leading-stars)
  (makunbound 'org-startup-indented)
  (let* ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030"))
         (bundle (desktop-theme-test--runtime "#203040"))
         (marker (expand-file-name "legitimate-org-after-load" "~/"))
         (custom-theme-load-path (cons catalog custom-theme-load-path)))
    ;; An existing Org registration must survive without receiving the footer's
    ;; additions through a shared alist entry.
    (eval-after-load 'org
      `(with-temp-file ,marker (insert "existing callback ran")))
    (let ((after-load (copy-tree after-load-alist))
          (search-path (copy-sequence custom-theme-load-path)))
      (desktop-theme-test--select catalog)
      (desktop-theme-test--active bundle)
      (trev/desktop-theme-load)
      (should-not (featurep 'org))
      (should-not (boundp 'org-hide-leading-stars))
      (should-not (boundp 'org-startup-indented))
      (should (equal after-load-alist after-load))
      (should (equal custom-theme-load-path search-path))
      (desktop-theme-test--active "current")
      (trev/desktop-theme-load)
      ;; Preferences chosen after returning to Niri must not be overwritten
      ;; when Org is loaded later.
      (set 'org-hide-leading-stars nil)
      (set 'org-startup-indented nil)
      (require 'org)
      (should (file-exists-p marker))
      (should-not (symbol-value 'org-hide-leading-stars))
      (should-not (symbol-value 'org-startup-indented))
      (should (equal custom-theme-load-path search-path)))))

(desktop-theme-test desktop-theme-noctalia-preserves-loaded-org-preferences
  (require 'org)
  (let ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030")))
    (desktop-theme-test--select catalog)
    (dolist (preferences '((nil nil) (t nil) (nil t)))
      (set 'org-hide-leading-stars (car preferences))
      (set 'org-startup-indented (cadr preferences))
      (let ((after-load (copy-tree after-load-alist))
            (search-path (copy-sequence custom-theme-load-path)))
        (desktop-theme-test--active (desktop-theme-test--runtime "#203040"))
        (trev/desktop-theme-load)
        (should (equal (symbol-value 'org-hide-leading-stars) (car preferences)))
        (should (equal (symbol-value 'org-startup-indented) (cadr preferences)))
        (should (equal after-load-alist after-load))
        (should (equal custom-theme-load-path search-path))
        (desktop-theme-test--active "current")
        (trev/desktop-theme-load)
        (should (equal (symbol-value 'org-hide-leading-stars) (car preferences)))
        (should (equal (symbol-value 'org-startup-indented) (cadr preferences)))))))

(desktop-theme-test desktop-theme-noctalia-footer-isolates-error-and-quit
  (let ((catalog (desktop-theme-test--bundle "saved" 'trev-nord "#102030")))
    (desktop-theme-test--select catalog)
    (trev/desktop-theme-load)
    (dolist (loaded '(nil t))
      (if loaded
          (progn
            (require 'org)
            (set 'org-hide-leading-stars nil)
            (set 'org-startup-indented nil))
        (should-not (featurep 'org))
        (makunbound 'org-hide-leading-stars)
        (makunbound 'org-startup-indented))
      (dolist (condition '(error quit))
        (let ((after-load (copy-tree after-load-alist))
              (search-path (copy-sequence custom-theme-load-path))
              caught)
          (desktop-theme-test--active
           (desktop-theme-test--runtime
            "#abcdef" "dark" nil
            (format "(signal '%s '(\"footer failure\"))\n" condition)))
          (condition-case failure
              (trev/desktop-theme-load)
            ((error quit) (setq caught failure)))
          (should (equal caught (list condition "footer failure")))
          (should (equal custom-enabled-themes '(trev-nord)))
          (should (equal (face-background 'default nil t) "#102030"))
          (should (eq (and (boundp 'org-hide-leading-stars) t) loaded))
          (should (eq (and (boundp 'org-startup-indented) t) loaded))
          (when loaded
            (should-not (symbol-value 'org-hide-leading-stars))
            (should-not (symbol-value 'org-startup-indented)))
          (should (equal after-load-alist after-load))
          (should (equal custom-theme-load-path search-path)))))))

(desktop-theme-test desktop-theme-active-resolves-one-runtime-snapshot
  (let* ((a (desktop-theme-test--runtime "#102030"))
         (b (desktop-theme-test--runtime "#304050"))
         (insert-file (symbol-function 'insert-file-contents)))
    (desktop-theme-test--active a)
    (cl-letf (((symbol-function 'insert-file-contents)
               (lambda (file &rest args)
                 (prog1 (apply insert-file file args)
                   (when (equal file (expand-file-name "emacs-theme" a))
                     (desktop-theme-test--active b))))))
      (trev/desktop-theme-load))
    (should (equal (face-background 'default nil t) "#102030"))
    (trev/desktop-theme-load)
    (should (equal (face-background 'default nil t) "#304050"))))

(desktop-theme-test desktop-theme-native-catalog-ignores-bundle-source
  (let ((poison (expand-file-name "native-poison-executed" "~/")))
    ;; Traverse both ways to cover light/dark switches, reloading each symbol.
    (dolist (theme (append modus-themes-items (reverse modus-themes-items)))
      (ert-info ((format "Native desktop theme: %s" theme))
        (let ((bundle (desktop-theme-test--bundle
                       (symbol-name theme) theme "#abcdef")))
          (desktop-theme-test--write
           (expand-file-name (format "%s-theme.el" theme) bundle)
           (format "(with-temp-file %S (insert \"executed\"))\n(error \"Bundle source shadowed native theme\")\n"
                   poison))
          (desktop-theme-test--select bundle)
          (desktop-theme-test--active "current")
          (dotimes (_ 2)
            (trev/desktop-theme-load)
            (should (equal custom-enabled-themes (list theme)))
            (should (equal (face-background 'default nil t)
                           (modus-themes-get-color-value 'bg-main t theme)))
            (should (equal (face-foreground 'default nil t)
                           (modus-themes-get-color-value 'fg-main t theme)))
            (should-not (file-exists-p poison))))))))

(desktop-theme-test desktop-theme-invalid-loads-preserve-precedence
  (let* ((low (desktop-theme-test--bundle "low" 'trev-ayu-dark "#102030"))
         (high (desktop-theme-test--bundle "high" 'trev-nord "#304050"))
         (known (desktop-theme-test--bundle "known" 'trev-ayu-light "#506070")))
    (let ((custom-theme-load-path (list low high known)))
      (load-theme 'trev-ayu-dark t)
      (load-theme 'trev-nord t)
      (load-theme 'trev-ayu-light t t))
    (let ((appearance (desktop-theme-test--appearance))
          (old-themes (copy-sequence custom-enabled-themes))
          (search-path (copy-sequence custom-theme-load-path)))
      (dolist (fault '(invalid missing partial active empty))
        (ert-info ((format "Broken theme: %s" fault))
          (let* ((target (pcase fault
                           ('active 'trev-nord)
                           ('empty 'trev-ayu-light)
                           (_ 'trev-ayu-mirage)))
                 (plist (copy-tree (symbol-plist target) t))
                 (known-p (and (custom-theme-p target) t))
                 (bundle (desktop-theme-test--bundle
                          (symbol-name fault) target "#abcdef"))
                 (source (expand-file-name (format "%s-theme.el" target) bundle)))
            (pcase fault
              ('invalid
               (desktop-theme-test--write (expand-file-name "emacs-theme" bundle)
                                          "not-an-allowed-theme"))
              ('missing (delete-file source))
              ('empty (desktop-theme-test--write source "; No theme definition.\n"))
              (_ (with-temp-buffer
                   (insert-file-contents source)
                   (goto-char (point-max))
                   (insert "\n(error \"partial theme evaluation\")\n")
                   (write-region (point-min) (point-max) source nil 'silent))))
            (desktop-theme-test--select bundle)
            (should-error (trev/desktop-theme-load))
            (should (equal custom-enabled-themes old-themes))
            (should (equal (desktop-theme-test--appearance) appearance))
            (should (equal (symbol-plist target) plist))
            (should (eq (and (custom-theme-p target) t) known-p))
            (should (equal custom-theme-load-path search-path))))))))

(desktop-theme-test desktop-theme-partial-enable-cleans-error-and-quit
  (let ((old (desktop-theme-test--bundle "old" 'trev-nord "#102030"))
        (target 'trev-ayu-dark))
    (desktop-theme-test--select old)
    (trev/desktop-theme-load)
    (let ((appearance (desktop-theme-test--appearance))
          (bundle (desktop-theme-test--bundle
                   "target" target "#aabbcc"
                   '((desktop-theme-test-only ((t :foreground "#ff00ff")))))))
      (desktop-theme-test--select bundle)
      (dolist (condition '(error quit))
        (let ((recalc (symbol-function 'custom-theme-recalc-face))
              caught applied registered fired)
          (cl-letf (((symbol-function 'custom-theme-recalc-face)
                     (lambda (face)
                       (funcall recalc face)
                       (when (and (eq face 'desktop-theme-test-only)
                                  (assq target (get face 'theme-face))
                                  (not fired))
                         (setq fired t
                               applied (face-foreground face nil t)
                               registered (memq target custom-enabled-themes))
                         (signal condition '("injected enable condition"))))))
            (condition-case failure
                (trev/desktop-theme-load)
              ((error quit) (setq caught failure))))
          (should (equal applied "#ff00ff"))
          (should-not registered)
          (should (equal caught (list condition "injected enable condition")))
          (should (equal custom-enabled-themes '(trev-nord)))
          (should-not (custom-theme-p target))
          (should-not (assq target (get 'desktop-theme-test-only 'theme-face)))
          (should (equal (desktop-theme-test--appearance) appearance)))))))

(desktop-theme-test desktop-theme-same-symbol-revisions-and-rollback
  (let* ((theme 'trev-ayu-dark)
         (original (face-foreground 'desktop-theme-test-only nil t))
         (a (desktop-theme-test--bundle
             "a" theme "#102030"
             '((desktop-theme-test-only ((t :foreground "#ff00ff"))))))
         (b (desktop-theme-test--bundle "b" theme "#304050"))
         (a2 (desktop-theme-test--bundle "a2" theme "#506070"))
         ;; An old global search path must not shadow the selected snapshot.
         (custom-theme-load-path (cons a custom-theme-load-path))
         (search-path (copy-sequence custom-theme-load-path)))
    (dolist (revision `((,a "#102030" "#ff00ff")
                        (,b "#304050" ,original)
                        (,a "#102030" "#ff00ff")
                        (,a2 "#506070" ,original)))
      (desktop-theme-test--select (car revision))
      (trev/desktop-theme-load)
      (should (equal (face-background 'default nil t) (cadr revision)))
      (should (equal (face-foreground 'desktop-theme-test-only nil t)
                     (caddr revision)))
      (should (equal custom-enabled-themes (list theme)))
      (should (equal custom-theme-load-path search-path)))))

(desktop-theme-test desktop-theme-resolves-one-bundle-snapshot
  (let* ((a (desktop-theme-test--bundle "a" 'trev-ayu-dark "#102030"))
         (b (desktop-theme-test--bundle "b" 'trev-ayu-dark "#304050"))
         (insert-file (symbol-function 'insert-file-contents)))
    (desktop-theme-test--select a)
    (desktop-theme-test--active "current")
    (cl-letf (((symbol-function 'insert-file-contents)
               (lambda (file &rest args)
                 (prog1 (apply insert-file file args)
                   (when (equal file (expand-file-name "emacs-theme" a))
                     (desktop-theme-test--select b))))))
      (trev/desktop-theme-load))
    (should (equal (face-background 'default nil t) "#102030"))
    (trev/desktop-theme-load)
    (should (equal (face-background 'default nil t) "#304050"))))

(desktop-theme-test desktop-theme-font-preferences-survive-palette-reloads
  ;; Apply the normal startup preferences before any palette is loaded.
  (trev/theme-set-fonts)
  (let ((baseline
         (cl-loop for (face . attributes)
                  in '((default :family :height :weight)
                       (fixed-pitch :family :height)
                       (fixed-pitch-serif :family :height)
                       (variable-pitch :family :weight))
                  append (cl-loop for attribute in attributes
                                  collect (list face attribute
                                                (face-attribute face attribute nil)))))
        (native (desktop-theme-test--bundle
                 "fonts-native" 'modus-vivendi-deuteranopia "#abcdef"))
        (generated (desktop-theme-test--bundle
                    "fonts-generated" 'trev-nord-light "#e5e9f0"))
        (runtime (desktop-theme-test--runtime
                  "#304050" "dark"
                  '((fixed-pitch ((t (:family "monospace"))))
                    (fixed-pitch-serif ((t (:family "monospace serif"))))
                    (variable-pitch ((t (:family "sans serif")))))))
        previous-theme previous-background)
    (dolist (selection `((,native modus-vivendi-deuteranopia)
                         (,native modus-vivendi-deuteranopia)
                         (,generated trev-nord-light)
                         (,generated trev-nord-light)
                         (,runtime noctalia)
                         (,runtime noctalia)
                         (,native modus-vivendi-deuteranopia)))
      (if (eq (cadr selection) 'noctalia)
          (desktop-theme-test--active (car selection))
        (desktop-theme-test--select (car selection))
        (desktop-theme-test--active "current"))
      (trev/desktop-theme-load)
      (should (equal custom-enabled-themes (list (cadr selection))))
      (dolist (font baseline)
        (cl-destructuring-bind (face attribute value) font
          (ert-info ((format "%s %s under %s" face attribute (cadr selection)))
            (should (equal (face-attribute face attribute nil) value)))))
      (let ((background (face-background 'default nil t)))
        (should (equal background
                       (pcase (cadr selection)
                         ('trev-nord-light "#e5e9f0")
                         ('noctalia "#304050")
                         (_ (modus-themes-get-color-value 'bg-main t)))))
        (when previous-theme
          (if (eq previous-theme (cadr selection))
              (should (equal background previous-background))
            (should-not (equal background previous-background))))
        (setq previous-theme (cadr selection)
              previous-background background)))))

(desktop-theme-test desktop-theme-semantic-faces-padding-and-late-declarations
  (let* ((modus-themes-common-palette-overrides '((bg-added-fringe "#1188ee")))
         (native (desktop-theme-test--bundle
                  "native" 'modus-vivendi-deuteranopia "#abcdef"))
         (base16 (desktop-theme-test--bundle "base16" 'trev-ayu-dark "#102030")))
    (desktop-theme-test--write
     (expand-file-name "modus-vivendi-deuteranopia-theme.el" native)
     "(error \"Native themes must use their installed search path\")")
    (desktop-theme-test--select native)
    (trev/desktop-theme-load)
    ;; These real declarations arrive after custom-set-faces recorded specs.
    (require 'diff-hl)
    (require 'erc)
    (require 'erc-match)
    (let ((native-colors
           (mapcar (lambda (color) (modus-themes-get-color-value color t))
                   '(bg-added-fringe bg-changed-fringe bg-removed-fringe))))
      (should (equal (car native-colors) "#1188ee"))
      (should-not (equal (caddr native-colors) (face-foreground 'error nil t)))
      (desktop-theme-test--markers native-colors)
      (spacious-padding-mode 1)
      (desktop-theme-test--markers native-colors)
      (spacious-padding-mode -1)
      (let ((custom-theme-load-path (list base16)))
        (load-theme 'trev-ayu-dark t))
      ;; A lower-precedence Modus theme must not supply the active colors.
      (desktop-theme-test--markers '("#00aa44" "#aa00dd" "#bb3300"))
      (disable-theme 'trev-ayu-dark)
      (desktop-theme-test--markers native-colors)
      (enable-theme 'trev-ayu-dark)
      (spacious-padding-mode 1)
      (desktop-theme-test--markers '("#00aa44" "#aa00dd" "#bb3300"))
      (spacious-padding-mode -1)
      (disable-theme 'trev-ayu-dark)
      (desktop-theme-test--markers native-colors))))

(desktop-theme-test desktop-theme-picker-is-asynchronous-and-reports-failure
  (let ((exec-path nil))
    (should-error (trev/desktop-theme-select) :type 'user-error))
  (let* ((program (expand-file-name "bin/theme-switch" "~/"))
         (shell (executable-find "sh"))
         (exec-path (list (file-name-directory program)))
         messages picker)
    (desktop-theme-test--write
     program (format "#!%s\nprintf 'picker-output\\n'\nexit 7\n" shell))
    (set-file-modes program #o700)
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (setq picker (trev/desktop-theme-select))
          ;; Even an already-exited child is returned as a process, not a
          ;; synchronous exit status.  Let its sentinel finish before reading.
          (should (processp picker))
          (with-timeout (5 (ert-fail "Theme picker did not report its exit"))
            (while (or (process-live-p picker) (not messages))
              (accept-process-output picker 0.1)))
          (should (= (process-exit-status picker) 7))
          (should (with-current-buffer (process-buffer picker)
                    (string-match-p "picker-output" (buffer-string))))
          (should (seq-some (lambda (text)
                              (string-match-p "\\*Desktop theme picker\\*" text))
                            messages)))
      (when (process-live-p picker) (delete-process picker)))))

(provide 'desktop-theme-tests)
;;; desktop-theme-tests.el ends here
