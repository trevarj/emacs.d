;;; desktop-theme-tests.el --- Shared desktop theme regressions -*- lexical-binding: t; -*-

;;; Commentary:
;; Run: emacs -Q --batch -l lisp/desktop-theme-tests.el -f ert-run-tests-batch-and-exit
;; Each test gets a fresh Emacs and disposable HOME.  Only the real Modus
;; use-package form's :preface and :config are evaluated, never the full init.
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
          (when (memq section '(:preface :config))
            (eval item t))))))
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
      (trev/desktop-theme-initialize)
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
        (require 'server)
        (let ((server-name "desktop-theme-test")
              (server-socket-dir (expand-file-name "server" "~/"))
              (server-auth-dir (expand-file-name "auth" "~/"))
              (server-use-tcp nil))
          (unwind-protect
              (progn
                (trev/desktop-theme-start-server)
                (should (server-running-p server-name))
                (let ((started server-process))
                  (trev/desktop-theme-start-server)
                  (should (eq started server-process))))
            (when (process-live-p server-process) (delete-process server-process))))
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
    (cl-letf (((symbol-function 'insert-file-contents)
               (lambda (file &rest args)
                 (prog1 (apply insert-file file args)
                   (when (equal file (expand-file-name "emacs-theme" a))
                     (desktop-theme-test--select b))))))
      (trev/desktop-theme-load))
    (should (equal (face-background 'default nil t) "#102030"))
    (trev/desktop-theme-load)
    (should (equal (face-background 'default nil t) "#304050"))))

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
