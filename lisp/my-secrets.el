;;; my-secrets.el --- My Secrets                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp
;; Package-Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A wrapper package for my secrets file.

;;; Code:

(require 'epa-file)

(defconst my-secrets-file
  (expand-file-name "secrets.el.gpg" user-emacs-directory)
  "Encrypted file containing private local configuration.")

(defun my-secrets-file-p (file)
  "Return non-nil when FILE names `my-secrets-file'."
  (and file (file-equal-p (expand-file-name file) my-secrets-file)))

(defun my-secrets-protect-buffer ()
  "Keep encrypted secrets buffers from writing companion state files."
  (when (my-secrets-file-p buffer-file-name)
    ;; Let epa-file own the encrypted write; do not create additional encrypted
    ;; auto-save or backup files for private configuration.
    (setq-local auto-save-default nil)
    (setq-local backup-inhibited t)))

(defun my-secrets-edit ()
  "Open `my-secrets-file' for editing through `epa-file'."
  (interactive)
  (find-file my-secrets-file))

(epa-file-enable)
(add-hook 'find-file-hook #'my-secrets-protect-buffer)

(load my-secrets-file nil t)

(provide 'my-secrets)
;;; my-secrets.el ends here
