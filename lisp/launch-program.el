;;; launch-program.el --- Launch a program from Emacs using gio  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp
;; Package-Version: 1.0.0
;; Package-Requires: ((s "20220902.1511") (consult "20241115.517"))

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

;; Uses ripgrep to find all freedesktop .desktop files and present a list of
;; programs to the user, and then launches the selected program.

;;; Code:

(require 'consult)
(require 's)

(defconst regex-string "^\\[Desktop Entry\\](?:\\s|.)*?^Name=(.*)\\n(?:\\s|.)*?^Exec=.*"
  "The regex string that parses a desktop file for the name and exec patch of a program.")

(defconst rg-cmd (format "rg --multiline --no-heading --no-line-number -m 1 '%s' -or '$1' -g '*.desktop' '/usr' '/opt' 2>/dev/null" regex-string)
  "The ripgrep command to parse all desktop files in /usr and /opt.")

(defun parse-result (line)
  (reverse (s-split-up-to ":" line 1)))

(defun launch-program--launch-desktop-file (file-path)
  "Helper that actually launches desktop file."
  (call-process-shell-command
   (format "setsid gio launch %s &" file-path) nil 0 nil))

;;;###autoload
(defun launch-program-launch ()
  "Find and launch a program outside of Emacs."
  (interactive)
  (let ((apps (mapcar 'parse-result
                      (s-split "\n" (shell-command-to-string rg-cmd) t))))
    (launch-program--launch-desktop-file
     (cadr (assoc-string (consult--read (mapcar 'car apps)
                                        :prompt "Launch a program: ") apps)))))

(provide 'launch-program)
;;; launch-program.el ends here
