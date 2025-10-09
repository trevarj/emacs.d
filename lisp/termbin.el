;;; termbin.el --- Helper for interacting with termbin  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Trevor Arjeski

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

;; Helper to send selected region to termbin

;;; Code:

;;;###autoload
(defun termbin-send-region (start end)
  "Send selected region to termbin command.
Write the selected region from START to END to a temp file and pipe it
to the termbin command.  Display the output when done and save it to
the kill ring."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (temp-file (make-temp-file "tb-input-"))
         (process-name "tb-process")
         (buffer-name "*tb-output*"))
    (with-temp-file temp-file
      (insert region-text))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (make-process
     :name process-name
     :buffer buffer-name
     :command (list shell-file-name shell-command-switch
                    (format "cat %s | nc termbin.com 9999" (shell-quote-argument temp-file)))
     :noquery t
     :sentinel
     (lambda (proc event)
       (when (string= event "finished\n")
         (with-current-buffer (process-buffer proc)
           (let ((output (string-trim-right (buffer-string) "[ \n\r\x00]+")))
             (kill-new output)
             (if (< (length output) 1000)
                 (message "tb link: %s" output)
               (message "tb output saved to kill ring (too long to display)"))))
         ;; Cleanup temp file
         (delete-file temp-file))))))

(provide 'termbin)
;;; termbin.el ends here
