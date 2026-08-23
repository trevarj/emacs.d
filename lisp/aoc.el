;;; aoc.el --- Advent of Code utility functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.4"))

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

;; It is recommended to use savehist's `savehist-additional-variables' to
;; preserve `aoc-year' and `aoc-day-level' across Emacs restarts.

;; Advent of Code utilities to fetch and save problems and also submit an
;; answer.

;;; Code:

(require 'cl-lib)
(require 'shr)
(require 'url)
(require 'url-cookie)

(defgroup aoc nil
  "Customization variables for Advent of Code (aoc.el).")

(defcustom aoc-session-cookie ""
  "The session cookie found within the browser on adventofcode.com."
  :group 'aoc
  :type 'string)

(defcustom aoc-input-directory "inputs/"
  "The relative path to store input files."
  :group 'aoc
  :type 'directory)

(defcustom aoc-year (string-to-number
                     (format-time-string "%Y"))
  "The year of AoC being worked on.
Defaults to current year."
  :group 'aoc
  :type 'number)

(defcustom aoc-day-level '(1 . 1)
  "The day and level of AoC being worked on.
Increments on correct solution."
  :group 'aoc
  :type '(cons number number))

(defun aoc--require-session-cookie ()
  "Load secrets and require `aoc-session-cookie' to be set."
  (require 'my-secrets)
  (when (string-empty-p aoc-session-cookie)
    (user-error "`aoc-session-cookie' is not set")))

(defun aoc--response-body ()
  "Return the decoded body of the current `url-retrieve' response buffer."
  (goto-char (point-min))
  (decode-coding-string
   (buffer-substring-no-properties
    (if (re-search-forward "\r?\n\r?\n" nil t) (point) (point-min))
    (point-max))
   'utf-8))

(defun aoc--request (url callback &optional form)
  "Fetch URL with the AoC session cookie, calling CALLBACK with the body.
With FORM, an alist of string pairs, POST it instead of issuing a GET."
  (let ((url-request-method (if form "POST" "GET"))
        (url-request-data
         (and form (url-build-query-string
                    (mapcar (lambda (kv) (list (car kv) (cdr kv))) form))))
        (url-request-extra-headers
         `(("Cookie" . ,(format "session=%s" aoc-session-cookie))
           ("User-Agent" . "trevarj/emacs.d/lisp/aoc.el")
           ,@(and form
                  '(("Content-Type" . "application/x-www-form-urlencoded"))))))
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (if-let* ((err (plist-get status :error)))
               (message "AoC request failed: %s" err)
             (funcall callback (aoc--response-body)))
         (kill-buffer (current-buffer))))
     nil t)))

(defun aoc--increment-day-level ()
  "Increment the `aoc-day-level' pair."
  (setf aoc-day-level
        (pcase aoc-day-level
          (`(25 . 2) (cons 25 2))
          (`(,day . 2) (cons (1+ day) 1))
          (`(,day . 1) (cons day 2)))))

(defun aoc--render-data-to-help-buffer (data buffer)
  "Render HTML DATA (if non-nil) and open it in a help BUFFER."
  (when data
    (with-temp-buffer
      (insert data)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (with-help-window buffer
          (with-current-buffer standard-output
            (shr-insert-document dom)
            (pop-to-buffer (current-buffer))))))))

(defun aoc--check-submit-response (data)
  "Checks DATA for the success phrase."
  (cond
   ((cl-search "That's not the right answer" data)
    (message "Incorrect answer. : %s" data))
   ((cl-search "You gave an answer too recently" data)
    (message "Submitted too recently. Please wait. %s" data))
   ((cl-search "That's the right answer!" data)
    (aoc--increment-day-level)
    (message "Answer submitted successfully."))
   (t (message "Submittion failed.")))
  (aoc--render-data-to-help-buffer data "*aoc submission*"))

;;;###autoload
(defun aoc-fetch-input (year day &optional force)
  "Fetches input file for given YEAR and DAY.
Can provide FORCE to overwrite existing file."
  (interactive (list (read-number "Year: " aoc-year)
                     (read-number "Day: " (car aoc-day-level))))
  (aoc--require-session-cookie)
  (let ((input-file-path
         (expand-file-name (format "%d.txt" day) aoc-input-directory)))
    (if (and (file-exists-p input-file-path)
             (not force))
        (message "Input file for day %d already exists at %s" day input-file-path)
      (unless (file-directory-p aoc-input-directory)
        (make-directory aoc-input-directory t))
      (aoc--request
       (format "https://adventofcode.com/%d/day/%d/input" year day)
       (lambda (data)
         (with-temp-file input-file-path
           (insert data))
         (message "Wrote day %d input file: %s" day input-file-path))))))

;;;###autoload
(defun aoc-view-problem (year day)
  "View AoC problem for given YEAR and DAY."
  (interactive (list (read-number "Year: " aoc-year)
                     (read-number "Day: " (car aoc-day-level))))
  (aoc--require-session-cookie)
  (unless (assoc ".adventofcode.com" url-cookie-secure-storage)
    (url-cookie-store "session" aoc-session-cookie nil ".adventofcode.com" "/" t)  )
  (eww (format "https://adventofcode.com/%d/day/%d" year day)))

;;;###autoload
(defun aoc-submit-answer (year day level answer)
  "Submit an ANSWER for LEVEL of given YEAR, DAY."
  (interactive (list (read-number "Year: " aoc-year)
                     (read-number "Day: " (car aoc-day-level))
                     (read-number "Level: " (cdr aoc-day-level))
                     (read-string "Answer: ")))
  (aoc--require-session-cookie)
  (when (and (not (and (eq year aoc-year)
                       (equal `(,day . ,level) aoc-day-level)))
             (yes-or-no-p
              (format "Set current problem to %d, Day %d Level %d?"
                      year day level)))
    (setopt aoc-year year
            aoc-day-level (cons day level)))
  (aoc--request
   (format "https://adventofcode.com/%d/day/%d/answer" year day)
   #'aoc--check-submit-response
   `(("level" . ,(number-to-string level))
     ("answer" . ,answer))))

(provide 'aoc)
;;; aoc.el ends here
