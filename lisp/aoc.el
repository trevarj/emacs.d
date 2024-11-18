;;; aoc.el --- Advent of Code utility functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: lisp
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.4") (request "20230127.417"))

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

;; Built-ins
(require 'cl-lib)
(require 'shr)

;; External
(require 'request)

(defgroup aoc nil
  "Customization variables for Advent of Code (aoc.el).")

(defcustom aoc-session-cookie ""
  "The session cookie found within the browser on adventofcode.com after you
login."
  :group 'aoc
  :type 'string)

(defcustom aoc-input-directory "inputs/"
  "The relative path to store input files."
  :group 'aoc
  :type 'directory)

(defcustom aoc-year (string-to-number
                     (format-time-string "%Y"))
  "The year of AoC being worked on. Defaults to current year."
  :group 'aoc
  :type 'number)

(defcustom aoc-day-level '(1 . 1)
  "The day and level of AoC being worked on. Increments on correct solution."
  :group 'aoc
  :type '(cons number number))

(cl-defun aoc--fetch-input-error (&key status &allow-other-keys)
  (message "Could not fetch input file: %s" status))

(defun aoc--increment-day-level ()
  "Increment the `aoc-day-level' pair."
  (setf aoc-day-level
        (pcase aoc-day-level
          (`(25 . 2) (cons 25 2))
          (`(,day . 2) (cons (1+ day) 1))
          (`(,day . 1) (cons day 2)))))

(defun aoc--render-data-to-help-buffer (data buffer)
  "Render HTML data (if non-nil) and open it in a help buffer."
  (when data
    (with-temp-buffer
      (insert data)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (with-help-window buffer
          (shr-insert-document dom)
          (pop-to-buffer (current-buffer)))))))

(cl-defun aoc--check-submit-response (&key data &allow-other-keys)
  (cond
   ((cl-search "That's not the right answer." data)
    (message "Incorrect answer."))
   ((cl-search "You gave an answer too recently" data)
    (message "Submitted too recently. Please wait."))
   (t
    (aoc--increment-day-level)
    (aoc--render-data-to-help-buffer data "*aoc submission*")
    (message "Answer submitted successfully."))))

(cl-defun aoc--submit-error (&key status &allow-other-keys)
  (message "Could not submit answer: %s" status))

;;;###autoload
(defun aoc-fetch-input (year day &optional force)
  "Fetches input file for given year and day."
  (interactive (list (read-number "Year: " aoc-year)
                     (read-number "Day: " (car aoc-day-level))))
  (if (string-empty-p aoc-session-cookie)
      (message "`aoc-session-cookie' not set.")
    (let ((input-file-path
           (expand-file-name (format "%d.txt" day) aoc-input-directory)))
      (if (and (file-exists-p input-file-path)
               (not force))
          (message "Input file for day %d already exists at %s" day input-file-path)
        (unless (file-directory-p aoc-input-directory)
          (make-directory aoc-input-directory))
        (request (format "https://adventofcode.com/%d/day/%d/input" year day)
          :headers `(("Cookie" . ,(format "session=%s" aoc-session-cookie)))
          :error #'aoc--fetch-input-error
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (with-temp-file input-file-path
                        (insert data))
                      (message "Wrote day %d input file: %s" day input-file-path))))))))

;;;###autoload
(defun aoc-view-problem (year day)
  "View AoC problem for given year and day."
  (interactive (list (read-number "Year: " aoc-year)
                     (read-number "Day: " (car aoc-day-level))))
  (request (format "https://adventofcode.com/%d/day/%d" year day)
    :headers `(("Cookie" . ,(format "session=%s" aoc-session-cookie)))
    :parse 'buffer-string
    :complete (cl-function
               (lambda (&key data &allow-other-keys)
                 (aoc--render-data-to-help-buffer
                  data (format "*AoC Day %d Year %d" day year))))))

;;;###autoload
(defun aoc-submit-answer (year day level answer)
  "Submit a solution for given year, day and level."
  (interactive (list (read-number "Year: " aoc-year)
                     (read-number "Day: " (car aoc-day-level))
                     (read-number "Level: " (cadr aoc-day-level))
                     (read-string "Answer: ")))
  (when (and (not (and (eq year aoc-year)
                       (equal '(day level) aoc-day-level)))
             (yes-or-no-p
              (format "Set current problem to %d, Day %d Level %d?"
                      year day level)))
    (setopt aoc-year year
            aoc-day-level (cons day level)))
  (request (format "https://adventofcode.com/%d/day/%d/answer" year day)
    :type "POST"
    :headers `(("Cookie" . ,(format "session=%s" aoc-session-cookie)))
    :data `(("level" . ,level) ("answer" . ,answer))
    :parse 'buffer-string
    :error #'aoc--submit-error
    :success #'aoc--check-submit-response))

(provide 'aoc)
;;; aoc.el ends here
