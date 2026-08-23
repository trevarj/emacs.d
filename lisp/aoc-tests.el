;;; aoc-tests.el --- Tests for aoc.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L lisp -l lisp/aoc-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'aoc)

(ert-deftest aoc-response-body-strips-headers ()
  "The body starts after the first blank line, CRLF or LF."
  (dolist (sep '("\r\n\r\n" "\n\n"))
    (with-temp-buffer
      (insert "HTTP/1.1 200 OK" sep "one\ntwo\n")
      (should (equal (aoc--response-body) "one\ntwo\n"))))
  ;; No header block at all: return everything rather than nothing.
  (with-temp-buffer
    (insert "raw")
    (should (equal (aoc--response-body) "raw"))))

(ert-deftest aoc-increment-day-level-walks-the-calendar ()
  (let ((aoc-day-level '(1 . 1)))
    (should (equal (aoc--increment-day-level) '(1 . 2)))
    (should (equal (aoc--increment-day-level) '(2 . 1)))
    (setq aoc-day-level '(25 . 2))
    (should (equal (aoc--increment-day-level) '(25 . 2)))))

(provide 'aoc-tests)
;;; aoc-tests.el ends here
