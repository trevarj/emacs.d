;;; erc-links.el --- Inline page-title hints for links in ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: comm, hypermedia
;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

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

;; Append fetched page titles inline after http(s) links posted in ERC
;; buffers.  When a message containing a URL is inserted, the page's
;; <title> (or og:title) is fetched asynchronously and appended right
;; after the link in a dimmed, italic face:
;;
;;     <alice> check this out https://example.com/x [Example Domain]
;;
;; Enable with `M-x erc-links-mode' (a global minor mode), or from your
;; init file:
;;
;;     (use-package erc-links
;;       :ensure nil
;;       :after erc
;;       :config (erc-links-mode))
;;
;; Only the document <head> is parsed, so even multi-megabyte pages stay
;; cheap.  Historical messages replayed by a bouncer on connect are
;; titled too.
;;
;; Fetched titles are cached to `erc-links-cache-file', which defaults to
;; a path under `temporary-file-directory' (typically /tmp).  The cache
;; survives Emacs restarts but is discarded when the machine reboots, so
;; each URL is fetched from the network at most once per uptime — bouncer
;; backlog replayed across sessions is served from disk.  Only successful
;; lookups are persisted; failures retry next session.

;;; Code:

(require 'erc)
(require 'erc-button)
(require 'dom)
(require 'seq)

(defgroup erc-links nil
  "Append fetched page titles after links posted in ERC."
  :group 'erc
  :prefix "erc-links-")

(defcustom erc-links-cache-ttl (* 7 24 60 60)
  "Seconds before a cached link title is considered stale.
Defaults to one week.  Because the on-disk cache lives in a temporary
directory that is cleared on reboot (see `erc-links-cache-file'), a long
TTL keeps bouncer backlog served from disk for the whole machine uptime
while still refreshing titles that change over time."
  :type 'natnum)

(defcustom erc-links-cache-file
  (expand-file-name "erc-links-cache.eld" temporary-file-directory)
  "File where fetched link titles are cached between sessions.
Defaults to a path under `temporary-file-directory' (typically /tmp), so
the cache is discarded when the machine reboots.  Set to nil to disable
on-disk caching entirely."
  :type '(choice file (const :tag "No persistence" nil)))

(defcustom erc-links-head-bytes 65536
  "Bytes of the response head scanned for a title when </head> is absent."
  :type 'natnum)

(defcustom erc-links-max-length 200
  "Maximum length of a displayed link title."
  :type 'natnum)

(defcustom erc-links-fill-column nil
  "Column to wrap appended title hints at.
When nil, fall back to the buffer's `fill-column' (or 80).  A hint that
would push a line past this column is wrapped, with continuation lines
indented to align under the message body (see
`erc-links-continuation-indent')."
  :type '(choice (const :tag "Follow buffer fill-column" nil) natnum))

(defcustom erc-links-continuation-indent nil
  "Column to indent wrapped continuation lines of a title hint to.
When nil, derive it from ERC's fill settings: `erc-fill-static-center'
under `erc-fill-static', otherwise no indent."
  :type '(choice (const :tag "Derive from ERC fill" nil) natnum))

(defcustom erc-links-user-agent
  "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0"
  "User-Agent header sent when fetching a page title.
Many sites gate their markup on a browser-like User-Agent."
  :type 'string)

(defface erc-links-face
  '((t :inherit shadow :slant italic))
  "Face for inline page-title hints appended after links in ERC.")

(defvar erc-links--cache (make-hash-table :test 'equal)
  "Hash table mapping URL to (TIMESTAMP . TITLE) of fetched link titles.")

(defvar erc-links--save-timer nil
  "Idle timer debouncing writes of the cache to `erc-links-cache-file'.")

(defun erc-links--fool-message-p ()
  "Return non-nil when the current ERC insertion is from a fool.
`erc-hide-fools' records fool matches in `erc--msg-props' before ERC
applies any later hidden-text behavior."
  (and (boundp 'erc--msg-props)
       (hash-table-p erc--msg-props)
       (eq (gethash 'erc--invisible erc--msg-props) 'erc-match-fool)))

(defun erc-links--load-cache ()
  "Populate `erc-links--cache' from `erc-links-cache-file' if present."
  (when (and erc-links-cache-file (file-readable-p erc-links-cache-file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents erc-links-cache-file)
        (let ((data (read (current-buffer))))
          (when (hash-table-p data)
            (setq erc-links--cache data)))))))

(defun erc-links--save-cache ()
  "Write successful cache entries to `erc-links-cache-file'.
Entries with an empty title (failed or title-less fetches) are not
persisted, so they are retried in a later session."
  (when erc-links--save-timer
    (cancel-timer erc-links--save-timer)
    (setq erc-links--save-timer nil))
  (when erc-links-cache-file
    (let ((keep (make-hash-table :test 'equal)))
      (maphash (lambda (url entry)
                 (when (and (cdr entry) (not (string-empty-p (cdr entry))))
                   (puthash url entry keep)))
               erc-links--cache)
      (ignore-errors
        (with-temp-file erc-links-cache-file
          (let ((print-length nil) (print-level nil))
            (prin1 keep (current-buffer))))))))

(defun erc-links--schedule-save ()
  "Arrange for the cache to be flushed to disk once Emacs goes idle."
  (unless (or erc-links--save-timer (null erc-links-cache-file))
    (setq erc-links--save-timer
          (run-with-idle-timer 5 nil #'erc-links--save-cache))))

;;;###autoload
(defun erc-links-clear-cache ()
  "Forget all cached link titles, in memory and on disk."
  (interactive)
  (clrhash erc-links--cache)
  (when (and erc-links-cache-file (file-exists-p erc-links-cache-file))
    (ignore-errors (delete-file erc-links-cache-file))))

(defun erc-links--clean (title)
  "Collapse whitespace, strip control chars, and truncate TITLE."
  (let ((s (string-trim
            (replace-regexp-in-string "[[:space:][:cntrl:]]+" " " title))))
    (if (> (length s) erc-links-max-length)
        (concat (substring s 0 erc-links-max-length) "…")
      s)))

(defun erc-links--og-title (dom)
  "Return the og:title/twitter:title meta content from DOM, or nil."
  (seq-some
   (lambda (m)
     (when (member (or (dom-attr m 'property) (dom-attr m 'name))
                   '("og:title" "twitter:title"))
       (dom-attr m 'content)))
   (dom-by-tag dom 'meta)))

(defun erc-links--from-html (beg end)
  "Return the cleaned page title from HTML between BEG and END, or nil.
Only the document <head> is parsed so large pages stay cheap; falls back
to the og:title/twitter:title meta tags when <title> is absent."
  (let* ((head-end
          (save-excursion
            (goto-char beg)
            (if (re-search-forward "</head>" end t)
                (point)
              (min end (+ beg erc-links-head-bytes)))))
         (title
          (if (fboundp 'libxml-parse-html-region)
              (when-let* ((dom (libxml-parse-html-region beg head-end)))
                (let* ((node (car (dom-by-tag dom 'title)))
                       (ttext (and node (dom-text node))))
                  (or (and ttext (not (string-empty-p (string-trim ttext)))
                           ttext)
                      (erc-links--og-title dom))))
            (save-excursion
              (goto-char beg)
              (when (re-search-forward
                     "<title[^>]*>\\(\\(?:.\\|\n\\)*?\\)</title>" head-end t)
                (match-string 1))))))
    (when (and title (not (string-empty-p (string-trim title))))
      (erc-links--clean title))))

(defun erc-links--fill-column ()
  "Column at which to wrap appended title hints."
  (or erc-links-fill-column
      (and (natnump fill-column) fill-column)
      80))

(defun erc-links--continuation-indent ()
  "Indent (a column number) for wrapped continuation lines of a hint.
Aligns under the ERC message body.  Under `erc-fill-static' that is
`erc-fill-static-center' plus the left-timestamp width, since
`erc-fill-static' rigidly indents continuation lines by the timestamp
offset on top of the center column."
  (or erc-links-continuation-indent
      (and (eq erc-fill-function 'erc-fill-static)
           (boundp 'erc-fill-static-center)
           (+ erc-fill-static-center
              (if (fboundp 'erc-timestamp-offset) (erc-timestamp-offset) 0)))
      0))

(defun erc-links--wrap (text start-col)
  "Word-wrap TEXT for insertion beginning at column START-COL.
Lines are broken on whitespace so none exceeds `erc-links--fill-column';
continuation lines are prefixed with `erc-links--continuation-indent'
spaces so they align under the ERC message body.  A word longer than the
available width is left whole rather than split."
  (let* ((fill (erc-links--fill-column))
         (indent (make-string (erc-links--continuation-indent) ?\s))
         (col start-col)
         (parts nil))
    (dolist (word (split-string text nil t))
      ;; +1 for the space that would join this word to the current line.
      (if (and (> col (length indent))
               (> (+ col 1 (length word)) fill))
          (progn                        ; wrap: newline + indent, no leading space
            (push "\n" parts) (push indent parts) (push word parts)
            (setq col (+ (length indent) (length word))))
        (push " " parts) (push word parts) ; same line: separating space + word
        (setq col (+ col 1 (length word)))))
    (apply #'concat (nreverse parts))))

(defun erc-links--insert (buffer marker title)
  "Append TITLE as a dimmed, line-filled hint at MARKER in BUFFER."
  (when (and (buffer-live-p buffer) (markerp marker) (marker-buffer marker))
    (with-current-buffer buffer
      (save-excursion
        (goto-char marker)
        (let ((inhibit-read-only t))
          (insert (propertize (erc-links--wrap (concat "[" title "]")
                                               (current-column))
                              'face 'erc-links-face
                              'rear-nonsticky t)))))))

(defun erc-links--fetch (url buffer marker)
  "Asynchronously fetch URL and append its page title at MARKER in BUFFER."
  (require 'url)
  (ignore-errors
    (let* ((url-user-agent erc-links-user-agent)
           (work (url-retrieve
                  url
                  (lambda (status)
                    (let ((response (current-buffer)))
                      (unwind-protect
                          (condition-case nil
                              (unless (plist-get status :error)
                                (goto-char (point-min))
                                (let* ((headers-end
                                        (if (re-search-forward "\r?\n\r?\n" nil t)
                                            (point) (point-max)))
                                       (ctype
                                        (progn
                                          (goto-char (point-min))
                                          (when (re-search-forward
                                                 "^Content-Type:[ \t]*\\(.*\\)$" headers-end t)
                                            (downcase (string-trim (match-string 1)))))))
                                  ;; Only parse the head, so multi-MB pages stay cheap.
                                  (when (and ctype (string-match-p "html" ctype))
                                    (let ((title (erc-links--from-html
                                                  headers-end (point-max))))
                                      (puthash url (cons (current-time) (or title ""))
                                               erc-links--cache)
                                      (when (and title (not (string-empty-p title)))
                                        (erc-links--schedule-save)
                                        (erc-links--insert buffer marker title))))))
                            (error nil))
                        (when (buffer-live-p response) (kill-buffer response)))))
                  nil t t)))
      ;; These background fetches must never block on a prompt.  url handles
      ;; 401 responses in its async filter (outside this dynamic extent) by
      ;; decrypting `auth-sources' and asking for a username in the
      ;; minibuffer — and that auth machinery is what spams *Messages*.
      ;; Disabling auth in the work buffer makes such responses get dropped
      ;; quietly instead.  (`url-retrieve' SILENT already mutes progress.)
      (when (buffer-live-p work)
        (with-current-buffer work
          (setq-local url-registered-auth-schemes nil))))))

(defun erc-links--urls-in-region (beg end)
  "Return a list of (URL . END-MARKER) for http(s) URLs between BEG and END."
  (let (urls)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward erc-button-url-regexp end t)
        (let ((url (match-string-no-properties 0))
              (url-end (match-end 0)))
          (when (string-match-p "\\`https?://" url)
            (push (cons url (copy-marker url-end t)) urls)))))
    (nreverse urls)))

(defun erc-links-insert-hints ()
  "Append page-title hints after http(s) links in the inserted message.
Intended for `erc-insert-modify-hook'."
  (when (and (derived-mode-p 'erc-mode)
             (erc-default-target)
             (not (erc-links--fool-message-p)))
    (dolist (item (erc-links--urls-in-region (point-min) (point-max)))
      (let* ((url (car item))
             (marker (cdr item))
             (cached (gethash url erc-links--cache)))
        (if (and cached
                 (cdr cached)
                 (not (string-empty-p (cdr cached)))
                 (<= (float-time (time-subtract nil (car cached)))
                     erc-links-cache-ttl))
            (erc-links--insert (current-buffer) marker (cdr cached))
          (erc-links--fetch url (current-buffer) marker))))))

;;;###autoload
(define-minor-mode erc-links-mode
  "Global minor mode appending fetched page titles after links in ERC."
  :global t
  :group 'erc-links
  (if erc-links-mode
      (progn
        (erc-links--load-cache)
        ;; Run late, after ERC's own buttonizing of URLs.
        (add-hook 'erc-insert-modify-hook #'erc-links-insert-hints 80)
        (add-hook 'kill-emacs-hook #'erc-links--save-cache))
    (remove-hook 'erc-insert-modify-hook #'erc-links-insert-hints)
    (remove-hook 'kill-emacs-hook #'erc-links--save-cache)
    (erc-links--save-cache)))

(provide 'erc-links)
;;; erc-links.el ends here
