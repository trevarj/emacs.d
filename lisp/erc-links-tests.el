;;; erc-links-tests.el --- Tests for erc-links  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Trevor Arjeski

;; Author: Trevor Arjeski <tmarjeski@gmail.com>
;; Keywords: comm, hypermedia

;;; Commentary:

;; ERT tests for `erc-links'.  The default suite is deterministic and
;; offline: title extraction is exercised against HTML fixtures modelled
;; on every link type seen in real channel traffic (plain <title>,
;; entity-encoded, whitespace, "| WIRED" / "· GitHub" suffixes, og:title
;; and twitter:title fallbacks, oversized bodies, …), and URL scanning is
;; exercised against the literal message lines those links appeared in.
;;
;; A second, opt-in suite actually fetches the real URLs to confirm a
;; non-empty title comes back today.  It is skipped unless the
;; environment variable ERC_LINKS_LIVE is set, e.g.:
;;
;;     ERC_LINKS_LIVE=1 emacs -Q --batch -L lisp \
;;       -l ert -l erc-links-tests -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'erc-links)

(defun erc-links-tests--title (html)
  "Return the title `erc-links' extracts from the HTML string HTML."
  (with-temp-buffer
    (insert html)
    (erc-links--from-html (point-min) (point-max))))

(defun erc-links-tests--urls (line)
  "Return the list of http(s) URLs `erc-links' scans out of LINE."
  (with-temp-buffer
    (insert line)
    (mapcar #'car (erc-links--urls-in-region (point-min) (point-max)))))


;;;; Title extraction — one case per link type from the log.

(ert-deftest erc-links-test-title-plain ()
  "Simple <title> (systemcrafters.net, commodore.net, roman.pt, freebsd)."
  (should (equal (erc-links-tests--title
                  "<html><head><title>Welcome! - System Crafters</title>\
</head><body>x</body></html>")
                 "Welcome! - System Crafters")))

(ert-deftest erc-links-test-title-github-suffix ()
  "GitHub repo pages carry a \" · GitHub\" suffix."
  (should (equal (erc-links-tests--title
                  "<head><title>GitHub - parenworks/clatter.el: \
An IRCv3-compliant IRC client for Emacs · GitHub</title></head>")
                 "GitHub - parenworks/clatter.el: \
An IRCv3-compliant IRC client for Emacs · GitHub")))

(ert-deftest erc-links-test-title-wikipedia ()
  "Wikipedia article title."
  (should (equal (erc-links-tests--title
                  "<head><title>Nika riots - Wikipedia</title></head>")
                 "Nika riots - Wikipedia")))

(ert-deftest erc-links-test-title-pipe-suffix ()
  "Pipe-delimited suffix (WIRED, KUVRD, Tom's Hardware)."
  (should (equal (erc-links-tests--title
                  "<head><title>All the Ways Europe Is Ditching American \
Technology | WIRED</title></head>")
                 "All the Ways Europe Is Ditching American Technology | WIRED")))

(ert-deftest erc-links-test-title-named-entities ()
  "Named HTML entities are decoded (&amp;, &mdash;)."
  (should (equal (erc-links-tests--title
                  "<head><title>Jordanian Red Keffiyeh &mdash; Authentic \
Bedouin Heritage | KUVRD</title></head>")
                 "Jordanian Red Keffiyeh — Authentic Bedouin Heritage | KUVRD"))
  (should (equal (erc-links-tests--title
                  "<head><title>GPL issues &amp; more - MobileRead Forums\
</title></head>")
                 "GPL issues & more - MobileRead Forums")))

(ert-deftest erc-links-test-title-numeric-entity ()
  "Numeric HTML entities are decoded (&#8212; em dash)."
  (should (equal (erc-links-tests--title
                  "<head><title>A backdoor in a LinkedIn job offer &#8212; \
Roman Imankulov</title></head>")
                 "A backdoor in a LinkedIn job offer — Roman Imankulov")))

(ert-deftest erc-links-test-title-whitespace-collapsed ()
  "Leading/trailing/interior whitespace and newlines collapse to one space."
  (should (equal (erc-links-tests--title
                  "<head><title>\n   Why a Flip Phone?\n\t- Commodore   \
</title></head>")
                 "Why a Flip Phone? - Commodore")))

(ert-deftest erc-links-test-title-unicode-emoji ()
  "Titles containing emoji/unicode survive intact (Piaille cat post)."
  (should (equal (erc-links-tests--title
                  "<head><title>Cat Every Hour : « ✨ c a t ✨ »\
</title></head>")
                 "Cat Every Hour : « ✨ c a t ✨ »")))


;;;; og:title / twitter:title fallbacks (mastodon instances, JS apps).

(ert-deftest erc-links-test-og-title-fallback ()
  "Falls back to og:title when there is no <title> (grapheneos.social)."
  (should (equal (erc-links-tests--title
                  "<head><meta property=\"og:title\" \
content=\"GrapheneOS: &quot;Today is the official release day&quot;\">\
</head>")
                 "GrapheneOS: \"Today is the official release day\"")))

(ert-deftest erc-links-test-twitter-title-fallback ()
  "Falls back to twitter:title when present and no usable <title>."
  (should (equal (erc-links-tests--title
                  "<head><meta name=\"twitter:title\" \
content=\"FreeBSD Graphics Port Upgraded\"></head>")
                 "FreeBSD Graphics Port Upgraded")))

(ert-deftest erc-links-test-empty-title-uses-og ()
  "An empty <title> still falls back to og:title."
  (should (equal (erc-links-tests--title
                  "<head><title>   </title>\
<meta property=\"og:title\" content=\"Jolla Phone (Sep 2026)\"></head>")
                 "Jolla Phone (Sep 2026)")))

(ert-deftest erc-links-test-title-preferred-over-og ()
  "A real <title> wins over og:title."
  (should (equal (erc-links-tests--title
                  "<head><title>Real Title</title>\
<meta property=\"og:title\" content=\"OG Title\"></head>")
                 "Real Title")))


;;;; Robustness: large bodies, missing </head>, non-HTML, truncation.

(ert-deftest erc-links-test-large-body-head-only ()
  "Title in <head> is found even when the body is huge (github/youtube)."
  (let ((html (concat "<html><head><title>I Made Mortal Kombat with AI - \
YouTube</title></head><body>"
                      (make-string (* 1024 1024) ?x) ; ~1 MB body
                      "</body></html>")))
    (should (equal (erc-links-tests--title html)
                   "I Made Mortal Kombat with AI - YouTube"))))

(ert-deftest erc-links-test-no-head-close-within-window ()
  "Title near the top is found when </head> is absent but body is large."
  (let ((html (concat "<html><head><title>No Head Close</title>"
                      (make-string (* 200 1024) ?y)))) ; 200 KB, no </head>
    (should (equal (erc-links-tests--title html) "No Head Close"))))

(ert-deftest erc-links-test-non-html-returns-nil ()
  "Binary/non-HTML payloads (e.g. a .png) yield no title."
  (should (null (erc-links-tests--title
                 (concat "\x89PNG\r\n\x1a\n" (make-string 64 ?\0))))))

(ert-deftest erc-links-test-missing-title-returns-nil ()
  "HTML with no <title> and no og/twitter meta yields nil."
  (should (null (erc-links-tests--title
                 "<html><head><meta charset=\"utf-8\"></head><body>hi</body>\
</html>"))))

(ert-deftest erc-links-test-title-truncated ()
  "Over-long titles are truncated to `erc-links-max-length' plus an ellipsis."
  (let* ((erc-links-max-length 20)
         (out (erc-links-tests--title
               (format "<head><title>%s</title></head>" (make-string 200 ?a)))))
    (should (= (length out) 21))
    (should (string-suffix-p "…" out))))


;;;; --clean unit behaviour.

(ert-deftest erc-links-test-clean-whitespace ()
  (should (equal (erc-links--clean "  foo\n\t  bar  ") "foo bar")))

(ert-deftest erc-links-test-clean-truncate ()
  (let ((erc-links-max-length 5))
    (should (equal (erc-links--clean "abcdefghij") "abcde…"))))


;;;; URL scanning — over the literal message lines from the log.

(ert-deftest erc-links-test-scan-topic-two-urls ()
  "Two bare URLs separated by prose are both collected (topic line)."
  (should (equal
           (erc-links-tests--urls
            "Welcome to the SystemCrafters lounge - https://systemcrafters.net \
and https://systemcrafters.net/community - Daviwil")
           '("https://systemcrafters.net" "https://systemcrafters.net/community"))))

(ert-deftest erc-links-test-scan-two-wikipedia-urls ()
  "Both URLs are scanned when separated only by \" / \" (the Demes/Nika bug)."
  (should (equal
           (erc-links-tests--urls
            "talking about https://en.wikipedia.org/wiki/Demes_in_the_Byzantine_Empire \
/ https://en.wikipedia.org/wiki/Nika_riots so")
           '("https://en.wikipedia.org/wiki/Demes_in_the_Byzantine_Empire"
             "https://en.wikipedia.org/wiki/Nika_riots"))))

(ert-deftest erc-links-test-scan-query-string ()
  "A URL with a ?query is captured whole (mobileread showthread)."
  (should (member "https://www.mobileread.com/forums/showthread.php?t=277431"
                  (erc-links-tests--urls
                   "https://www.mobileread.com/forums/showthread.php?t=277431"))))

(ert-deftest erc-links-test-scan-fragment-and-query ()
  "A URL with a #fragment is captured whole (codeberg PR comment anchor)."
  (should (member
           "https://codeberg.org/guix/guix-consensus-documents/pulls/13#issuecomment-15926108"
           (erc-links-tests--urls
            "futurile's take https://codeberg.org/guix/guix-consensus-documents/pulls/13#issuecomment-15926108"))))

(ert-deftest erc-links-test-scan-multiple-query-params ()
  "Multiple &-joined query params stay part of the URL (kuvrd abaya)."
  (should (member
           "https://kuvrd.ca/collections/qamar/products/the-keffiyeh-abaya?_pos=4&_fid=ab65aed10&_ss=c"
           (erc-links-tests--urls
            "also https://kuvrd.ca/collections/qamar/products/the-keffiyeh-abaya?_pos=4&_fid=ab65aed10&_ss=c which"))))

(ert-deftest erc-links-test-scan-github-issue-path ()
  "Deep GitHub issue path is captured."
  (should (member
           "https://github.com/FreeBSDFoundation/freebsd-laptop-testing/issues/34"
           (erc-links-tests--urls
            "https://github.com/FreeBSDFoundation/freebsd-laptop-testing/issues/34"))))

(ert-deftest erc-links-test-scan-mastodon-handle ()
  "Mastodon status URLs with @handle are captured."
  (should (member
           "https://grapheneos.social/@GrapheneOS/116761584866632116"
           (erc-links-tests--urls
            "https://grapheneos.social/@GrapheneOS/116761584866632116 just in case"))))

(ert-deftest erc-links-test-scan-long-path ()
  "Very long article slugs are captured whole (Tom's Hardware)."
  (let ((u "https://www.tomshardware.com/phones/commodore-announces-linux-based-flip-phone-with-no-social-media-no-browser-the-callback-8020-will-be-available-in-five-retro-colorways-starting-at-usd499-runs-99-percent-of-android-apps"))
    (should (member u (erc-links-tests--urls (concat "see " u))))))

(ert-deftest erc-links-test-scan-png-is-a-url ()
  "A .png link is scanned as a URL (content-type filtering happens at fetch)."
  (should (member
           "https://files.mastodon.social/media_attachments/files/116/759/103/original/41cbc6f15de5d38f.png"
           (erc-links-tests--urls
            "https://files.mastodon.social/media_attachments/files/116/759/103/original/41cbc6f15de5d38f.png"))))

(ert-deftest erc-links-test-scan-http-with-port ()
  "Plain http URLs with an explicit port are captured (outrider shortener)."
  (should (member "http://outrider.deepsky.com:5808/SOUQB"
                  (erc-links-tests--urls
                   "[ http://outrider.deepsky.com:5808/SOUQB ]::[ Demes ]"))))

(ert-deftest erc-links-test-scan-rejects-non-http ()
  "Non-http(s) schemes are filtered out."
  (should (equal (erc-links-tests--urls
                  "ftp://example.com/file and mailto:a@b.c and https://ok.example")
                 '("https://ok.example"))))


;;;; Inline insertion.

(ert-deftest erc-links-test-insert-appends-faced-hint ()
  "The hint is appended at the marker with `erc-links-face'."
  (with-temp-buffer
    (insert "see https://x.example")
    (let ((m (copy-marker (point-max) t)))
      (erc-links--insert (current-buffer) m "Example Title")
      (should (equal (buffer-string) "see https://x.example [Example Title]"))
      (let ((pos (1+ (string-search "[" (buffer-string)))))
        (should (eq (get-text-property pos 'face) 'erc-links-face))))))

(ert-deftest erc-links-test-wrap-fits-on-line ()
  "A hint that fits within the fill column is left on one line."
  (let ((erc-links-fill-column 80)
        (erc-links-continuation-indent 4))
    (should (equal (erc-links--wrap "[Short Title]" 20)
                   " [Short Title]"))))

(ert-deftest erc-links-test-wrap-breaks-and-indents ()
  "An over-long hint wraps on whitespace, indenting continuation lines."
  (let ((erc-links-fill-column 40)
        (erc-links-continuation-indent 4))
    (should (equal (erc-links--wrap "[one two three four five six seven]" 30)
                   " [one two\n    three four five six seven]"))))

(ert-deftest erc-links-test-wrap-long-word-not-split ()
  "A single word wider than the fill column is emitted whole, not split."
  (let ((erc-links-fill-column 20)
        (erc-links-continuation-indent 2))
    (should (equal (erc-links--wrap "[supercalifragilistic]" 15)
                   "\n  [supercalifragilistic]"))))


;;;; Caching.

(ert-deftest erc-links-test-cache-roundtrip ()
  "Fresh cache entries are returned; the cache can be cleared."
  (let ((erc-links--cache (make-hash-table :test 'equal))
        (erc-links-cache-file nil))
    (puthash "https://h" (cons (current-time) "Hello") erc-links--cache)
    (should (equal (cdr (gethash "https://h" erc-links--cache)) "Hello"))
    (erc-links-clear-cache)
    (should (zerop (hash-table-count erc-links--cache)))))

(ert-deftest erc-links-test-cache-persist-roundtrip ()
  "Successful entries survive a save/load cycle; failures are not persisted."
  (let* ((erc-links-cache-file (make-temp-file "erc-links-test" nil ".eld"))
         (erc-links--cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "https://a" (cons (current-time) "Title A") erc-links--cache)
          (puthash "https://bad" (cons (current-time) "") erc-links--cache)
          (erc-links--save-cache)
          ;; Drop the in-memory table and reload purely from disk.
          (setq erc-links--cache (make-hash-table :test 'equal))
          (erc-links--load-cache)
          (should (equal (cdr (gethash "https://a" erc-links--cache)) "Title A"))
          (should (null (gethash "https://bad" erc-links--cache))))
      (ignore-errors (delete-file erc-links-cache-file)))))

(ert-deftest erc-links-test-cache-load-missing-file ()
  "Loading a non-existent cache file is a no-op, not an error."
  (let ((erc-links-cache-file (expand-file-name
                               "erc-links-does-not-exist.eld"
                               temporary-file-directory))
        (erc-links--cache (make-hash-table :test 'equal)))
    (when (file-exists-p erc-links-cache-file)
      (delete-file erc-links-cache-file))
    (should-not (erc-links--load-cache))
    (should (zerop (hash-table-count erc-links--cache)))))

(ert-deftest erc-links-test-clear-cache-deletes-file ()
  "`erc-links-clear-cache' removes the on-disk cache too."
  (let* ((erc-links-cache-file (make-temp-file "erc-links-test" nil ".eld"))
         (erc-links--cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "https://a" (cons (current-time) "Title A") erc-links--cache)
          (erc-links--save-cache)
          (should (file-exists-p erc-links-cache-file))
          (erc-links-clear-cache)
          (should-not (file-exists-p erc-links-cache-file))
          (should (zerop (hash-table-count erc-links--cache))))
      (ignore-errors (delete-file erc-links-cache-file)))))


;;;; Opt-in: live network fetches of the real URLs (skipped by default).

(defun erc-links-tests--fetch-title (url)
  "Synchronously fetch URL the way `erc-links--fetch' would and return title."
  (require 'url)
  (let* ((url-user-agent erc-links-user-agent)
         (buf (url-retrieve-synchronously url t t 25)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (let* ((headers-end (if (re-search-forward "\r?\n\r?\n" nil t)
                                  (point) (point-max)))
                 (ctype (progn
                          (goto-char (point-min))
                          (when (re-search-forward
                                 "^Content-Type:[ \t]*\\(.*\\)$" headers-end t)
                            (downcase (string-trim (match-string 1)))))))
            (when (and ctype (string-match-p "html" ctype))
              (erc-links--from-html headers-end (point-max)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defconst erc-links-tests--live-urls
  '(("https://systemcrafters.net" . html)
    ("https://github.com/parenworks/clatter.el" . html)
    ("https://www.youtube.com/watch?v=ZTIRLCtOnlc" . html)
    ("https://www.wired.com/story/all-the-ways-europe-is-ditching-american-technology/" . html)
    ("https://commerce.jolla.com/products/jolla-phone-sept-26" . html)
    ("https://en.wikipedia.org/wiki/Nika_riots" . html)
    ("https://commodore.net/why-a-flip-phone/" . html)
    ("https://files.mastodon.social/media_attachments/files/116/759/103/171/949/555/original/41cbc6f15de5d38f.png" . non-html))
  "Representative live URLs by type; titles change so only presence is checked.")

(ert-deftest erc-links-test-live-fetch ()
  "Each live HTML URL yields a non-empty title; the PNG yields nil.
Skipped unless the environment variable ERC_LINKS_LIVE is set."
  (skip-unless (getenv "ERC_LINKS_LIVE"))
  (dolist (entry erc-links-tests--live-urls)
    (let ((title (ignore-errors (erc-links-tests--fetch-title (car entry)))))
      (pcase (cdr entry)
        ('html (should (and (stringp title) (not (string-empty-p title)))))
        ('non-html (should (null title)))))))

(provide 'erc-links-tests)
;;; erc-links-tests.el ends here
