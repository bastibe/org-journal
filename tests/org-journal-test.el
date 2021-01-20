;; org-journal-test.el --- Test file for org-journal -*- lexical-binding: t; -*-
;;
;; Author: Christian Schwarzgruber (c.schwarzgruber.cs@gmail.com)
;;
;; Copyright (c) Christian Schwarzgruber
;;
;; Description:
;;

(require 'ert)
(require 'org-journal)

(defvar org-journal-dir-test (make-temp-file "org-journal-" t))

(defun org-journal-dir-test-setup ()
  "Create temporary directory."
  (when (file-exists-p org-journal-dir-test)
    (delete-directory org-journal-dir-test t))
  ;; Kill leftover buffers from previous failed test
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer) (string-match org-journal-dir-test (buffer-file-name buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (kill-buffer buffer))))
  (make-directory org-journal-dir-test)
  (make-symbolic-link org-journal-dir-test (concat org-journal-dir-test "-link") t)
  (org-journal-invalidate-cache))

(defmacro org-journal-test-macro (&rest body)
  "Wrapp a `org-journal' -- `ert'-test with default values."
  (declare (indent 1))
  `(let* ((org-journal-dir (concat org-journal-dir-test "-link"))
          (comment-start-skip "^\\s-*#\\(?: \\|$\\)")
          (org-journal--cache-file (expand-file-name  "org-journal.cache" org-journal-dir-test))
          (org-journal-file-type 'daily)
          (org-journal-date-format "Test header")
          (org-agenda-inhibit-startup t)
          (org-journal-time-format "%R")
          org-journal-file-header)
     (org-journal-invalidate-cache)
     (org-journal-dir-test-setup)
     ,@body
     (delete-directory org-journal-dir-test t)
     (delete-file (concat org-journal-dir-test "-link"))))
(def-edebug-spec org-journal-test-macro (body))

(ert-deftest org-journal-calendar-date-from-file-test ()
  "Should return a list with day/month/year"
  (org-journal-test-macro
      (should (equal (org-journal--file-name->calendar-date
                      (expand-file-name "20190103" (file-truename org-journal-dir)))
                     '(1 03 2019)))))

(ert-deftest org-journal-convert-time-to-file-type-time-test ()
  "Testing"
  (org-journal-test-macro
      (let ((time (current-time)))
        (should (equal (org-journal--convert-time-to-file-type-time time)
                       time))
        (setq time (encode-time 0 0 0 3 1 2019)
              org-journal-file-type 'weekly)
        (should (equal (org-journal--convert-time-to-file-type-time time)
                       (encode-time 0 0 0 31 12 2018)))
        (setq time (encode-time 0 0 0 30 12 2018)
              org-journal-file-type 'weekly
              org-journal-start-on-weekday 7) ;; Start of week is Sunday
        (should (equal (org-journal--convert-time-to-file-type-time time)
                       (encode-time 0 0 0 30 12 2018)))
        (setq time (encode-time 0 0 0 15 4 2019)
              org-journal-file-type 'monthly)
        (should (equal (org-journal--convert-time-to-file-type-time time)
                       (encode-time 0 0 0 1 4 2019)))
        (setq time (encode-time 0 0 0 3 2 2019)
              org-journal-file-type 'yearly)
        (should (equal (org-journal--convert-time-to-file-type-time time)
                       (encode-time 0 0 0 1 1 2019))))))

(ert-deftest org-journal-insert-header-test ()
  "Test insertion of header"
  (org-journal-test-macro
      (let ((org-journal-file-header "#+TITLE: Some header\n#+STARTUP: folded"))
        (org-journal-new-entry t)
        (save-buffer)
        (kill-buffer)
        (should (string= (with-temp-buffer
                           (insert-file-contents (org-journal--get-entry-path))
                           (buffer-substring-no-properties (point-min) (point-max)))
                         (concat org-journal-file-header "\n* Test header\n"))))))

(ert-deftest org-journal-carryover-items-test ()
  "Org journal new entry test."
  (org-journal-test-macro
      (let ((org-journal-file-type 'weekly)
            ;; Always use english as time locale.
            (system-time-locale "C")
            (buffer "20181231"))
        (with-temp-buffer
          (insert "* Tuesday, 01/01/19\n")
          (org-set-property "CREATED" "20190101")
          (insert "** 13:00 Some journal entry\n")
          (insert "* Wednesday, 01/02/19\n")
          (org-set-property "CREATED" "20190102")
          (insert "** TODO First\n")
          (insert "** 13:00 Some journal entry 1\n")
          (insert "** TODO Second\n")
          (insert "** 14:00 Some journal entry 2\n")
          (write-file (expand-file-name buffer org-journal-dir-test))
          (kill-buffer buffer))
        (find-file (expand-file-name buffer org-journal-dir-test))
        (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 1 1 2019)))))
        (kill-buffer buffer)
        (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019)))))
        (kill-buffer buffer)
        (org-journal-new-entry t)
        (save-buffer)
        (kill-buffer) ;; Kills new journal file buffer
        (kill-buffer buffer)
        (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 1 1 2019)))))
        (kill-buffer)
        (should (not (stringp (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019)))))
        (kill-buffer)

        (should (string= (with-temp-buffer
                           (org-journal-mode)
                           (insert-file-contents (org-journal--get-entry-path))
                           (replace-regexp-in-string
                            "^[ ]*" ""
                            (buffer-substring-no-properties (point-min) (point-max))))
                         (concat "* Test header\n:PROPERTIES:\n:CREATED:  "
                                 (format-time-string org-journal-created-property-timestamp-format)
                                 "\n:END:\n** TODO First\n** TODO Second\n"))))))

(ert-deftest org-journal-carryover-keep-parents-test ()
  "Org journal new entry test for daily files."
  (org-journal-test-macro
      (let ((buffer "20181231")
            (new-entry (concat "** " (format-time-string org-journal-time-format))))
        (with-temp-buffer
          (insert "* Wednesday, 01/02/19\n")
          (insert "** a\n")
          (insert "** TODO a\n")
          (insert "** b1\n")
          (insert "*** TODO b1\n")
          (insert "*** DONE b1\n")
          (insert "*** b1 note\n")
          (insert "*** b2\n")
          (insert "**** TODO b2\n")
          (insert "**** b3\n")
          (insert "***** TODO b3\n")
          (insert "***** DONE b3\n")
          (insert "** TODO b\n")
          (insert "** 14:00 Some journal entry 2\n")
          (write-file (expand-file-name buffer org-journal-dir-test))
          (kill-buffer buffer))
        (org-journal-new-entry nil)
        (save-buffer)
        (kill-buffer)
        (kill-buffer buffer)
        (should (string= (with-temp-buffer
                           (insert-file-contents (org-journal--get-entry-path))
                           (buffer-substring-no-properties (point-min) (point-max)))
                         (concat "* Test header\n** TODO a\n** b1\n*** TODO b1\n*** b2\n**** TODO b2\n**** b3\n***** TODO b3\n** TODO b\n" new-entry "\n")))
        )))

(ert-deftest org-journal-carryover-delete-empty-journal-test ()
  "Org journal delete empty journal test"
  (org-journal-test-macro
      (let ((buffer "20181231")
            (org-journal-carryover-delete-empty-journal 'always))
        ;; Test that journal file gets dumped, after carryover
        (with-temp-buffer
          (insert "* Wednesday, 01/02/19\n")
          (insert "** TODO a\n")
          (write-file (expand-file-name buffer org-journal-dir-test))
          (kill-buffer buffer))
        (org-journal-new-entry nil)
        (save-buffer)
        (kill-buffer)
        (should (not (file-exists-p (org-journal--get-entry-path (encode-time 0 0 0 31 12 2018)))))

        ;; Test even single entry gets deleted, relevant for weekly, monthly and yearly journal files.
        (org-journal-dir-test-setup)
        (setq org-journal-file-type 'yearly)
        (with-temp-buffer
          (insert "* Wednesday, 01/01/19\n")
          (org-set-property "CREATED" "20190101")
          (insert "** TODO a\n")
          (insert "* Wednesday, 01/02/19\n")
          (org-set-property "CREATED" "20190102")
          (insert "** TODO a\n")
          (write-file (expand-file-name "20190101" org-journal-dir-test))
          (kill-buffer "20190101"))
        (org-journal-new-entry nil)
        (save-buffer)
        (kill-buffer)
        (let ((inhibit-message t))
          (should
           (string= "No journal entry for this date."
                    (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019) 'noselect)))))))

(ert-deftest org-journal-search-build-file-list-test ()
  "Test for `org-journal--search-build-file-list'."
  (org-journal-test-macro
      (let ((test-file-daily '("20170104" "20170312" "20190201"))
            (test-file-yearly '("20170101" "20180101" "20190101"))
            (test-file-weekly '("20170102" "20180430" "20181231"))
            (test-file-monthly '("20170101" "20180401" "20190301"))
            (create-files (lambda (test-files)
                            (org-journal-dir-test-setup)
                            (dolist (file test-files)
                              (find-file (expand-file-name file org-journal-dir-test))
                              (save-buffer)
                              (kill-buffer))))
            period-start
            period-end)
        ;; Daily build file boundary check
        (setq period-start (encode-time 0 0 0 4 1 2017)
              period-end (encode-time 0 0 0 1 2 2019))
        (funcall create-files test-file-daily)
        (should (equal (length (org-journal--search-build-file-list period-start period-end)) 1))

        (setq period-start (encode-time 0 0 0 8 1 2017)
              period-end (encode-time 0 0 0 31 12 2018))
        ;; Weekly build file boundary check
        (setq org-journal-file-type 'weekly)
        (funcall create-files test-file-weekly)
        (should (equal (length (org-journal--search-build-file-list period-start period-end)) 1))

        (setq period-start (encode-time 0 0 0 31 1 2017)
              period-end (encode-time 0 0 0 1 3 2019))
        ;; Monthly build file boundary check
        (setq org-journal-file-type 'monthly)
        (funcall create-files test-file-monthly)
        (should (equal (length (org-journal--search-build-file-list period-start period-end)) 1))

        (setq period-start (encode-time 0 0 0 31 12 2017)
              period-end (encode-time 0 0 0 1 1 2019))
        ;; Yearly build file boundary check
        (setq org-journal-file-type 'yearly)
        (funcall create-files test-file-yearly)
        (should (equal (length (org-journal--search-build-file-list period-start period-end)) 1)))))

(ert-deftest org-journal-scheduled-carryover-yearly-test ()
  (org-journal-test-macro
      (let ((org-journal-file-type 'yearly)
            (org-journal-created-property-timestamp-format "[%Y-%m-%d %a]")
            new-scheduled-date)
        ;; Copy test files to temporary directory
        (copy-directory
         (directory-file-name "tests/journals/yearly/carryover1")
         (file-name-as-directory org-journal-dir-test) nil nil t)
        (org-journal-new-entry 4) ;; 4 - no new time entry
        (setq new-scheduled-date (with-temp-buffer
                                   (org-insert-time-stamp (current-time))
                                   (buffer-substring-no-properties (point-min) (point-max))))
        (goto-char (point-min))
        (search-forward new-scheduled-date)
        (search-forward new-scheduled-date))))

(ert-deftest org-journal-scheduled-carryover-daily-test ()
  (org-journal-test-macro
      (let ((org-journal-file-type 'daily)
            (org-journal-date-prefix "#+TITLE: ")
            (org-journal-time-prefix "* ")
            (org-journal-file-format "%Y-%m-%d.org")
            (org-journal-date-format "%Y-%m-%d (%A)")
            org-journal-encrypt-journal
            org-journal-enable-encryption
            org-journal-enable-cache
            org-journal-file-header
            new-scheduled-date)
        ;; Copy test files to temporary directory
        (copy-directory
         (directory-file-name "tests/journals/daily/carryover1")
         (file-name-as-directory org-journal-dir-test) nil nil t)

        (org-journal-new-entry 4) ;; 4 - no new time entry
        (setq new-scheduled-date (with-temp-buffer
                                   (org-insert-time-stamp (current-time))
                                   (buffer-substring-no-properties (point-min) (point-max))))
        (save-buffer)
        (goto-char (point-min))
        ;; FIXME(cschwarzgruber): this is bad; changes are high that this test passes even though it shouldn't. Better use `should'...
        (search-forward new-scheduled-date)
        (search-forward new-scheduled-date))))
