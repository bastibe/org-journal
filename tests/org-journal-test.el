;; org-journal-test.el --- Test file for org-journal -*- lexical-binding: t; -*-
;;
;; Author: Christian Schwarzgruber (c.schwarzgruber.cs@gmail.com)
;;
;; Copyright (c) 2020-2022 Christian Schwarzgruber
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;; this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its contributors
;; may be used to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
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
          (org-journal-created-property-timestamp-format "%Y%m%d")
          org-journal-file-header
          org-journal-encrypt-journal
          org-journal-enable-encryption)
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
     (let ((inhibit-message t))
       (org-journal-new-entry t))
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
       (org-journal-mode)
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
          (org-journal-mode)
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
          (org-journal-mode)
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
        (let ((message-marker nil))
          (cl-letf (((symbol-function 'message)
                     #'(lambda (x &rest y) (setq message-marker x))))
            (org-journal-read-or-display-entry (encode-time 0 0 0 2 1 2019) 'noselect)
            (should (equal "No journal entry for this date." message-marker))
            )))))

(ert-deftest org-journal-search-build-file-list-test ()
  "Test for `org-journal--search-build-file-list'."
  (org-journal-test-macro
      (let ((test-file-daily '("20170104" "20170312" "20190201"))
            (test-file-yearly '("2017" "2018" "2019"))
            (test-file-weekly '("20170102" "20180430" "20181231"))
            (test-file-monthly '("201701" "201804" "201903"))
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

        ;; Weekly build file boundary check
        (setq period-start (encode-time 0 0 0 8 1 2017)
              period-end (encode-time 0 0 0 31 12 2018)
              org-journal-file-type 'weekly)
        (funcall create-files test-file-weekly)
        (should (equal (length (org-journal--search-build-file-list period-start period-end)) 1))

        ;; Monthly build file boundary check
        (setq period-start (encode-time 0 0 0 31 1 2017)
              period-end (encode-time 0 0 0 1 3 2019)
              org-journal-file-type 'monthly
              org-journal-file-format "%Y%m")
        (funcall create-files test-file-monthly)
        (should (equal (length (org-journal--search-build-file-list period-start period-end)) 1))

        ;; Yearly build file boundary check
        (setq period-start (encode-time 0 0 0 31 12 2017)
              period-end (encode-time 0 0 0 1 1 2019)
              org-journal-file-type 'yearly
              org-journal-file-format "%Y")
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

(ert-deftest org-journal-scheduled-weekly-test ()
  (org-journal-test-macro
   (let ((org-journal-file-type 'weekly)
         (org-journal-start-on-weekday 7) ;; sunday
         org-journal-encrypt-journal
         org-journal-enable-encryption
         org-journal-enable-cache
         org-journal-file-header
         day-offset)

     ;; Compute correct day-offset, so future and today journal entry end in the same file
     (let ((current-date (calendar-current-date))
           (current-date+1 (calendar-current-date 1)))
       (if (equal (org-journal--convert-time-to-file-type-time
                   (org-journal--calendar-date->time current-date) )
                  (org-journal--convert-time-to-file-type-time
                   (org-journal--calendar-date->time current-date+1)))
           (setq day-offset 1)
         (setq day-offset 2)))

     (let* ((scheduled-entry-date (calendar-current-date day-offset))
            (scheduled-entry-time (org-journal--calendar-date->time scheduled-entry-date))
            (new-entry-date (calendar-current-date (if (= day-offset 1) nil 1)))
            (new-entry-time (org-journal--calendar-date->time new-entry-date))
            (scheduled-string (with-temp-buffer
                                (org-insert-time-stamp scheduled-entry-time)
                                (buffer-string))))
       ;; Add first scheduled entry
       (org-journal-new-scheduled-entry nil scheduled-entry-time)
       (insert "Task 1")
       ;; Add a second scheduled entry
       (org-journal-new-scheduled-entry nil scheduled-entry-time)
       (insert "Task 2")
       ;; New today entry should be added at the beginning of the journal file
       (org-journal-new-entry 4 new-entry-time)
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      (with-temp-buffer
                        (org-journal-mode)
                        (insert
                         (concat
                          ;; Today entry
                          "* Test header\n"
                          ":PROPERTIES:\n"
                          (concat
                           ":CREATED:  "
                           (format-time-string org-journal-created-property-timestamp-format new-entry-time)
                           "\n")
                          ":END:\n"

                          ;; Scheduled entries
                          "* Test header\n"
                          ":PROPERTIES:\n"
                          (concat
                           ":CREATED:  "
                           (format-time-string org-journal-created-property-timestamp-format scheduled-entry-time)
                           "\n")
                          ":END:\n"
                          "** TODO Task 1\n"
                          scheduled-string
                          "\n"
                          ;; (format-time-string "   SCHEDULED: <%F %a>\n" )
                          "** TODO Task 2\n"
                          scheduled-string))
                        (buffer-substring-no-properties (point-min) (point-max)))))))))
