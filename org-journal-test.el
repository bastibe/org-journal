;; org-journal-test.el --- -*- lexical-binding: t; -*-
;;
;; Author: Christian Schwarzgruber (c.schwarzgruber.cs@gmail.com)
;;
;; Copyright (c) Christian Schwarzgruber
;;
;; Description:
;;
;; Test org-journal in a clean Emacs environment. You need to adopt the
;; path to the package `use-package', `bind-key' and `org-journal'.
;;
;; (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20181119.2350")
;; (add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20180513.430")
;; (require 'use-package)
;;
;; (use-package org-journal
;;   :load-path "~/Developing/third_party/org-journal"
;;   :defer 0.5
;;   :bind (("<f9> s" . org-journal-search)
;;          ("<f9> j" . org-journal-new-entry))
;;   :init
;;   ;; Must be set before loading org-journal
;;   (setq org-journal-dir "~/emacs/org/journal/")
;;   :config
;;   (setq org-journal-encrypt-journal t
;;         org-journal-file-type 'yearly))
;;

(require 'org-journal)

(defvar org-journal-dir-test "/tmp/org-journal")

(defun org-journal-dir-test-setup ()
  "Create temporary directory."
  (when (file-exists-p org-journal-dir-test)
    (delete-directory org-journal-dir-test t))
  (make-directory org-journal-dir-test))

(defun org-journal-file-pattern-test ()
  (org-journal-dir-and-format->regex
   org-journal-dir org-journal-file-format))

(ert-deftest org-journal-calendar-date-from-file ()
  "Should return a list with day/month/year"
  (org-journal-dir-test-setup)
  (let* ((org-journal-dir org-journal-dir-test)
         (org-journal-file-pattern (org-journal-file-pattern-test)))
    (should (equal (org-journal-file-name->calendar-date
                    (expand-file-name "20190103" org-journal-dir))
                   '(1 03 2019)))))

(ert-deftest org-journal-convert-time-to-file-type-time-test ()
  "Testing"
  (let ((time (current-time))
        (org-journal-file-type 'daily))
    (should (equal (org-journal-convert-time-to-file-type-time time)
                   time))
    (setq time (encode-time 0 0 0 3 1 2019)
          org-journal-file-type 'weekly)
    (should (equal (org-journal-convert-time-to-file-type-time time)
                   (encode-time 0 0 0 31 12 2018)))
    (setq time (encode-time 0 0 0 15 4 2019)
          org-journal-file-type 'monthly)
    (should (equal (org-journal-convert-time-to-file-type-time time)
                   (encode-time 0 0 0 1 4 2019)))
    (setq time (encode-time 0 0 0 3 2 2019)
          org-journal-file-type 'yearly)
    (should (equal (org-journal-convert-time-to-file-type-time time)
                   (encode-time 0 0 0 1 1 2019)))))


(ert-deftest org-journal-carryover-items-test ()
  "Org journal new enty test."
  (org-journal-dir-test-setup)
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
    (write-file (expand-file-name "20181231" org-journal-dir-test)))
  (let* ((org-journal-dir org-journal-dir-test)
         (comment-start-skip "^\\s-*#\\(?: \\|$\\)")
         (org-journal-file-pattern (org-journal-file-pattern-test))
         (org-journal-file-type 'weekly)
         (org-journal-carryover-items "TODO=\"TODO\"")
         (org-journal-encrypt-journal nil)
         (org-journal-enable-encryption nil)
         (org-journal-date-format "Test header")
         (org-journal-time-format "%R")
         (new-entry (concat "** " (format-time-string org-journal-time-format))))
    (org-journal-new-entry nil)
    (kill-buffer "20181231")
    (write-file (org-journal-get-entry-path))
    (kill-buffer)
    (should (string= (with-temp-buffer
                       (insert-file-contents (org-journal-get-entry-path))
                       (buffer-substring-no-properties (point-min) (point-max)))
                     (concat "* Test header\n:PROPERTIES:\n:CREATED:  "
                             (format-time-string "%Y%m%d")
                             "\n:END:\n** TODO First\n** TODO Second\n"
                             new-entry "\n")))))

(ert-deftest org-journal-carryover-items-daily-test ()
  "Org journal new enty test for daily files."
  (org-journal-dir-test-setup)
  (with-temp-buffer
    (insert "* Wednesday, 01/02/19\n")
    (insert "** TODO First\n")
    (insert "** 13:00 Some journal entry 1\n")
    (insert "** TODO Second\n")
    (insert "** 14:00 Some journal entry 2\n")
    (write-file (expand-file-name "20181231" org-journal-dir-test)))
  (let* ((org-journal-dir org-journal-dir-test)
         (comment-start-skip "^\\s-*#\\(?: \\|$\\)")
         (org-journal-file-pattern (org-journal-file-pattern-test))
         (org-journal-file-type 'daily)
         (org-journal-carryover-items "TODO=\"TODO\"")
         (org-journal-encrypt-journal nil)
         (org-journal-enable-encryption nil)
         (org-hide-block-startup t)
         (org-journal-date-format "Test header")
         (org-journal-time-format "%R")
         (new-entry (concat "** " (format-time-string org-journal-time-format))))
    (org-journal-new-entry nil)
    (kill-buffer "20181231")
    (write-file (org-journal-get-entry-path))
    (kill-buffer)
    (should (string= (with-temp-buffer
                       (insert-file-contents (org-journal-get-entry-path))
                       (buffer-substring-no-properties (point-min) (point-max)))
                     (concat "* Test header\n** TODO First\n** TODO Second\n" new-entry "\n")))))

(ert-deftest org-journal-search-build-file-list-test ()
  "Test for `org-journal-search-build-file-list'."
  (let* ((org-journal-dir org-journal-dir-test)
         (org-journal-file-pattern (org-journal-file-pattern-test))
         (org-journal-file-type 'daily)
         (org-journal-carryover-items "TODO=\"TODO\"")
         (org-journal-encrypt-journal nil)
         (org-journal-enable-encryption nil)
         (test-file-daily '("20170104" "20170312" "20190201"))
         (test-file-yearly '("20170101" "20180101" "20190101"))
         (test-file-weekly '("20170102" "20180430" "20181231"))
         (test-file-monthly '("20170101" "20180401" "20190301"))
         (create-files (lambda (test-files)
                         (org-journal-dir-test-setup)
                         (dolist (file test-files)
                           (with-temp-buffer
                             (write-file (expand-file-name file org-journal-dir-test))))))
         period-start
         period-end)
    (message "Running daily build-file-test")
    ;; Daily build file boundary check
    (setq period-start (encode-time 0 0 0 4 1 2017)
          period-end (encode-time 0 0 0 1 2 2019))
    (funcall create-files test-file-daily)
    (should (equal (length (org-journal-search-build-file-list period-start period-end)) 1))

    (message "Running weekly build-file-test")
    (setq period-start (encode-time 0 0 0 8 1 2017)
          period-end (encode-time 0 0 0 31 12 2018))
    ;; Weekly build file boundary check
    (setq org-journal-file-type 'weekly)
    (funcall create-files test-file-weekly)
    (should (equal (length (org-journal-search-build-file-list period-start period-end)) 1))

    (message "Running monthly build-file-test")
    (setq period-start (encode-time 0 0 0 31 1 2017)
          period-end (encode-time 0 0 0 1 3 2019))
    ;; Monthly build file boundary check
    (setq org-journal-file-type 'monthly)
    (funcall create-files test-file-monthly)
    (should (equal (length (org-journal-search-build-file-list period-start period-end)) 1))

    (message "Running yearly build-file-test")
    (setq period-start (encode-time 0 0 0 31 12 2017)
          period-end (encode-time 0 0 0 1 1 2019))
    ;; Yearly build file boundary check
    (setq org-journal-file-type 'yearly)
    (funcall create-files test-file-yearly)
    (should (equal (length (org-journal-search-build-file-list period-start period-end)) 1))))
