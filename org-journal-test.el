;; org-journal-test.el ---
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
  org-journal-file-pattern
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
         (org-journal-date-format "Test header"))
    (org-journal-new-entry t)
    (kill-buffer "20181231")
    (write-file (org-journal-get-entry-path))
    (kill-buffer)
    (should (string= (with-temp-buffer
                       (insert-file-contents (org-journal-get-entry-path))
                       (buffer-substring-no-properties (point-min) (point-max)))
                     (concat "* Test header\n:PROPERTIES:\n:CREATED:  "
                             (format-time-string "%Y%m%d")
                             "\n:END:\n** TODO First\n** TODO Second\n")))))
