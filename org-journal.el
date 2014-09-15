;;; org-journal.el --- a simple org-mode based journaling mode

;; Author: Bastian Bechtold
;; URL: http://github.com/bastibe/emacs-journal
;; Version: 1.5.3

;; Adapted from http://www.emacswiki.org/PersonalDiary

;; Functions to maintain a simple personal diary / journal in Emacs.
;; Feel free to use, modify and improve the code! - mtvoid, bastibe

;; This file is also available from marmalade as
;; http://marmalade-repo.org/packages/journal. After installing, add
;; the line (require 'journal) to your .emacs or init.el to activate
;; it. You also need to specify the directory where your journal files
;; will be saved. You can do this by setting the variable journal-dir
;; (remember to add a trailing slash). journal-dir is also a
;; customizable variable. The default value for journal-dir is
;; ~/Documents/journal/.
;;
;; Inside the journal directory, a separate file is created for each
;; day with a journal entry, with a file name in the format YYYYMMDD
;; (this is customizable). Each journal entry is an org-mode file that
;; begins with a date entry on the top, followed by entries for a
;; different times. Any subsequent entries on the same day are written
;; in the same file, with their own timestamp. You can customize the
;; date and time formats (or remove them entirely). To start writing a
;; journal entry, press "C-c j".
;;
;; You can browse through existing journal entries on disk via the
;; calendar. All dates for which an entry is present are highlighted.
;; Pressing "j" will open it up for viewing. Pressing "[" or "]" will
;; select the date with the previous or next journal entry,
;; respectively. Pressing "i j" will create a new entry for the chosen
;; date.
;;
;; Quick summary:
;; To create a new journal entry for the current time and day: C-c j
;; In calendar view: j to view an entry
;;                   i j to add a new entry
;;                   [ to go to previous entry
;;                   ] to go to next entry
;; When viewing a journal entry: C-c b to view previous entry
;;                               C-c f to view next entry

;; use this function to update auto-mode-alist whenever
;; org-journal-dir or org-journal-file-pattern change.
;;;###autoload
(defun org-journal-update-auto-mode-alist ()
  "Update auto-mode-alist to open journal files in
  org-journal-mode"
  (let ((name (concat (file-truename org-journal-dir)
                      (substring org-journal-file-pattern 1))))
    (add-to-list 'auto-mode-alist
                 (cons name 'org-journal-mode))))

;;;###autoload
(add-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)

(defvar org-journal-date-list nil)
(defvar org-journal-file-pattern
  "^\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\)$")

;;;###autoload
(defun org-journal-format-string->regex (format-string)
  "Update org-journal-file-pattern with the current
  org-journal-file-format"
  (concat
   "^"
   (replace-regexp-in-string
    "%d" "\\\\(?3:[0-9][0-9]\\\\)"
    (replace-regexp-in-string
     "%m" "\\\\(?2:[0-9][0-9]\\\\)"
     (replace-regexp-in-string
      "%Y" "\\\\(?1:[0-9]\\\\{4\\\\}\\\\)" format-string)))
   "$"))

; Customizable variables
(defgroup org-journal nil
  "Settings for the personal journal"
  :group 'applications)
;;;###autoload
(defcustom org-journal-dir "~/Documents/journal/"
  "Directory containing journal entries.
  Setting this will update auto-mode-alist using
  `(org-journal-update-auto-mode-alist)`"
  :type 'string :group 'org-journal
  :set (lambda (symbol value)
         (set-default symbol value)
         (org-journal-update-auto-mode-alist)))
;;;###autoload
(defcustom org-journal-file-format "%Y%m%d"
  "Format string for journal file names, by default \"YYYYMMDD\".
  This pattern must include `%Y`, `%m` and `%d`. Setting this
  will update the internal `org-journal-file-pattern` to a regex
  that matches the format string using
  `(org-journal-format-string->regex format-string)`, and update
  `auto-mode-alist` using
  `(org-journal-update-auto-mode-alist)`."
  :type 'string :group 'org-journal
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq org-journal-file-pattern
               (org-journal-format-string->regex value))
         (org-journal-update-auto-mode-alist)))
(defcustom org-journal-date-format "%A, %x"
  "Format string for date, by default \"WEEKDAY, DATE\", where
  DATE is what Emacs thinks is an appropriate way to format days
  in your language."
  :type 'string :group 'org-journal)
(defcustom org-journal-date-prefix "* "
  "String that is put before every date at the top of a journal
  file. By default, this is a org-mode heading. Another good idea
  would be \"#+TITLE: \" for org titles."
  :type 'string :group 'org-journal)
(defcustom org-journal-time-format "%R "
  "Format string for time, by default HH:MM. Set it to a blank
string if you want to disable timestamps."
  :type 'string :group 'org-journal)
(defcustom org-journal-time-prefix "** "
  "String that is put before every time entry in a journal file.
  By default, this is an org-mode sub-heading."
  :type 'string :group 'org-journal)

(require 'calendar)
;;;###autoload
(add-hook 'calendar-initial-window-hook 'org-journal-get-list)
;;;###autoload
(add-hook 'calendar-today-visible-hook 'org-journal-mark-entries)
;;;###autoload
(add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

;; Journal mode definition
(define-derived-mode org-journal-mode org-mode "Journal"
  "Mode for writing or viewing entries written in the journal"
  (turn-on-visual-line-mode)
  (add-hook 'after-save-hook 'org-journal-redraw-calendar nil t)
  (add-hook 'after-revert-hook 'org-journal-redraw-calendar nil t)
  (run-mode-hooks))

;; Key bindings
(define-key org-journal-mode-map (kbd "C-c f") 'org-journal-open-next-entry)
(define-key org-journal-mode-map (kbd "C-c b") 'org-journal-open-previous-entry)

;;;###autoload
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map "j" 'org-journal-read-entry)
     (define-key calendar-mode-map "]" 'org-journal-next-entry)
     (define-key calendar-mode-map "[" 'org-journal-previous-entry)
     (define-key calendar-mode-map (kbd "i j") 'org-journal-new-date-entry)))

;;;###autoload
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(defun org-journal-dir-check-or-create ()
  "Check existence of `org-journal-dir'. If it doesn't exist, try to make directory."
  (unless (file-exists-p org-journal-dir)
    (if (yes-or-no-p (format "Journal directory %s not found. Create one? " org-journal-dir))
        (make-directory org-journal-dir t)
      (error "Journal directory is necessary to use org-journal.")))
  t)

;; Creates a new entry
;;;###autoload
(defun org-journal-new-entry ()
  "Open today's journal file and start a new entry"
  (interactive)
  (org-journal-dir-check-or-create)
  (find-file (concat org-journal-dir
                     (format-time-string org-journal-file-format)))
  (goto-char (point-max))
  (let ((unsaved (buffer-modified-p)))
    (if (equal (point-max) 1)
        (insert org-journal-date-prefix
                (format-time-string org-journal-date-format)))
    (unless (eq (current-column) 0) (insert "\n"))
    (insert "\n" org-journal-time-prefix
            (format-time-string org-journal-time-format))
    (org-journal-mode)
    (hide-sublevels 2)
    (set-buffer-modified-p unsaved)))

(defun org-journal-calendar-date->time (calendar-date)
  "Convert a date as returned from the calendar to a time"
  (encode-time 0 0 0                   ; second, minute, hour
               (nth 1 calendar-date)   ; day
               (nth 0 calendar-date)   ; month
               (nth 2 calendar-date))) ; year

(defun org-journal-file-name->calendar-date (file-name)
  "Convert an org-journal file name to a calendar date.
   If org-journal-file-pattern does not contain capture groups,
   fall back to the old behavior of taking substrings."
  (if (and (integerp (string-match "\(\?1:" org-journal-file-pattern))
           (integerp (string-match "\(\?2:" org-journal-file-pattern))
           (integerp (string-match "\(\?3:" org-journal-file-pattern)))
      (list (string-to-number (replace-regexp-in-string
                               org-journal-file-pattern "\\2"
                               file-name))
            (string-to-number (replace-regexp-in-string
                               org-journal-file-pattern "\\3"
                               file-name))
            (string-to-number (replace-regexp-in-string
                               org-journal-file-pattern "\\1"
                               file-name)))
    (list (string-to-number (substring file-name 4 6))
          (string-to-number (substring file-name 6 8))
          (string-to-number (substring file-name 0 4)))))

;;;###autoload
(defun org-journal-new-date-entry (arg &optional event)
  "Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-dir-check-or-create)
    (find-file-other-window (concat org-journal-dir
                                    (format-time-string org-journal-file-format time)))
    (goto-char (point-max))
    (let ((unsaved (buffer-modified-p)))
      (if (equal (point-max) 1)
          (insert org-journal-date-prefix
                  (format-time-string org-journal-date-format time)))
      (unless (eq (current-column) 0) (insert "\n"))
      (insert "\n" org-journal-time-prefix
              (if (= (time-to-days (current-time)) (time-to-days time))
                  (format-time-string org-journal-time-format)
                ""))
      (hide-sublevels 2)
      (set-buffer-modified-p unsaved))))

(defun org-journal-open-next-entry ()
  "Open the next journal entry starting from a currently displayed one"
  (interactive)
  (let ((calendar-date (org-journal-file-name->calendar-date
                        (file-name-nondirectory (buffer-file-name))))
        (view-mode-p view-mode)
        (dates org-journal-date-list))
    (calendar-basic-setup nil t)
    (while (and dates (not (calendar-date-compare (list calendar-date) dates)))
      (setq dates (cdr dates)))
    (calendar-exit)
    (if dates
        (let* ((time (org-journal-calendar-date->time (car dates)))
               (filename (concat org-journal-dir
                                 (format-time-string
                                  org-journal-file-format time))))
          (find-file filename)
          (view-mode (if view-mode-p 1 -1))
          (org-show-subtree))
      (message "No next journal entry after this one"))))

(defun org-journal-open-previous-entry ()
  "Open the previous journal entry starting from a currently displayed one"
  (interactive)
  (let ((calendar-date (org-journal-file-name->calendar-date
                        (file-name-nondirectory (buffer-file-name))))
        (view-mode-p view-mode)
        (dates (reverse org-journal-date-list)))
    (calendar-basic-setup nil t)
    (while (and dates (calendar-date-compare (list calendar-date) dates))
      (setq dates (cdr dates)))
    (calendar-exit)
    (if (and dates (cadr dates))
        (let* ((time (org-journal-calendar-date->time (cadr dates)))
               (filename (concat org-journal-dir
                                 (format-time-string
                                  org-journal-file-format time))))
          (find-file filename)
          (view-mode (if view-mode-p 1 -1))
          (org-show-subtree))
      (message "No previous journal entry before this one"))))

;;
;; Functions to browse existing journal entries using the calendar
;;

;;;###autoload
(defun org-journal-get-list ()
  "Loads the list of files in the journal directory, and converts
  it into a list of calendar DATE elements"
  (org-journal-dir-check-or-create)
  (setq org-journal-date-list
	(mapcar #'org-journal-file-name->calendar-date
            (directory-files org-journal-dir nil org-journal-file-pattern nil)))
  (calendar-redraw))

;;;###autoload
(defun org-journal-mark-entries ()
  "Mark days in the calendar for which a diary entry is present"
  (dolist (journal-entry org-journal-date-list)
    (if (calendar-date-is-visible-p journal-entry)
      (calendar-mark-visible-date journal-entry))))

;;;###autoload
(defun org-journal-read-entry (arg &optional event)
  "Open journal entry for selected date for viewing"
  (interactive
   (list current-prefix-arg last-nonmenu-event))

  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event)))
         (org-journal-file (concat org-journal-dir
                                   (format-time-string org-journal-file-format time))))

    (if (file-exists-p org-journal-file)
        (progn
          (view-file-other-window org-journal-file)
          (setq-local org-hide-emphasis-markers t)
          (org-show-subtree))
      (message "No journal entry for this date."))))

;;;###autoload
(defun org-journal-next-entry ()
  "Go to the next date with a journal entry"
  (interactive)
  (let ((dates org-journal-date-list))
    (while (and dates (not (calendar-date-compare
                            (list (calendar-cursor-to-date)) dates)))
      (setq dates (cdr dates)))
    (if dates (calendar-goto-date (car dates)))))

;;;###autoload
(defun org-journal-previous-entry ()
  "Go to the previous date with a journal entry"
  (interactive)
  (let ((dates (reverse org-journal-date-list)))
    (while (and dates
                (not (calendar-date-compare dates (list (calendar-cursor-to-date)))))
      (setq dates (cdr dates)))
    (if dates (calendar-goto-date (car dates)))))

(defun org-journal-redraw-calendar ()
  "Redraw the calendar with all current journal entries"
  (save-window-excursion
    (calendar-basic-setup nil t)
    (org-journal-mark-entries)
    (calendar-exit)))

(provide 'org-journal)

;;; org-journal.el ends here
