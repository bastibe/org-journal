;;; org-journal.el --- a simple org-mode based journaling mode

;; Author: Bastian Bechtold
;; URL: http://github.com/bastibe/emacs-journal
;; Version: 1.3.1

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
;; day with a journal entry, with a file name in the format YYYYMMDD.
;; Each journal entry is an org-mode file that begins with the date on
;; the top, followed by the time. Any subsequent entries on the same
;; day are written in the same file, with their own timestamp. You can
;; customize the date and time formats (or remove them entirely). To
;; start writing a journal entry, press "C-c j".
;;
;; You can browse through existing journal entries on disk via the
;; calendar. All dates for which an entry is present are highlighted.
;; Pressing "j" will open it up for viewing. Pressing "[" or "]" will
;; select the date with the previous or next journal entry,
;; respectively. Pressing "i j" will create a new entry for the chosen
;; date.
;;
;; Quick summary:
;; To create a new journal entry: C-c j
;; In calendar view: j to view an entry
;;                   i j to add a new entry
;;                   [ to go to previous entry
;;                   ] to go to next entry


; Customizable variables
(defgroup org-journal nil "Settings for the personal journal" :group
'applications)
(defcustom org-journal-dir "~/Documents/journal/" "Directory containing journal entries"
  :type 'string :group 'org-journal)
(defcustom org-journal-date-format "%A, %x%n"
  "Format string for date, by default YYYY-MM-DD."
  :type 'string :group 'org-journal)
(defcustom org-journal-time-format "%R "
  "Format string for time, by default HH:MM. Set it to a blank string if you want to disable timestamps."
  :type 'string :group 'org-journal)

(defvar org-journal-date-list nil)
(defvar org-journal-file)

;; Automatically switch to journal mode when opening a journal entry file
(add-to-list 'auto-mode-alist
	     (cons (concat (car (last (split-string org-journal-dir "/" t)))
			   "/[0-9]\\{8\\}$") 'org-journal-mode))

(require 'calendar)
(add-hook 'calendar-initial-window-hook 'org-journal-get-list)
(add-hook 'calendar-today-visible-hook 'org-journal-mark-entries)
(add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

;; Key bindings
(define-key calendar-mode-map "j" 'org-journal-read-entry)
(define-key calendar-mode-map "]" 'org-journal-next-entry)
(define-key calendar-mode-map "[" 'org-journal-previous-entry)
(define-key calendar-mode-map (kbd "i j") 'org-journal-new-date-entry)
(global-set-key "\C-cj" 'org-journal-new-entry)

;; Journal mode definition
(define-derived-mode org-journal-mode org-mode "Journal" "Mode for writing or viewing entries written in the journal"
  (turn-on-visual-line-mode)
  (add-hook 'after-save-hook 'org-journal-redraw-calendar nil t)
  (add-hook 'after-revert-hook 'org-journal-redraw-calendar nil t)
  (run-mode-hooks))

;; Creates a new entry
(defun org-journal-new-entry ()
  "Open today's journal file and start a new entry"
  (interactive)
  (unless (file-exists-p org-journal-dir) (error "Journal directory %s not found" org-journal-dir))
  (find-file (concat org-journal-dir (format-time-string "%Y%m%d")))
  (goto-char (point-max))
  (let ((unsaved (buffer-modified-p)))
    (if (equal (point-max) 1) (insert "* " (format-time-string org-journal-date-format)))
    (unless (eq (current-column) 0) (insert "\n"))
    (insert "\n** " (format-time-string org-journal-time-format))
    (hide-sublevels 2)
    (set-buffer-modified-p unsaved)))

(defun org-journal-calendar-date->time (calendar-date)
  "Convert a date as returned from the calendar to a time"
  (encode-time 0 0 0                    ; second, minute, hour
               (cadr calendar-date)     ; day
               (car calendar-date)      ; month
               (caddr calendar-date)))  ; year

(defun org-journal-new-date-entry (arg &optional event)
  "Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal-calendar-date->time (calendar-cursor-to-date t event))))
    (unless (file-exists-p org-journal-dir) (error "Journal directory %s not found" org-journal-dir))
    (find-file-other-window (concat org-journal-dir (format-time-string "%Y%m%d" time)))
    (goto-char (point-max))
    (let ((unsaved (buffer-modified-p)))
      (if (equal (point-max) 1) (insert "* " (format-time-string org-journal-date-format time)))
      (unless (eq (current-column) 0) (insert "\n"))
      (insert "\n** " (if (= (time-to-days (current-time)) (time-to-days time))
                  (format-time-string org-journal-time-format)
                ""))
      (hide-sublevels 2)
      (set-buffer-modified-p unsaved))))

;;
;; Functions to browse existing journal entries using the calendar
;;

(defun org-journal-get-list ()
  "Loads the list of files in the journal directory, and converts it into a list of calendar DATE elements"
  (unless (file-exists-p org-journal-dir) (error "Journal directory %s not found" org-journal-dir))
  (setq org-journal-date-list
	(mapcar #'(lambda (journal-file)
		   (let ((y (string-to-number (substring journal-file 0 4)))
			 (m (string-to-number (substring journal-file 4 6)))
			 (d (string-to-number (substring journal-file 6 8))))
		     (list m d y)))
		   (directory-files org-journal-dir nil "^[0-9]\\{8\\}$" nil)))
  (calendar-redraw))

(defun org-journal-mark-entries ()
  "Mark days in the calendar for which a diary entry is present"
  (dolist (journal-entry org-journal-date-list)
    (if (calendar-date-is-visible-p journal-entry)
      (calendar-mark-visible-date journal-entry))))

(defun org-journal-read-entry ()
  "Open journal entry for selected date for viewing"
  (interactive)
  (setq org-journal-file (int-to-string (+ (* 10000 (nth 2 (calendar-cursor-to-date))) (* 100 (nth 0 (calendar-cursor-to-date))) (nth 1 (calendar-cursor-to-date)))))
  (if (file-exists-p (concat org-journal-dir org-journal-file))
      (progn
        (view-file-other-window (concat org-journal-dir org-journal-file))
        (setq-local org-hide-emphasis-markers t)
        (org-show-subtree))
    (message "No journal entry for this date.")))

(defun org-journal-next-entry ()
  "Go to the next date with a journal entry"
  (interactive)
  (let ((dates org-journal-date-list))
    (while (and dates (not (calendar-date-compare (list (calendar-cursor-to-date)) dates)))
      (setq dates (cdr dates)))
    (if dates (calendar-goto-date (car dates)))))

(defun org-journal-previous-entry ()
  "Go to the previous date with a journal entry"
  (interactive)
  (let ((dates (reverse org-journal-date-list)))
    (while (and dates (not (calendar-date-compare dates (list (calendar-cursor-to-date)))))
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
