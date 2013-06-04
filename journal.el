;;; journal.el --- a simple org-mode based journaling mode

;; Author: Bastian Bechtold
;; URL: http://github.com/bastibe/emacs-journal
;; Version: 1.2.1

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
;; respectively. Pressing "J" will create a new entry for the chosen
;; date.
;;
;; Quick summary:
;; To create a new journal entry: C-c j
;; In calendar view: j to view an entry
;;                   J to add a new entry
;;                   [ to go to previous entry
;;                   ] to go to next entry


; Customizable variables
(defgroup journal nil "Settings for the personal journal" :group
'applications)
(defcustom journal-dir "~/Documents/journal/" "Directory containing journal entries"
  :type 'string :group 'journal)
(defcustom journal-date-format "%A, %x%n"
  "Format string for date, by default YYYY-MM-DD."
  :type 'string :group 'journal)
(defcustom journal-time-format "%R "
  "Format string for time, by default HH:MM. Set it to a blank string if you want to disable timestamps."
  :type 'string :group 'journal)

;(defvar journal-dir "~/Documents/journal/") ; Directory containing journal files
(defvar journal-date-list nil)
(defvar journal-file)

;; Automatically switch to journal mode when opening a journal entry file
(add-to-list 'auto-mode-alist
	     (cons (concat (car (last (split-string journal-dir "/" t)))
			   "/[0-9]\\{8\\}$") 'journal-mode))

(require 'calendar)
(add-hook 'calendar-initial-window-hook 'journal-get-list)
(add-hook 'calendar-today-visible-hook 'journal-mark-entries)
(add-hook 'calendar-today-invisible-hook 'journal-mark-entries)

;; Key bindings
(define-key calendar-mode-map "j" 'journal-read-entry)
(define-key calendar-mode-map "]" 'journal-next-entry)
(define-key calendar-mode-map "[" 'journal-previous-entry)
(define-key calendar-mode-map "J" 'journal-new-date-entry)
(global-set-key "\C-cj" 'journal-new-entry)

;; Journal mode definition
(define-derived-mode journal-mode org-mode "Journal" "Mode for writing or viewing entries written in the journal"
  (turn-on-visual-line-mode)
  (add-hook 'after-save-hook 'journal-redraw-calendar nil t)
  (add-hook 'after-revert-hook 'journal-redraw-calendar nil t)
  (run-mode-hooks))

;; Creates a new entry
(defun journal-new-entry ()
  "Open today's journal file and start a new entry"
  (interactive)
  (unless (file-exists-p journal-dir) (error "Journal directory %s not found" journal-dir))
  (find-file (concat journal-dir (format-time-string "%Y%m%d")))
  (goto-char (point-max))
  (let ((unsaved (buffer-modified-p)))
    (if (equal (point-max) 1) (insert "* " (format-time-string journal-date-format)))
    (unless (eq (current-column) 0) (insert "\n"))
    (insert "\n** " (format-time-string journal-time-format))
    (hide-sublevels 2)
    (set-buffer-modified-p unsaved)))

(defun journal-calendar-date->time (calendar-date)
  "Convert a date as returned from the calendar to a time"
  (encode-time 0 0 0                    ; second, minute, hour
               (cadr calendar-date)     ; day
               (car calendar-date)      ; month
               (caddr calendar-date)))  ; year

(defun journal-new-date-entry (arg &optional event)
  "Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (journal-calendar-date->time (calendar-cursor-to-date t event))))
    (unless (file-exists-p journal-dir) (error "Journal directory %s not found" journal-dir))
    (find-file-other-window (concat journal-dir (format-time-string "%Y%m%d" time)))
    (goto-char (point-max))
    (let ((unsaved (buffer-modified-p)))
      (if (equal (point-max) 1) (insert "* " (format-time-string journal-date-format time)))
      (unless (eq (current-column) 0) (insert "\n"))
      (insert "\n** " (if (= (time-to-days (current-time)) (time-to-days time))
                  (format-time-string journal-time-format)
                ""))
      (hide-sublevels 2)
      (set-buffer-modified-p unsaved))))

;;
;; Functions to browse existing journal entries using the calendar
;;

(defun journal-get-list ()
  "Loads the list of files in the journal directory, and converts it into a list of calendar DATE elements"
  (unless (file-exists-p journal-dir) (error "Journal directory %s not found" journal-dir))
  (setq journal-date-list
	(mapcar #'(lambda (journal-file)
		   (let ((y (string-to-number (substring journal-file 0 4)))
			 (m (string-to-number (substring journal-file 4 6)))
			 (d (string-to-number (substring journal-file 6 8))))
		     (list m d y)))
		   (directory-files journal-dir nil "^[0-9]\\{8\\}$" nil)))
  (calendar-redraw))

(defun journal-mark-entries ()
  "Mark days in the calendar for which a diary entry is present"
  (dolist (journal-entry journal-date-list)
    (if (calendar-date-is-visible-p journal-entry)
      (calendar-mark-visible-date journal-entry))))

(defun journal-read-entry ()
  "Open journal entry for selected date for viewing"
  (interactive)
  (setq journal-file (int-to-string (+ (* 10000 (nth 2 (calendar-cursor-to-date))) (* 100 (nth 0 (calendar-cursor-to-date))) (nth 1 (calendar-cursor-to-date)))))
  (if (file-exists-p (concat journal-dir journal-file))
      (progn
        (view-file-other-window (concat journal-dir journal-file))
        (org-show-subtree))
    (message "No journal entry for this date.")))

(defun journal-next-entry ()
  "Go to the next date with a journal entry"
  (interactive)
  (let ((dates journal-date-list))
    (while (and dates (not (calendar-date-compare (list (calendar-cursor-to-date)) dates)))
      (setq dates (cdr dates)))
    (if dates (calendar-goto-date (car dates)))))

(defun journal-previous-entry ()
  "Go to the previous date with a journal entry"
  (interactive)
  (let ((dates (reverse journal-date-list)))
    (while (and dates (not (calendar-date-compare dates (list (calendar-cursor-to-date)))))
      (setq dates (cdr dates)))
    (if dates (calendar-goto-date (car dates)))))

(defun journal-redraw-calendar ()
  "Redraw the calendar with all current journal entries"
  (save-window-excursion
    (calendar)
    (journal-mark-entries)
    (calendar-exit)))

(provide 'journal)

;;; journal.el ends here
