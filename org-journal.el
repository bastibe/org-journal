;;; org-journal.el --- a simple org-mode based journaling mode

;; Author: Bastian Bechtold
;; URL: http://github.com/bastibe/org-journal
;; Version: 1.15.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Adapted from http://www.emacswiki.org/PersonalDiary

;; Functions to maintain a simple personal diary / journal in Emacs.
;; Feel free to use, modify and improve the code! - mtvoid, bastibe

;; This file is also available from marmalade as
;; http://marmalade-repo.org/packages/journal. After installing, add
;; the line (require 'org-journal) to your .emacs or init.el to activate
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
;; journal entry, press "C-c C-j". You can also open the current day's
;; entry without adding a new entry with "C-u C-c C-j".
;;
;; You can browse through existing journal entries on disk via the
;; calendar. All dates for which an entry is present are highlighted.
;; Pressing "j" will open it up for viewing. Pressing "C-j" will open
;; it for viewing, but not switch to it. Pressing "[" or "]" will
;; select the date with the previous or next journal entry,
;; respectively. Pressing "i j" will create a new entry for the chosen
;; date.
;;
;; TODO items from the previous day will carry over to the current
;; day. This is customizable through org-journal-carryover-items.
;;
;; Quick summary:
;; To create a new journal entry for the current time and day: C-c C-j
;; To open today's journal without creating a new entry: C-u C-c C-j
;; In calendar view: j to view an entry in a new buffer
;;                   C-j to view an entry but not switch to it
;;                   i j to add a new entry
;;                   f w to search all entries of the current week
;;                   f m to search all entries of the current month
;;                   f y to search all entries of the current year
;;                   f f to search all entries of all time
;;                   f F to search all entries in the future
;;                   [ to go to previous entry
;;                   ] to go to next entry
;; When viewing a journal entry: C-c C-b to view previous entry
;;                               C-c C-f to view next entry

;;; Code:
(require 'org)

(when (version< org-version "9.2")
  (defalias 'org-set-tags-to 'org-set-tags))

(defvar org-journal-file-pattern
  (expand-file-name "~/Documents/journal/\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\)\\'")
  "This matches journal files in your journal directory.
This variable is created and updated automatically by
org-journal. Use org-journal-file-format instead.")

;; use this function to update auto-mode-alist whenever
;; org-journal-dir or org-journal-file-pattern change.
;;;###autoload
(defun org-journal-update-auto-mode-alist ()
  "Update auto-mode-alist to open journal files in
  org-journal-mode"
  (add-to-list 'auto-mode-alist
               (cons org-journal-file-pattern 'org-journal-mode)))

;;;###autoload
(add-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)
(add-hook 'org-agenda-mode-hook 'org-journal-update-org-agenda-files)

;;;###autoload
(defun org-journal-dir-and-format->regex (dir format)
  "Update org-journal-file-pattern with the current
  org-journal-file-format"
  (concat
   (file-truename (expand-file-name (file-name-as-directory dir)))
   (replace-regexp-in-string
    "%d" "\\\\(?3:[0-9][0-9]\\\\)"
    (replace-regexp-in-string
     "%m" "\\\\(?2:[0-9][0-9]\\\\)"
     (replace-regexp-in-string
      "%Y" "\\\\(?1:[0-9]\\\\{4\\\\}\\\\)" format)))
   "\\(\\.gpg\\)?\\'"))

; Customizable variables
(defgroup org-journal nil
  "Settings for the personal journal"
  :version "1.15.1"
  :group 'applications)

(defface org-journal-highlight
  '((t (:foreground "#ff1493")))
  "Face for highlighting org-journal buffers.")

(defun org-journal-highlight (str)
  "Highlight STR in current-buffer"
  (goto-char (point-min))
  (while (search-forward str nil t)
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'org-journal-highlight)))

(defface org-journal-calendar-entry-face
  '((t (:foreground "#aa0000" :slant italic)))
  "Face for highlighting org-journal entries in M-x calendar.")

(defface org-journal-calendar-scheduled-face
  '((t (:foreground "#600000" :slant italic)))
  "Face for highlighting future org-journal entries in M-x calendar.")

(defcustom org-journal-dir "~/Documents/journal/"
  "Directory containing journal entries. Setting this will update the
  internal `org-journal-file-pattern` to a regex that matches the
  directory, using `org-journal-dir-and-format->regex`, and update
  `auto-mode-alist` using `(org-journal-update-auto-mode-alist)`."
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; if org-journal-file-format is not yet bound, we’ll need a default value
         (let ((format (if (boundp 'org-journal-file-format)
                           org-journal-file-format
                         "%Y%m%d")))
           (setq org-journal-file-pattern
                 (org-journal-dir-and-format->regex value format)))
         (org-journal-update-auto-mode-alist)))

(defcustom org-journal-file-format "%Y%m%d"
  "Format string for journal file names, by default \"YYYYMMDD\".
  This pattern must include `%Y`, `%m` and `%d`. Setting this
  will update the internal `org-journal-file-pattern` to a regex
  that matches the format string, using
  `org-journal-dir-and-format->regex`, and update
  `auto-mode-alist` using
  `(org-journal-update-auto-mode-alist)`."
  :type 'string
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; If org-journal-dir is not yet bound, we’ll need a default value
         (let ((dir (if (boundp 'org-journal-dir)
                        org-journal-dir
                      "~/Documents/journal/")))
           (setq org-journal-file-pattern
                 (org-journal-dir-and-format->regex dir value)))
         (org-journal-update-auto-mode-alist)))

(defcustom org-journal-date-format "%A, %x"
  "Format string for date, by default \"WEEKDAY, DATE\", where
  DATE is what Emacs thinks is an appropriate way to format days
  in your language. If you define it as a function, it is evaluated
  and inserted."
  :type 'string)

(defcustom org-journal-date-prefix "* "
  "String that is put before every date at the top of a journal
  file. By default, this is a org-mode heading. Another good idea
  would be \"#+TITLE: \" for org titles."
  :type 'string)

(defcustom org-journal-time-format "%R "
  "Format string for time, by default HH:MM. Set it to a blank
string if you want to disable timestamps."
  :type 'string)

(defcustom org-journal-time-format-post-midnight ""
  "When non-blank, a separate time format string for after midnight,
when the current time is before the hour set by `org-extend-today-until`."
  :type 'string)

(defcustom org-journal-time-prefix "** "
  "String that is put before every time entry in a journal file.
  By default, this is an org-mode sub-heading."
  :type 'string)

(defcustom org-journal-hide-entries-p t
  "If true, org-journal-mode will hide all but the current entry
   when creating a new one."
  :type 'boolean)

(require 'org-crypt nil 'noerror)

(defcustom org-journal-enable-encryption nil
  "If non-nil, New journal entries will have a
`org-crypt-tag-matcher' tag for encrypting. Whenever a user
saves/opens these journal entries, emacs asks a user passphrase
to encrypt/decrypt it."
  :type 'boolean)

(defcustom org-journal-encrypt-journal nil
  "If non-nil, encrypt journal files using gpg.
The journal files will have the file extension .gpg"
  :type 'boolean)

(defcustom org-journal-encrypt-on 'before-save-hook
  "Hook on which to encrypt entries. It can be set to other hooks
  like kill-buffer-hook. "
  :type 'function)

(defcustom org-journal-enable-agenda-integration nil
  "If non-nil, automatically adds current and future org-journal
  files to org-agenda-files."
  :type 'boolean)

(defcustom org-journal-find-file 'find-file-other-window
  "The function to use when opening an entry. Set this to `find-file` if you don't want org-journal to split your window."
  :type 'function)

(defcustom org-journal-carryover-items "TODO=\"TODO\""
  "Carry over items that match these criteria from the previous entry to new entries.
See agenda tags view match description for the format of this."
  :type 'string)

(defcustom org-journal-search-results-order-by :asc
  "When :desc, make search results ordered by date descending

Otherwise, date ascending."
  :type 'symbol)

(defvar org-journal-after-entry-create-hook nil
  "Hook called after journal entry creation")

;; Automatically switch to journal mode when opening a journal entry file
(setq org-journal-file-pattern
      (org-journal-dir-and-format->regex org-journal-dir org-journal-file-format))
(org-journal-update-auto-mode-alist)

(require 'calendar)
;;;###autoload
(add-hook 'calendar-today-visible-hook 'org-journal-mark-entries)
;;;###autoload
(add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

;; Journal mode definition
;;;###autoload
(define-derived-mode org-journal-mode org-mode "Journal"
  "Mode for writing or viewing entries written in the journal"
  (turn-on-visual-line-mode)
  (add-hook 'after-save-hook 'org-journal-redraw-calendar nil t)
  (add-hook 'after-save-hook 'org-journal-update-org-agenda-files nil t)
  (add-hook 'after-revert-hook 'org-journal-redraw-calendar nil t)
  (run-mode-hooks))

;; Key bindings
(define-key org-journal-mode-map (kbd "C-c C-f") 'org-journal-open-next-entry)
(define-key org-journal-mode-map (kbd "C-c C-b") 'org-journal-open-previous-entry)
(define-key org-journal-mode-map (kbd "C-c C-j") 'org-journal-new-entry)
(define-key org-journal-mode-map (kbd "C-c C-s") 'org-journal-search)

;;;###autoload
(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map "j" 'org-journal-read-entry)
     (define-key calendar-mode-map (kbd "C-j") 'org-journal-display-entry)
     (define-key calendar-mode-map "]" 'org-journal-next-entry)
     (define-key calendar-mode-map "[" 'org-journal-previous-entry)
     (define-key calendar-mode-map (kbd "i j") 'org-journal-new-date-entry)
     (define-key calendar-mode-map (kbd "f f") 'org-journal-search-forever)
     (define-key calendar-mode-map (kbd "f F") 'org-journal-search-future)
     (define-key calendar-mode-map (kbd "f w") 'org-journal-search-calendar-week)
     (define-key calendar-mode-map (kbd "f m") 'org-journal-search-calendar-month)
     (define-key calendar-mode-map (kbd "f y") 'org-journal-search-calendar-year)))

;;;###autoload
(global-set-key (kbd "C-c C-j") 'org-journal-new-entry)

(defun org-journal-get-entry-path (&optional time)
  "Return the path to an entry given a TIME.
If no TIME is given, uses the current time."
  (let ((file (file-truename
               (expand-file-name
                (format-time-string org-journal-file-format time)
                (file-name-as-directory org-journal-dir)))))
    (when (and org-journal-encrypt-journal (not (file-exists-p file)))
      (setq file (concat file ".gpg")))
    file))

(defun org-journal-dir-check-or-create ()
  "Check existence of `org-journal-dir'. If it doesn't exist, try to make directory."
  (unless (file-exists-p org-journal-dir)
    (if (yes-or-no-p (format "Journal directory %s not found. Create one? " org-journal-dir))
        (make-directory org-journal-dir t)
      (error "Journal directory is necessary to use org-journal.")))
  t)

;;;###autoload
(defun org-journal-new-entry (prefix &optional time)
  "Open today's journal file and start a new entry.
Giving the command a PREFIX arg will just open a today's file,
without adding an entry. If given a TIME, create an entry for the
time's day. If no TIME was given, use the current time (which is
interpreted as belonging to yesterday if smaller than
`org-extend-today-until`).

Whenever a journal entry is created the
`org-journal-after-entry-create-hook' hook is run"
  (interactive "P")
  (org-journal-dir-check-or-create)

  ;; if time is before org-extend-today-until, interpret it as
  ;; part of the previous day:
  (let ((oetu-active-p nil))
    (let ((now (decode-time nil)))
      (if (and (not time) ; time was not given
               (< (nth 2 now)
                  org-extend-today-until))
          (setq oetu-active-p t
                time (encode-time (nth 0 now)      ; second
                                  (nth 1 now)      ; minute
                                  (nth 2 now)      ; hour
                                  (1- (nth 3 now)) ; day
                                  (nth 4 now)      ; month
                                  (nth 5 now)      ; year
                                  (nth 8 now)))))  ; timezone

    (let* ((entry-path (org-journal-get-entry-path time))
           (should-add-entry-p (not prefix)))

      ;; open journal file
      (unless (string= entry-path (buffer-file-name))
        (funcall org-journal-find-file entry-path))
      (org-journal-decrypt)
      (goto-char (point-max))
      (let ((new-file-p (equal (point-max) 1)))

        ;; empty file? Add a date timestamp
        (when new-file-p
          (if (functionp org-journal-date-format)
              (insert (funcall org-journal-date-format time))
              (insert org-journal-date-prefix
                      (format-time-string org-journal-date-format time))))

        ;; add crypt tag if encryption is enabled and tag is not present
        (when org-journal-enable-encryption
          (goto-char (point-min))
          (unless (member org-crypt-tag-matcher (org-get-tags))
            (org-set-tags-to org-crypt-tag-matcher))
          (goto-char (point-max)))

        ;; move TODOs from previous day here
        (when (and org-journal-carryover-items
                   (string= entry-path (org-journal-get-entry-path (current-time))))
          (save-excursion (org-journal-carryover)))

        ;; insert the header of the entry
        (when should-add-entry-p
          (unless (eq (current-column) 0) (insert "\n"))
          (let* ((day-discrepancy (- (time-to-days (current-time)) (time-to-days time)))
                 (timestamp (cond
                             ;; “time” is today, use normal timestamp format
                             ((= day-discrepancy 0)
                              (format-time-string org-journal-time-format))
                             ;; “time” is yesterday with org-extend-today-until,
                             ;; use different timestamp format if available
                             ((and (= day-discrepancy 1) oetu-active-p)
                              (if (not (string-equal org-journal-time-format-post-midnight ""))
                                  (format-time-string org-journal-time-format-post-midnight)
                                  (format-time-string org-journal-time-format)))
                             ;; “time” is on some other day, use blank timestamp
                             (t ""))))
            (insert "\n" org-journal-time-prefix timestamp))
          (run-hooks 'org-journal-after-entry-create-hook))

        ;; switch to the outline, hide subtrees
        (org-journal-mode)
        (if (and org-journal-hide-entries-p (org-journal-time-entry-level))
            (outline-hide-sublevels (org-journal-time-entry-level))
            (outline-show-all))

        ;; open the recent entry when the prefix is given
        (when should-add-entry-p
          (outline-show-entry))))))

(defun org-journal-carryover ()
  "Moves all items matching org-journal-carryover-items from the
previous day's file to the current file."
  (interactive)
  (let ((current-buffer-name (buffer-name))
        (all-todos))
    (save-excursion
      (let ((org-journal-find-file 'find-file)
            (delete-mapper
             (lambda ()
               (let ((subtree (org-journal-extract-current-subtree t)))
                 ;; since the next subtree now starts at point,
                 ;; continue mapping from before that, to include it
                 ;; in the search
                 (backward-char)
                 (setq org-map-continue-from (point))
                 subtree))))
        (org-journal-open-previous-entry)
        (setq all-todos (org-map-entries delete-mapper
                                         org-journal-carryover-items))
        (save-buffer)))
    (switch-to-buffer current-buffer-name)
    (when all-todos
      (unless (eq (current-column) 0) (insert "\n"))
      (insert "\n")
      (insert (mapconcat 'identity all-todos "")))))

(defun org-journal-extract-current-subtree (delete-p)
  "Get the string content of the entire current subtree."
  (let* ((start (progn (beginning-of-line)
                       (point)))
         (end (progn (org-end-of-subtree)
                     (outline-next-heading)
                     (point)))
         (subtree (buffer-substring-no-properties start end)))
    (when delete-p
      (delete-region start end))
    subtree))

(defun org-journal-time-entry-level ()
  "Return the headline level of time entries based on the number
of leading asterisks in 'org-journal-time-prefix.

Return nil when it's impossible to figure out the level."
  (if (string-match "\\(^\*+\\)" org-journal-time-prefix)
      (length (match-string 1 org-journal-time-prefix))))

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
(defun org-journal-new-date-entry (prefix &optional event)
  "Open the journal for the date indicated by point and start a new entry.
If the date is not today, it won't be given a time heading. If a
prefix is given, don't add a new heading.
If the date is in the future, create a schedule entry."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (if (time-less-p time (current-time))
        (org-journal-new-entry prefix time)
      (org-journal-new-scheduled-entry prefix (format-time-string "%Y-%m-%d" time)))))

;;;###autoload
(defun org-journal-new-scheduled-entry (prefix &optional scheduled-time)
  "Create a new entry in the future."
  (interactive "P")
  (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:"))))
    (org-journal-new-entry nil (org-time-string-to-time scheduled-time))
    (if (not prefix)
        (insert "TODO "))
    (save-excursion
      (insert "\n<" scheduled-time ">"))))

(defun org-journal-open-next-entry ()
  "Open the next journal entry starting from a currently displayed one"
  (interactive)
  (let ((calendar-date (org-journal-file-name->calendar-date
                        (buffer-file-name)))
        (view-mode-p view-mode)
        (dates (org-journal-list-dates)))
    ;; insert current buffer in list if not present
    (unless (file-exists-p (buffer-file-name))
      (setq dates (cons calendar-date dates))
      (sort dates (lambda (a b) (calendar-date-compare (list a) (list b)))))
    (calendar-basic-setup nil t)
    (while (and dates (not (calendar-date-compare (list calendar-date) dates)))
      (setq dates (cdr dates)))
    (calendar-exit)
    (if dates
        (let* ((time (org-journal-calendar-date->time (car dates)))
               (filename (org-journal-get-entry-path time)))
          (find-file filename)
          (org-journal-decrypt)
          (view-mode (if view-mode-p 1 -1))
          (if (org-at-heading-p) (org-show-subtree)))
      (message "No next journal entry after this one"))))

(defun org-journal-open-previous-entry ()
  "Open the previous journal entry starting from a currently displayed one"
  (interactive)
  (let ((calendar-date (org-journal-file-name->calendar-date
                        (buffer-file-name)))
        (view-mode-p view-mode)
        (dates (reverse (org-journal-list-dates))))
    ;; insert current buffer in list if not present
    (unless (file-exists-p (buffer-file-name))
      (setq dates (cons calendar-date dates))
      ;; reverse-sort!
      (sort dates (lambda (a b) (calendar-date-compare (list b) (list a)))))
    (calendar-basic-setup nil t)
    (while (and dates (calendar-date-compare (list calendar-date) dates))
      (setq dates (cdr dates)))
    (calendar-exit)
    (if (and dates (cadr dates))
        (let* ((time (org-journal-calendar-date->time (cadr dates)))
               (filename (org-journal-get-entry-path time)))
          (find-file filename)
          (org-journal-decrypt)
          (view-mode (if view-mode-p 1 -1))
          (if (org-at-heading-p) (org-show-subtree)))
      (message "No previous journal entry before this one"))))

;;
;; Functions to browse existing journal entries using the calendar
;;

;;;###autoload
(defun org-journal-list-files ()
  "Returns a list of all files in the journal directory"
  (org-journal-dir-check-or-create)
  ;; grab the file list. We can’t use directory-files-recursively’s
  ;; regexp facility to filter it, because that only checks the
  ;; regexp against the base filenames, and we need to check it
  ;; against filenames relative to org-journal-dir.
  (let ((file-list (directory-files-recursively
                    (file-truename (expand-file-name (file-name-as-directory org-journal-dir))) "\.*"))
        (predicate (lambda (file-path)
                     (and (string-match-p org-journal-file-pattern (file-truename file-path))
                          (or org-journal-encrypt-journal
                              (not (string-match-p "\.gpg$" (file-truename file-path))))))))
    (seq-filter predicate file-list)))

(defun org-journal-list-dates ()
  "Loads the list of files in the journal directory, and converts
  it into a list of calendar DATE elements"
  (mapcar #'org-journal-file-name->calendar-date
          (org-journal-list-files)))

;;;###autoload
(defun org-journal-mark-entries ()
  "Mark days in the calendar for which a diary entry is present"
  (dolist (journal-entry (org-journal-list-dates))
    (if (calendar-date-is-visible-p journal-entry)
        (if (time-less-p (org-journal-calendar-date->time journal-entry)
                         (current-time))
            (calendar-mark-visible-date journal-entry 'org-journal-calendar-entry-face)
          (calendar-mark-visible-date journal-entry 'org-journal-calendar-scheduled-face)))))

;;;###autoload
(defun org-journal-read-entry (arg &optional event)
  "Open journal entry for selected date for viewing"
  (interactive
   (list current-prefix-arg last-nonmenu-event))

  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-read-or-display-entry time nil)))

;;;###autoload
(defun org-journal-display-entry (arg &optional event)
  "Display journal entry for selected date in another
  window (without switсhing to it)"
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-read-or-display-entry time t)))

;; silence compiler warning.
(defvar view-exit-action)

;;;###autoload
(defun org-journal-read-or-display-entry (time &optional noselect)
  "Read an entry for the TIME and either select the new
  window (NOSELECT is nil) or avoid switching (NOSELECT is
  non-nil."
  (let ((org-journal-file (concat (org-journal-get-entry-path time)
                                  (if org-journal-encrypt-journal ".gpg") "")))
    (if (file-exists-p org-journal-file)
        (progn
          ;; open file in view-mode if not opened already
          (let ((had-a-buf (get-file-buffer org-journal-file))
                ;; use find-file... instead of view-file... since
                ;; view-file does not respect auto-mode-alist
                (buf (find-file-noselect org-journal-file)))
            (with-current-buffer buf
              (when (not had-a-buf)
                (view-mode)
                (setq view-exit-action 'kill-buffer))
              (set (make-local-variable 'org-hide-emphasis-markers) t)
              (org-journal-decrypt)
              (if (org-at-heading-p) (org-show-subtree)))
            (if (not noselect)
                (funcall org-journal-find-file org-journal-file)
              (display-buffer buf t))))
      (message "No journal entry for this date."))))

;;;###autoload
(defun org-journal-next-entry ()
  "Go to the next date with a journal entry"
  (interactive)
  (let ((dates (org-journal-list-dates)))
    (while (and dates (not (calendar-date-compare
                            (list (calendar-cursor-to-date)) dates)))
      (setq dates (cdr dates)))
    (if dates (calendar-goto-date (car dates)))))

;;;###autoload
(defun org-journal-previous-entry ()
  "Go to the previous date with a journal entry"
  (interactive)
  (let ((dates (reverse (org-journal-list-dates))))
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

;;; Journal search facilities
;;

;;;###autoload
(defun org-journal-search (str &optional period-name)
  "Search for a string in the journal within a given interval.
See `org-read-date` for information on ways to specify dates.
If a prefix argument is given, search all dates."
  (interactive (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (let* ((period-pair (org-journal-read-period (if current-prefix-arg 'forever period-name)))
         (start (org-journal-calendar-date->time (car period-pair)))
         (end (org-journal-calendar-date->time (cdr period-pair))))
    (org-journal-search-by-string str start end)))

(defvar org-journal-search-history nil)

(defun org-journal-search-calendar-week (str)
  "Search for a string within a current calendar-mode week entries"
  (interactive (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'week))

(defun org-journal-search-calendar-month (str)
  "Search for a string within a current calendar-mode month entries"
  (interactive (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'month))

(defun org-journal-search-calendar-year (str)
  "Search for a string within a current calendar-mode year entries"
  (interactive (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'year))

(defun org-journal-search-forever (str)
  "Search for a string within all entries"
  (interactive (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'forever))

(defun org-journal-search-future (str)
  "Search for a string within all future entries"
  (interactive (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'future))

(defun org-journal-search-future-scheduled ()
  "Search for TODOs within all future entries"
  (interactive)
  (org-journal-search "TODO" 'future))

;; This macro is needed for many of the following functions.
(defmacro org-journal-with-find-file (file &rest body)
  "Executes BODY in FILE. Use this to insert text into FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((current-buffer (current-buffer))
           (buffer-exists (get-buffer (file-name-nondirectory ,file)))
           (result nil))
       (if buffer-exists
           (switch-to-buffer buffer-exists)
         (find-file ,file))
       (setq result (progn ,@body))
       (basic-save-buffer)
      (unless buffer-exists
        (kill-buffer))
      (switch-to-buffer current-buffer)
      result)))

(require 'seq)
(defun org-journal-update-org-agenda-files ()
  "Adds the current and future journal files to org-agenda-files.
And cleans out past org-journal files."
  (when org-journal-enable-agenda-integration
    (let ((agenda-files-without-org-journal
           (seq-filter
            (lambda (fname)
              (not (string-match org-journal-file-pattern fname)))
            (org-agenda-files)))
          (org-journal-agenda-files
           (seq-filter
            ;; skip files that are older than today
            (lambda (fname)
              (not (time-less-p
                    (org-journal-calendar-date->time
                     (org-journal-file-name->calendar-date fname))
                    (time-subtract (current-time) (days-to-time 1)))))
            (org-journal-list-files))))
      (setq org-agenda-files (append agenda-files-without-org-journal
                                     org-journal-agenda-files)))))

(defun org-journal-schedule-view ()
  "Opens a new window with all scheduled journal entries.
Think of this as a faster, less fancy version of your org-agenda."
  (interactive)
  (find-file-other-window "*Org-journal schedule*")
  (view-mode -1)
  (erase-buffer)
  (org-mode)
  (insert "#+TITLE: Org-Journal Schedule\n\n")
  (let* ((schedule-buffer (current-buffer))
         (period-pair (org-journal-read-period 'future))
         (start (org-journal-calendar-date->time (car period-pair)))
         (end (org-journal-calendar-date->time (cdr period-pair)))
         (file-list (org-journal-search-build-file-list start end))
         (search-results nil))
    (dolist (filename (sort file-list
                            (lambda (x y)
                              (time-less-p
                               (org-journal-calendar-date->time
                                (org-journal-file-name->calendar-date x))
                               (org-journal-calendar-date->time
                                (org-journal-file-name->calendar-date y))))))
      (let ((time (org-journal-calendar-date->time
                   (org-journal-file-name->calendar-date filename)))
            (copy-mapper (lambda () (let ((subtree (org-journal-extract-current-subtree nil)))
                 ;; since the next subtree now starts at point,
                 ;; continue mapping from before that, to include it
                 ;; in the search
                 (backward-char)
                 (setq org-map-continue-from (point))
                 subtree)))
            (content-to-copy nil))
        (if (functionp org-journal-date-format)
            (insert (funcall org-journal-date-format time))
          (insert org-journal-date-prefix
                  (format-time-string org-journal-date-format time)
                  "\n"))
        (org-journal-with-find-file
         filename
         (setq content-to-copy (org-map-entries copy-mapper
                                                "+TIMESTAMP>=\"<now>\"|+SCHEDULED>=\"<now>\"")))
        (if content-to-copy
            (insert (mapconcat 'identity content-to-copy "") "\n")
          (insert "N/A\n"))))
    (set-buffer-modified-p nil)
    (view-mode t)
    (goto-char (point-min))))

(defun org-journal-read-period (period-name)
  "If the PERIOD-NAME is nil, then ask the user for period
start/end; if PERIOD-NAME is 'forever, set the period from the
beginning of time to eternity; if PERIOD-NAME is a symbol equal
to 'week/'month/'year then use current week/month/year from the
calendar accordingly."
  (cond
   ;; no period-name? ask the user for input
   ((not period-name)
    (let* ((org-read-date-prefer-future nil)
           (absolute-start (time-to-days (org-read-date nil t nil "Enter a period start")))
           (absolute-end (time-to-days (org-read-date nil t nil "Enter a period end")))
           (start (calendar-gregorian-from-absolute absolute-start))
           (end (calendar-gregorian-from-absolute absolute-end)))
      (cons start end)))

   ;; eternity start/end
   ((eq period-name 'forever)
    (cons (list 1 1 1971)
          (list 12 31 2030)))

   ;; future start/end
   ((eq period-name 'future)
    (let ((date (decode-time (current-time))))
         (cons (list (nth 4 date) (nth 3 date) (nth 5 date))
               (list 12 31 2030))))

   ;; extract a year start/end using the calendar curson
   ((and (eq period-name 'year) (eq major-mode 'calendar-mode))
    (calendar-cursor-to-nearest-date)
    (let* ((date (calendar-cursor-to-date))
           (year (calendar-extract-year date))
           (jan-first (list 1 1 year))
           (dec-31 (list 12 31 year)))
      (cons jan-first
            dec-31)))

   ;; month start/end
   ((and (eq period-name 'month) (eq major-mode 'calendar-mode))
    (calendar-cursor-to-nearest-date)
    (let* ((date (calendar-cursor-to-date))
           (year (calendar-extract-year date))
           (month (calendar-extract-month date))
           (last-day (calendar-last-day-of-month month year)))
      (cons (list month 1 year)
            (list month last-day year))))

   ;; week start/end
   ((and (eq period-name 'week) (eq major-mode 'calendar-mode))
    (calendar-cursor-to-nearest-date)
    (let* ((date (calendar-cursor-to-date))
           (absoluteday (calendar-absolute-from-gregorian date))
           (weekday (calendar-day-of-week date))
           (zerobased-weekday (- weekday calendar-week-start-day))
           (absolute-start (- absoluteday zerobased-weekday))
           (absolute-end (+ absoluteday (- 7 zerobased-weekday)))
           (start (calendar-gregorian-from-absolute absolute-start))
           (end (calendar-gregorian-from-absolute absolute-end)))
      (cons start end)))

   (t (error "Wrong period-name given or not in the calendar mode"))))

(defun org-journal-search-by-string (str &optional period-start period-end)
  "Search for a string within a given time interval.
if no string is given, search for all entries using
org-journal-time-prefix."
  (when (time-less-p period-end period-start)
    (error "Period end cannot be before the start"))
  (let* ((search-str (if (string= "" str) org-journal-time-prefix str))
         (files (org-journal-search-build-file-list period-start period-end))
         (results (org-journal-search-do-search search-str files)))
    (with-current-buffer-window
     "*Org-journal search*" nil nil
     (org-journal-search-print-results str results period-start period-end))))

(defun org-journal-search-build-file-list (&optional period-start period-end)
  "Build a list of journal files within a given time interval"
  (let ((files (org-journal-list-files))
        result)
    (dolist (file files)
      (let ((filetime (org-journal-calendar-date->time
                       (org-journal-file-name->calendar-date file))))
        (cond ((not (and period-start period-end))
               (push file result))

              ((and period-start period-end
                    (time-less-p period-start filetime)
                    (time-less-p filetime period-end))
               (push file result))

              ((and period-start
                    (not period-end)
                    (time-less-p period-start filetime))
               (push file result))

              ((and period-end
                    (not period-start)
                    (time-less-p filetime period-end))
               (push file result)))))
    result))

(defun org-journal-search-do-search (str files)
  "Search for a string within a list of files, return match pairs (PATH . LINENUM)"
  (let (results)
    (dolist (fname (reverse files))
      (with-temp-buffer
        (insert-file-contents fname)
        (when org-journal-enable-encryption
          (org-decrypt-entry))
        (while (search-forward str nil t)
          (let* ((fullstr (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
                 (res (list fname (line-number-at-pos) fullstr)))
            (push res results)))))
    (cond
     ((eql org-journal-search-results-order-by :desc) results)
     (t (reverse results)))))

(defun org-journal-format-date (time)
  "Format TIME according to `org-journal-date-format`"
  (if (functionp org-journal-date-format)
      (funcall org-journal-date-format time)
    (format-time-string org-journal-date-format time)))

(defun org-journal-search-print-results (str results period-start period-end)
  "Print search results using text buttons"
  (let ((label-start (org-journal-format-date period-start))
        (label-end (org-journal-format-date period-end)))
    (princ (concat "Search results for \"" str "\" between "
                   label-start " and " label-end
                   ": \n\n")))
  (dolist (res results)
    (let* ((fname (nth 0 res))
           (lnum (nth 1 res))
           (fullstr (nth 2 res))
           (time (org-journal-calendar-date->time
                  (org-journal-file-name->calendar-date fname)))
           (label (org-journal-format-date time))

           (label-end (org-journal-format-date period-start)))

      (insert-text-button label
                          'action 'org-journal-search-follow-link-action
                          'org-journal-link (cons fname lnum))
      (princ "\t")
      (princ fullstr)
      (princ "\n")))
  (org-journal-highlight str)
  (local-set-key (kbd "q") 'kill-this-buffer)
  (local-set-key (kbd "<tab>") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button)
  (local-set-key (kbd "n") 'forward-button)
  (local-set-key (kbd "p") 'backward-button))

(defun org-journal-search-follow-link-action (button)
  "Follow the link using info saved in button properties"
  (let* ((target (button-get button 'org-journal-link))
         (fname (car target))
         (lnum (cdr target)))
    (org-journal-read-or-display-entry
     (org-journal-calendar-date->time
      (org-journal-file-name->calendar-date fname)))
    (outline-show-all) ; TODO: could not find out a proper way to go to a hidden line
    (goto-char (point-min))
    (forward-line (1- lnum))))

(defun org-journal-decrypt ()
  (when (fboundp 'org-decrypt-entries)
    (let ((buffer-read-only nil))
      (org-decrypt-entries))))

(defun org-journal-encryption-hook ()
  "The function added to the hook specified by
  `org-journal-encrypt-on'."
  (when org-journal-enable-encryption
    (org-encrypt-entries)
    (unless (equal org-journal-encrypt-on
                   'before-save-hook)
      (save-buffer))))

;; Setup encryption by default
;;;###autoload
(add-hook 'org-journal-mode-hook
          (lambda () (add-hook org-journal-encrypt-on
                               'org-journal-encryption-hook
                               nil t)))

(provide 'org-journal)

;;; org-journal.el ends here
