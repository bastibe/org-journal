;;; org-journal.el --- a simple org-mode based journaling mode -*- lexical-binding: t; -*-

;; Author: Bastian Bechtold
;;         Christian Schwarzgruber

;; URL: http://github.com/bastibe/org-journal
;; Version: 2.1.2
;; Package-Requires: ((emacs "25.1") (org "9.1"))

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
;; In calendar view: j m to mark entries in calendar
;;                   j r to view an entry in a new buffer
;;                   j d to view an entry but not switch to it
;;                   j n to add a new entry
;;                   j s w to search all entries of the current week
;;                   j s m to search all entries of the current month
;;                   j s y to search all entries of the current year
;;                   j s f to search all entries of all time
;;                   j s F to search all entries in the future
;;                   [ to go to previous entry
;;                   ] to go to next entry
;; When viewing a journal entry: C-c C-b to view previous entry
;;                               C-c C-f to view next entry


;;; Code:

(require 'cal-iso)
(require 'epa)
(require 'org)
(require 'org-crypt)
(require 'seq)
(require 'subr-x)

;; Silent byte-compiler
(defvar view-exit-action)
(declare-function org-collect-keywords "org")

(when (version< org-version "9.2")
  (defalias 'org-set-tags-to 'org-set-tags))

(unless (fboundp 'org--tag-add-to-alist)
  ;; This function can be removed once emacs-26 es required or de-facto standard.
  (defun org-tag-add-to-alist (alist1 alist2)
    "Append ALIST1 elements to ALIST2 if they are not there yet.

From branch \"emacs-26\", added for compatibility.
"
    (cond
      ((null alist2) alist1)
      ((null alist1) alist2)
      (t (let ((alist2-cars (mapcar (lambda (x) (car-safe x)) alist2))
               to-add)
           (dolist (i alist1)
             (unless (member (car-safe i) alist2-cars)
               (push i to-add)))
           (append to-add alist2)))))
  (defalias 'org--tag-add-to-alist 'org-tag-add-to-alist))

;;; Customizable variables
(defgroup org-journal nil
  "Settings for the personal journal"
  :group 'org
  :group 'org-journal)

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

(defcustom org-journal-file-type 'daily
  "What type of journal file to create.

When switching from daily to weekly, monthly, yearly, or from weekly,
monthly, yearly to daily, you need to invalidate the cache. This has currently
to be done manually by calling `org-journal-invalidate-cache'."
  :type '(choice
          (const :tag "Daily" daily)
          (const :tag "Weekly" weekly)
          (const :tag "Monthly" monthly)
          (const :tag "Yearly" yearly)))

(defcustom org-journal-start-on-weekday 1
  "When `org-journal-file-type' is set to 'weekly, start the week on this day.

1 for Monday, ..., and 7 for Sunday."
  :type '(choice
          (const :tag "Monday" 1)
          (const :tag "Tuesday" 2)
          (const :tag "Wednesday" 3)
          (const :tag "Thursday" 4)
          (const :tag "Friday" 5)
          (const :tag "Saturday" 6)
          (const :tag "Sunday" 7)))

(defcustom org-journal-dir "~/Documents/journal/"
  "Directory containing journal entries."
  :type 'directory
  :risky t)

(defcustom org-journal-file-format "%Y%m%d"
  "Format string for journal file names (Default \"YYYYMMDD\").

This pattern MUST include `%Y', `%m' and `%d' when `org-journal-file-type' is
`daily' or `weekly'. When `org-journal-file-type' is `monthly' this pattern
MUST at least include `%Y' and `%m', and at least `%Y' when
`org-journal-file-type' is `yearly'.

Currently supported placeholders are:

%Y is the year as decimal number, including the century.
%m is the month as a decimal number (range 01 to 12).
%d is the day as a decimal number (range 01 to 31).
%V is the ISO 8601 week number as a decimal number (range 01 to 53).
%a is the locale’s abbreviated name of the day of week, %A the full name.
%b is the locale's abbreviated name of the month, %B the full name.
%F is the ISO 8601 date format (equivalent to \"%Y-%m-%d\")."
  :type 'string)

(defcustom org-journal-date-format "%A, %x"
  "Format string for date entries.

By default \"WEEKDAY, DATE\", where DATE is what Emacs thinks is an
appropriate way to format days in your language.
If the value is a function, the function will be evaluated and the return
value will be inserted."
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defcustom org-journal-search-result-date-format "%A, %x"
  "Date format string for search result.

By default \"WEEKDAY, DATE\", where DATE is what Emacs thinks is an
appropriate way to format days in your language."
  :type 'string)

(defcustom org-journal-date-prefix "* "
  "Prefix for `org-journal-date-format'.

The default prefix creates an `org-mode' heading.  This default
should not be changed for weekly, monthly or yearly journal
files.  An alternative for daily journal files could be
\"#+title: \" creating a title rather than a heading.  To create
a \"#+title: \" for weekly, monthly or yearly (but also daily)
journal files, customize `org-journal-file-header' instead."
  :type 'string)

(defcustom org-journal-time-format "%R "
  "Format string for time entries.

By default HH:MM. Set it to a blank string if you want to disable timestamps."
  :type 'string)

(defcustom org-journal-time-format-post-midnight ""
  "When non-blank, a separate time format string for after midnight.

When the current time is before the hour set by `org-extend-today-until'."
  :type 'string)

(defcustom org-journal-time-prefix "** "
  "String that is put before every time entry in a journal file.

By default, this is an org-mode sub-heading."
  :type 'string)

(defcustom org-journal-hide-entries-p t
  "If true all but the current entry will be hidden when creating a new one."
  :type 'boolean)

(defcustom org-journal-enable-encryption nil
  "Add `org-crypt-tag-matcher' tag for encrypted entries when non-nil.

Whenever a user saves/opens these journal entries, Emacs asks a user
passphrase to encrypt/decrypt it."
  :type 'boolean)

(defcustom org-journal-encrypt-journal nil
  "If non-nil, encrypt journal files using gpg.

The journal files will have the file extension \".gpg\"."
  :type 'boolean)

(defcustom org-journal-encrypt-on 'before-save-hook
  "Hook on which to encrypt entries.

It can be set to other hooks like `kill-buffer-hook'."
  :type 'function)

(defcustom org-journal-enable-agenda-integration nil
  "Add current and future org-journal files to `org-agenda-files' when non-nil."
  :type 'boolean)

(defcustom org-journal-find-file 'find-file-other-window
  "The function to use when opening an entry.

Set this to `find-file' if you don't want org-journal to split your window."
  :type 'function)

(defcustom org-journal-carryover-items "TODO=\"TODO\""
  "Carry over items that match these criteria.

  See agenda tags view match description for the format of this."
  :type 'string)

(defcustom org-journal-skip-carryover-drawers nil
  "By default, we carry over all the drawers associated with the items.

This option can be used to skip certain drawers being carried over.
The drawers listed here will be wiped completely, when the item gets carried
over."
  :type 'list)

(defcustom org-journal-handle-old-carryover 'org-journal-delete-old-carryover
  "The function to handle the carryover entries in the previous journal.

This function takes one argument, which is a list of the carryover entries
in the journal of previous day.
The list is in form of ((START_POINT (END_POINT . \"TEXT\")) ...);
and in ascending order of START_POINT."
  :type 'function)

(defcustom org-journal-carryover-delete-empty-journal 'never
  "Delete empty journal entry/file after carryover.

Default is to `never' delete an empty journal entry/file. Other options
are `always', i.e. don't prompt, just delete or `ask'"
  :type '(choice
          (const :tag "never" never)
          (const :tag "always" always)
          (const :tag "ask" ask)))

(defcustom org-journal-search-results-order-by :asc
  "Journal entry search order.

Search gets sorted by date either ascending :asc, or descending :desc."
  :type 'symbol)

(defcustom org-journal-tag-alist nil
  "Default tags for use in Org-Journal mode.

This is analogous to `org-tag-alist', and uses the same format.
If nil, then `org-tag-alist' is used instead.
This can also be overridden on a file-local level by using a “#+TAGS:” keyword."
  :type (get 'org-tag-alist 'custom-type))

(defcustom org-journal-tag-persistent-alist nil
  "Persistent tags for use in Org-Journal mode.

This is analogous to `org-tag-persistent-alist', and uses the same
format. If nil, the default, then `org-tag-persistent-alist' is used
instead. These tags cannot be overridden with a “#+TAGS:” keyword, but
they can be disabled per-file by adding the line “#+STARTUP: noptag”
anywhere in your file."
  :type (get 'org-tag-persistent-alist 'custom-type))

(defcustom org-journal-search-forward-fn 'search-forward
  "The function used by `org-journal-search'.

Other possible value is e.g. `re-search-forward'."
  :type 'function)

(defcustom org-journal-follow-mode nil
  "If `t', follow journal entry in calendar."
  :type 'boolean)

(defcustom org-journal-enable-cache nil
  "If `t', journal entry dates will be cached for faster calendar operations."
  :type 'boolean)

(defcustom org-journal-file-header ""
  "A string which should be inserted at the top of a new journal file.

The string will be passed to `format-time-string' along with the time
of the new journal entry.

The value can also be a function expecting a time value."
  :type '(choice
          (string :tag "String")
          (function :tag "Function")))

(defcustom org-journal-created-property-timestamp-format "%Y%m%d"
  "The created property timestamp format-string.

We must be able to reconstruct the timestamp from year,
month and day.

Currently supported placeholders are:

%Y is the year as decimal number, including the century.
%m is the month as a decimal number (range 01 to 12).
%d is the day as a decimal number (range 01 to 31).
%V is the ISO 8601 week number as a decimal number (range 01 to 53).
%a is the locale’s abbreviated name of the day of week, %A the full name.
%b is the locale's abbreviated name of the month, %B the full name.
%F is the ISO 8601 date format (equivalent to \"%Y-%m-%d\").

You must call `org-journal-convert-created-property-timestamps' afterwards,
if you have existing journal entries."
  :type 'string)

(defcustom org-journal-prefix-key "C-c C-"
  "The default prefix key inside `org-journal-mode'.

This variable needs to set before `org-journal' gets loaded.
When this variable is set to an empty string or `nil' no bindings will
be made.

This prefix key is used for:
- `org-journal-next-entry' (key \"f\")
- `org-journal-previous-entry' (key \"b\")
- `org-journal-new-entry' (key \"j\")
- `org-journal-search' (key \"s\")"
  :type 'string)

(defvar org-journal-after-entry-create-hook nil
  "Hook called after journal entry creation.")

(defvar org-journal-after-header-create-hook nil
  "Hook called after journal header creation.
The header is the string described by `org-journal-date-format'.
This runs once per date, before `org-journal-after-entry-create-hook'.")

(defvar org-journal--search-buffer "*Org-journal search*")


;;;###autoload
(add-hook 'calendar-today-visible-hook 'org-journal-mark-entries)
;;;###autoload
(add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

;; Journal mode definition
;;;###autoload
(define-derived-mode org-journal-mode org-mode
  "Journal"
  "Mode for writing or viewing entries written in the journal."
  (turn-on-visual-line-mode)
  (add-hook 'after-save-hook 'org-journal-after-save-hook nil t)
  (when (or org-journal-tag-alist org-journal-tag-persistent-alist)
    (org-journal--set-current-tag-alist))
  (run-mode-hooks))

;;;###autoload
(define-obsolete-function-alias 'org-journal-open-next-entry 'org-journal-next-entry "2.1.0")
;;;###autoload
(define-obsolete-function-alias 'org-journal-open-previous-entry 'org-journal-previous-entry "2.1.0")

;; Key bindings
(when (and (stringp org-journal-prefix-key) (not (string-empty-p org-journal-prefix-key)))
  (let ((command-table '(("f" . org-journal-next-entry)
                         ("b" . org-journal-previous-entry)
                         ("j" . org-journal-new-entry)
                         ("s" . org-journal-search)))
        (key-func (if (string-prefix-p "\\" org-journal-prefix-key)
                      #'concat
                    (lambda (prefix key) (kbd (concat prefix "" key))))))
    (cl-loop for (key . command) in command-table
      do (define-key org-journal-mode-map (funcall key-func org-journal-prefix-key key) command))))

(eval-after-load "calendar"
  '(progn
    (define-key calendar-mode-map (kbd "j m") 'org-journal-mark-entries)
    (define-key calendar-mode-map (kbd "j r") 'org-journal-read-entry)
    (define-key calendar-mode-map (kbd "j d") 'org-journal-display-entry)
    (define-key calendar-mode-map "]" 'org-journal-next-entry)
    (define-key calendar-mode-map "[" 'org-journal-previous-entry)
    (define-key calendar-mode-map (kbd "j n") 'org-journal-new-date-entry)
    (define-key calendar-mode-map (kbd "j s f") 'org-journal-search-forever)
    (define-key calendar-mode-map (kbd "j s F") 'org-journal-search-future)
    (define-key calendar-mode-map (kbd "j s w") 'org-journal-search-calendar-week)
    (define-key calendar-mode-map (kbd "j s m") 'org-journal-search-calendar-month)
    (define-key calendar-mode-map (kbd "j s y") 'org-journal-search-calendar-year)))

(global-set-key (kbd "C-c C-j") 'org-journal-new-entry)

(defmacro org-journal--with-journal (file &rest body)
  "Opens JOURNAL-FILE in fundamental mode, or switches to the buffer which is visiting JOURNAL-FILE.

Returns the last value from BODY. If the buffer didn't exist before it will be deposed."
  ;; Use find-file... instead of view-file... since
  ;; view-file does not respect auto-mode-alist
  (declare (indent 1))
  `(let* ((buffer-exists (get-buffer (file-name-nondirectory ,file)))
          (buf (if buffer-exists buffer-exists
                 (generate-new-buffer (file-name-nondirectory ,file))))
          result)
     (with-current-buffer buf
       (unless buffer-exists
         (insert-file-contents ,file))
       (setq result (progn ,@body)))
     (unless buffer-exists
       (kill-buffer buf))
     result))
(def-edebug-spec org-journal--with-journal (form body))

(defun org-journal-after-save-hook ()
  "Update agenda files and dates."
  (org-journal--update-org-agenda-files)
  (org-journal--dates-puthash)
  (org-journal--serialize))

(defun org-journal-is-journal ()
  "Determine if file is a journal file."
  (and (buffer-file-name)
       (string-match (org-journal--dir-and-file-format->pattern) (buffer-file-name))))

;; Open files in `org-journal-mode' if `org-journal-is-journal' returns true.
(add-to-list 'magic-mode-alist '(org-journal-is-journal . org-journal-mode))

(defun org-journal--dir-and-file-format->pattern ()
  "Return the current journal file pattern"
  (concat (file-name-as-directory (file-truename org-journal-dir))
          (org-journal--format->regex org-journal-file-format)
          "\\(\\.gpg\\)?\\'"))

(defvar org-journal--format-rx-alist
  '(("%[aAbB]" . "\\\\(?4:[a-zA-Z]\\\\{3,\\\\}\\\\)")
    ("%d" . "\\\\(?3:[0-9]\\\\{2\\\\}\\\\)")
    ("%m" . "\\\\(?2:[0-9]\\\\{2\\\\}\\\\)")
    ("%Y" . "\\\\(?1:[0-9]\\\\{4\\\\}\\\\)")
    ("%V" . "[0-9]\\\\{2\\\\}")))

(defun org-journal--format->regex (format)
  (cl-loop
    initially (setq format (regexp-quote (replace-regexp-in-string "%F" "%Y-%m-%d" format t)))
    for (fmt . rx) in org-journal--format-rx-alist
    do (setq format (replace-regexp-in-string fmt rx format t))
    finally return format))

(defvar org-journal--created-re "^ *:CREATED: +.*$"  "Regex to find created property.")

(defun org-journal--search-forward-created (date &optional bound noerror count)
  "Search for CREATED tag with date."
  (re-search-forward
   (concat "[ \t]*:CREATED:[ \t]+"
           (format-time-string
            (regexp-quote org-journal-created-property-timestamp-format)
            (org-journal--calendar-date->time date))
           "[ \t]*$")
   bound noerror count))

(defsubst org-journal--daily-p ()
  "Returns t if `org-journal-file-type' is set to `'daily'."
  (eq org-journal-file-type 'daily))

(defun org-journal--is-date-prefix-org-heading-p ()
  "Returns t if `org-journal-date-prefix' starts with \"* \"."
  (eq 0 (string-match "^\* " org-journal-date-prefix)))

;;;###autoload
(defun org-journal-convert-created-property-timestamps (old-format)
  "Convert CREATED property timestamps to `org-journal-created-property-timestamp-format'."
  (interactive "sEnter old format: ")
  (if (org-journal--daily-p)
      (message "Nothing to do, org-journal-file-type is 'daily")
    (dolist (file (org-journal--list-files))
      (let* ((inhibit-read-only)
             (buffer (get-buffer (file-name-nondirectory file)))
             (buffer-modefied (when buffer (buffer-modified-p buffer))))
        (with-current-buffer (if buffer buffer (find-file-noselect file))
          (goto-char (point-min))
          (ignore-errors
            (dolist (date (reverse (let ((org-journal-created-property-timestamp-format old-format))
                                     (org-journal--file->calendar-dates file))))
              (unless (let ((org-journal-created-property-timestamp-format old-format))
                        (org-journal--search-forward-created date nil t))
                (error "Didn't find journal entry in file (%s), date was (%s) " file date))
              (org-set-property "CREATED" (format-time-string
                                           org-journal-created-property-timestamp-format
                                           (org-journal--calendar-date->time date)))))
          (unless buffer-modefied (save-buffer))
          (unless buffer (kill-buffer)))))))

(defun org-journal--convert-time-to-file-type-time (&optional time)
  "Converts TIME to the file type format date.

If `org-journal-file-type' is 'weekly, the TIME will be rounded to
the first date of the week.

If `org-journal-file-type' is 'monthly, the TIME will be rounded to
the first date of the month.

If `org-journal-file-type' is 'yearly, the TIME will be rounded to
the first date of the year."
  (or time (setq time (current-time)))
  (pcase org-journal-file-type
    ;; Do nothing for daily
    (`daily time)
    ;; Round to the monday of the current week, e.g. 20181231 is the first week of 2019
    (`weekly
     (let* ((absolute-monday
	     (calendar-iso-to-absolute
	      (mapcar 'string-to-number
		      (split-string (format-time-string "%V 1 %G" time) " "))))
	    (absolute-now
	     (calendar-absolute-from-gregorian
	      (mapcar 'string-to-number
		      (split-string (format-time-string "%m %d %Y" time) " "))))
	    (target-date
	     (+ absolute-monday
		(- org-journal-start-on-weekday 1)))
	    (date
             (calendar-gregorian-from-absolute
              (if (> target-date absolute-now)
		  (- target-date 7)
		target-date))))
       (org-journal--calendar-date->time date)))
    ;; Round to the first day of the month, e.g. 20190301
    (`monthly
     (org-journal--calendar-date->time
      (mapcar 'string-to-number (split-string (format-time-string "%m 1 %Y" time) " "))))
    ;; Round to the first day of the year, e.g. 20190101
    (`yearly
     (org-journal--calendar-date->time
      (mapcar 'string-to-number (split-string (format-time-string "1 1 %Y" time) " "))))))

(defun org-journal--get-entry-path (&optional time)
  "Return the path to an entry matching TIME, if no TIME is given, uses the current time."
  (let ((file (file-truename
               (expand-file-name
                (format-time-string org-journal-file-format
                                    (org-journal--convert-time-to-file-type-time time))
                org-journal-dir))))
    (when (and org-journal-encrypt-journal (not (file-exists-p file)))
      (setq file (concat file ".gpg")))
    file))

(defun org-journal--create-journal-dir ()
  "Create the `org-journal-dir'."
  (unless (file-exists-p org-journal-dir)
    (if (yes-or-no-p (format
                      "Journal directory %s doesn't exists. Create it? "
                      (file-truename org-journal-dir)))
        (make-directory (file-truename org-journal-dir) t)
      (user-error "A journal directory is necessary to use org-journal"))))

(defun org-journal--sanity-checks ()
  "Do some sanity checks."
  (unless (symbolp org-journal-file-type)
    (user-error
     "The value of `org-journal-file-type' must be symbol, not a %s"
     (type-of org-journal-file-type))))

(defun org-journal--set-current-tag-alist ()
  "Set `org-current-tag-alist' for the current journal file.
This allows the use of `org-journal-tag-alist' and
`org-journal-tag-persistent-alist', which when non-nil override
`org-tag-alist' and `org-journal-tag-persistent-alist' respectively."
  (setq org-current-tag-alist ; this var is always buffer-local
        (org--tag-add-to-alist
         (or org-journal-tag-persistent-alist org-tag-persistent-alist)
         ;; TODO: Remove this once org 9.3.7 is required
         ;; `org--setup-collect-keywords' was removed between version 9.3.6 and 9.3.7,
         ;; and is now called `org-collect-keywords', which has a different signature.
         (let* ((alist (if (fboundp 'org--setup-collect-keywords)
                           (org--setup-collect-keywords
                            (org-make-options-regexp
                             '("FILETAGS" "TAGS" "SETUPFILE")))
                         (org-collect-keywords '("FILETAGS" "TAGS"))))
                (tags (cdr (assq 'tags alist))))
           (if (and alist tags)
               (org-tag-string-to-alist tags)
             (or org-journal-tag-alist org-tag-alist))))))

(defun org-journal--calendar-date-compare (date1 date2)
  "Return t if DATE1 is before DATE2, nil otherwise."
  (< (calendar-absolute-from-gregorian date1)
     (calendar-absolute-from-gregorian date2)))

(defun org-journal--insert-header (time)
  "Insert `org-journal-file-header'."
  (when (and (or (functionp org-journal-file-header)
                 (and (stringp org-journal-file-header)
                      (not (string-empty-p org-journal-file-header))))
             (= (buffer-size) 0))
    (insert (if (functionp org-journal-file-header)
                (funcall org-journal-file-header time)
              (format-time-string org-journal-file-header time)))
    (save-excursion
      (when (re-search-backward "^#\\+" nil t)
        (org-ctrl-c-ctrl-c)))))

(defun org-journal--insert-entry-header (time)
  "Create new journal entry if there isn't one."
  (let ((entry-header
         (if (functionp org-journal-date-format)
             (funcall org-journal-date-format time)
           (when (string-empty-p org-journal-date-format)
             (user-error "org-journal-date-format is empty, this won't work"))
           (concat org-journal-date-prefix
                   (format-time-string org-journal-date-format time)))))
    (goto-char (point-min))
    (unless (if (org-journal--daily-p)
                (or (search-forward entry-header nil t) (and (goto-char (point-max)) nil))
              (cl-loop
                with date = (decode-time time)
                with file-dates = (sort (org-journal--file->calendar-dates (buffer-file-name))
                                        (lambda (a b)
                                          (org-journal--calendar-date-compare b a)))
                with entry
                initially (setq date (list (nth 4 date) (nth 3 date) (nth 5 date)))
                unless file-dates ;; New entry at bof
                do
                  (unless (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
                    (goto-char (point-max)))
                  (if (org-at-heading-p)
                      (progn
                        (beginning-of-line)
                        (insert "\n")
                        (forward-line -1))
                    (forward-line -1)
                    (end-of-line))
                and return nil
                while file-dates
                do
                  (setq entry (car file-dates)
                        file-dates (cdr file-dates))
                if (or (org-journal--calendar-date-compare entry date) (equal entry date))
                do
                  (org-journal--search-forward-created entry)
                  (when (org-journal--calendar-date-compare entry date) ;; New entry at eof, or somewhere in-between
                    (org-end-of-subtree))
                and return (equal entry date))) ;; If an entry exists don't create a header


      (when (looking-back "[^\t ]" (point-at-bol))
        (insert "\n"))
      (insert entry-header)

      ;; Create CREATED property for weekly, monthly, and yearly journal entries
      (unless (org-journal--daily-p)
        (org-set-property "CREATED"
                          (format-time-string
                           org-journal-created-property-timestamp-format time)))

      (when org-journal-enable-encryption
        (unless (member org-crypt-tag-matcher (org-get-tags))
          (org-set-tags org-crypt-tag-matcher)))
      (run-hooks 'org-journal-after-header-create-hook))))

(defun org-journal--insert-entry (time org-extend-today-until-active-p)
  "Insert a new entry."
  (unless (eq (current-column) 0) (insert "\n"))
  (let* ((day-discrepancy (- (time-to-days (current-time)) (time-to-days time)))
         (timestamp (cond
                      ;; “time” is today, use normal timestamp format
                      ((= day-discrepancy 0)
                       (format-time-string org-journal-time-format))
                      ;; “time” is yesterday with org-extend-today-until,
                      ;; use different timestamp format if available
                      ((and (= day-discrepancy 1) org-extend-today-until-active-p)
                       (if (not (string-equal org-journal-time-format-post-midnight ""))
                           (format-time-string org-journal-time-format-post-midnight)
                         (format-time-string org-journal-time-format)))
                      ;; “time” is on some other day, use blank timestamp
                      (t ""))))
    (insert org-journal-time-prefix timestamp))
  (run-hooks 'org-journal-after-entry-create-hook))

;;;###autoload
(defun org-journal-new-entry (prefix &optional time)
  "Open today's journal file and start a new entry.

With a PREFIX arg, open the today's file, create a heading if it doesn't exist yet,
but do not create a new entry.

If given a TIME, create an entry for the time's day. If no TIME was given,
use the current time (which is interpreted as belonging to yesterday if
smaller than `org-extend-today-until').

Whenever a journal entry is created the `org-journal-after-entry-create-hook'
hook is run."
  (interactive "P")
  (org-journal--sanity-checks)
  (org-journal--create-journal-dir)

  ;; If time is before org-extend-today-until, interpret it as
  ;; part of the previous day:
  (let* ((now (decode-time nil))
         (org-extend-today-until-active-p (and (not time) (< (nth 2 now) org-extend-today-until)))
         (entry-path)
         (should-add-entry-p (not prefix)))
    (when org-extend-today-until-active-p
      (setq time (encode-time (nth 0 now)
                              (nth 1 now)
                              (nth 2 now)
                              (1- (nth 3 now))
                              (nth 4 now)
                              (nth 5 now)
                              (nth 8 now))))
    (setq entry-path (org-journal--get-entry-path time))

    ;; Open journal file
    (unless (string= entry-path (buffer-file-name))
      (funcall org-journal-find-file entry-path))

    ;; Insure `view-mode' is not active
    (view-mode -1)

    (org-journal--insert-header time)
    (org-journal--insert-entry-header time)
    (org-journal--decrypt)

    ;; Move TODOs from previous day to new entry
    (when (and org-journal-carryover-items
               (not (string-blank-p org-journal-carryover-items))
               (string= entry-path (org-journal--get-entry-path (current-time))))
      (org-journal--carryover))

    (if (org-journal--is-date-prefix-org-heading-p)
        (outline-end-of-subtree)
      (goto-char (point-max)))

    (when should-add-entry-p
      (org-journal--insert-entry time org-extend-today-until-active-p))

    (if (and org-journal-hide-entries-p (org-journal--time-entry-level))
        (outline-hide-sublevels (org-journal--time-entry-level))
      (save-excursion (org-journal--finalize-view)))

    (when should-add-entry-p
      (outline-show-entry))))

(defvar org-journal--kill-buffer nil
  "Will be set to the `t' if `org-journal--open-entry' is visiting a
buffer not open already, otherwise `nil'.")

(defun org-journal--empty-journal-p (prev-buffer)
  (let (entry)
    (with-current-buffer prev-buffer (save-buffer))
    (save-excursion
      (org-journal--open-entry t t)
      (setq entry (if (org-journal--is-date-prefix-org-heading-p)
                      (org-get-entry)
                    (buffer-substring-no-properties (point) (point-max)))))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (let (start end)
        ;; Delete scheduled timestamps
        (while (re-search-forward (concat " *\\(CLOSED\\|DEADLINE\\|SCHEDULED\\): *" org-ts-regexp-both) nil t)
          (kill-region (match-beginning 0) (match-end 0)))

        ;; Delete drawers
        (while (re-search-forward org-drawer-regexp nil t)
          (setq start (match-beginning 0))
          (re-search-forward org-drawer-regexp nil t)
          (setq end (match-end 0))
          (kill-region start end)))
      (string-empty-p (org-trim (buffer-string))))))

(defun org-journal--remove-drawer ()
  "Removes the drawer configured via `org-journal-skip-carryover-drawers'"
  (save-excursion
    (save-restriction
      (unless (org-journal--daily-p)
        (org-narrow-to-subtree))
      (goto-char (point-min))
      (mapc 'delete-matching-lines (mapcar
                                    (lambda (x)
                                      (format ".*%s:[\\n[:ascii:]]+?:END:$" x))
                                    org-journal-skip-carryover-drawers)))))

(defun org-journal--carryover-delete-empty-journal (prev-buffer)
  "Check if the previous entry/file is empty after we carried over the
items, and delete or not delete the empty entry/file based on
`org-journal-carryover-delete-empty-journal'."
  (when (and (org-journal--empty-journal-p prev-buffer)
             (or (and (eq org-journal-carryover-delete-empty-journal 'ask)
                      (y-or-n-p "Delete empty journal entry/file?"))
                 (eq org-journal-carryover-delete-empty-journal 'always)))

    (let ((inhibit-message t))
      ;; Check if the file doesn't contain any other entry, by comparing the
      ;; new filename with the previous entry filename and the next entry filename.
      (if (and (save-excursion
                 (org-journal--open-entry t t)
                 (or (not (org-journal--open-entry t t))
                     (not (eq (current-buffer) prev-buffer))))
               (not (eq (current-buffer) prev-buffer)))
          (progn
            (delete-file (buffer-file-name prev-buffer))
            (kill-buffer prev-buffer)
            (org-journal--list-dates))
        (save-excursion
          (org-journal--open-entry t t)
          (kill-region (point) (progn (outline-end-of-subtree) (point)))
          (save-buffer))))))

(defun org-journal-delete-old-carryover (old_entries)
  "Delete all carryover entries from the previous day's journal.

If the parent heading has no more content, delete it as well."
  (mapc (lambda (x)
          (unless (save-excursion
                    (goto-char (1- (cadr x)))
                    (org-goto-first-child))
            (kill-region (car x) (cadr x))))
        (reverse old_entries)))

(defun org-journal-carryover-items (text entries prev-buffer)
  "Carryover items.

Will insert `entries', and run `org-journal-handle-old-carryover' function
to process the carryover entries in `prev-buffer'."
  (when entries
    (if (org-journal--is-date-prefix-org-heading-p)
        (progn
          (while (org-up-heading-safe))
          (outline-end-of-subtree))
      (goto-char (point-max)))

    ;; Insure `view-mode' is not active
    (view-mode -1)

    (unless (eq (current-column) 0) (insert "\n"))

    (insert text)

    (save-excursion
      (if (org-journal--daily-p)
          (goto-char (point-min))
        (while (org-up-heading-safe)))

      (unless (null org-journal-skip-carryover-drawers)
        (org-journal--remove-drawer))

      (save-excursion
        (while (re-search-forward "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( [a-z]+\\)?\\)>" nil t)
          (unless (save-excursion
                    (goto-char (point-at-bol))
                    (re-search-forward "\\<\\(SCHEDULED\\|DEADLINE\\):" (point-at-eol) t))
            (replace-match
             (format-time-string "%Y-%m-%d %a"
                                 (org-journal--calendar-date->time
                                  (save-match-data
                                    (if (org-journal--daily-p)
                                        (org-journal--file-name->calendar-date (buffer-file-name))
                                      (save-excursion
                                        (while (org-up-heading-safe))
                                        (org-journal--entry-date->calendar-date))))))
             nil nil nil 1)))))

    (outline-end-of-subtree)

    ;; Process carryover entries in the previous day's journal
    (with-current-buffer prev-buffer
      (funcall org-journal-handle-old-carryover entries))))

(defun org-journal--carryover ()
  "Moves all items matching `org-journal-carryover-items' from the
previous day's file to the current file."
  (interactive)
  (let* ((org-journal-find-file 'find-file)
         (mapper (lambda ()
                   (let ((headings (org-journal--carryover-item-with-parents)))
                     ;; Since the next subtree now starts at point,
                     ;; continue mapping from before that, to include it
                     ;; in the search
                     (setq org-map-continue-from (point))
                     headings)))
         carryover-paths prev-buffer)

    ;; Get carryover paths
    (save-excursion
      (save-restriction
        (when (org-journal--open-entry t t)
          (setq prev-buffer (current-buffer))
          (unless (org-journal--daily-p)
            (org-narrow-to-subtree))
          (setq carryover-paths (org-map-entries mapper org-journal-carryover-items)))))

    (when (and prev-buffer carryover-paths)
      (let (cleared-carryover-paths text)
        ;; Construct the text to carryover, and remove any duplicate elements from carryover-paths
        (cl-loop
          for paths in carryover-paths
          with prev-paths
          do (cl-loop
               for path in paths
               with cleared-paths
               count t into counter
               do (when (or (not (and prev-paths (nth counter prev-paths)))
                            (> (car path) (car (nth counter prev-paths))))
                    (setq text (concat text (cddr path)))
                    (if cleared-paths
                        (setcdr (last cleared-paths) (list path))
                      (setq cleared-paths (list path))))
               finally (if cleared-carryover-paths
                           (setcdr (last cleared-carryover-paths) cleared-paths)
                         (setq cleared-carryover-paths cleared-paths))
                 (setq prev-paths paths)))
        (org-journal-carryover-items text cleared-carryover-paths prev-buffer))
      (org-journal--carryover-delete-empty-journal prev-buffer))

    (when org-journal--kill-buffer
      (mapc 'kill-buffer org-journal--kill-buffer)
      (setq org-journal--kill-buffer nil))))

(defun org-journal--carryover-item-with-parents ()
  "Return carryover item inclusive the parents.

      The parents ...            The carryover item
;; ((START END . \"TEXT\") ... (START END . \"TEXT\"))
"
  (let (start end text carryover-item-with-parents)
    (save-excursion
      (while (> (org-outline-level) (org-journal--time-entry-level))
        (org-up-heading-safe)
        (setq start (point)
              end (save-excursion (outline-next-heading) (point))
              text (buffer-substring-no-properties start end))
        (push (cons start (cons end text)) carryover-item-with-parents)))
    (setq start (point-at-bol)
          end (progn (outline-end-of-subtree) (outline-next-heading) (point))
          text (buffer-substring-no-properties start end))
    (setq carryover-item-with-parents (append carryover-item-with-parents (list (cons start (cons end text)))))))

(defun org-journal--time-entry-level ()
  "Return the headline level of time entries based on the number
of leading asterisks in `org-journal-time-prefix'.

Return nil when it's impossible to figure out the level."
  (when (string-match "\\(^\*+\\)" org-journal-time-prefix)
    (length (match-string 1 org-journal-time-prefix))))

(defun org-journal--calendar-date->time (date)
  "Convert a date as returned from the calendar (MONTH DAY YEAR) to a time."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun org-journal--file-name->calendar-date (file-name)
  "Convert an org-journal file name to a calendar date.

Month and Day capture group default to 1."
  (let* ((file-pattern (org-journal--dir-and-file-format->pattern))
         (file-pattern-has-month-p (integerp (string-match "\(\?2:" file-pattern)))
         (file-pattern-has-day-p (integerp (string-match "\(\?3:" file-pattern)))
         (file (file-truename file-name))
         (day 1)
         (month 1)
         year)
    (setq year (string-to-number (replace-regexp-in-string file-pattern "\\1" file t)))
    (when (= year 0)
      (user-error "Failed to extract year from file: %s" file))

    (if (and (not file-pattern-has-month-p)
             (member org-journal-file-type '(daily weekly monthly)))
        (user-error "Failed to extract month from file: %s" file)
      (when file-pattern-has-month-p
        (setq month (string-to-number (replace-regexp-in-string file-pattern "\\2" file t)))))

    (if (and (not file-pattern-has-day-p)
             (member org-journal-file-type '(daily weekly)))
        (user-error "Failed to extract day from file: %s" file)
      (when file-pattern-has-day-p
        (setq day (string-to-number (replace-regexp-in-string file-pattern "\\3" file t)))))

    (list month day year)))

(defun org-journal--entry-date->calendar-date ()
  "Return journal calendar-date from current buffer.

This is the counterpart of `org-journal--file-name->calendar-date' for
'weekly, 'monthly and 'yearly journal files."
  (let ((re (org-journal--format->regex org-journal-created-property-timestamp-format))
        date)
    (setq date (org-entry-get (point) "CREATED"))
    (unless (ignore-errors (string-match re date))
      (user-error "Created property timestamp format \"%s\" doesn't match CREATED property value (%s) from entry at line: %s" org-journal-created-property-timestamp-format date (what-line)))
    (list (string-to-number (match-string 2 date))    ;; Month
          (string-to-number (match-string 3 date))    ;; Day
          (string-to-number (match-string 1 date))))) ;; Year

(defun org-journal--file->calendar-dates (file)
  "Return journal dates from FILE."
  (org-journal--with-journal
      file
    (let (dates)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-journal--created-re nil t)
          (when (= (save-excursion (org-back-to-heading) (org-outline-level)) 1)
            (push (org-journal--entry-date->calendar-date) dates)))
        dates))))

;;;###autoload
(defun org-journal-new-date-entry (prefix &optional event)
  "Open the journal for the date indicated by point and start a new entry.

If the date is not today, it won't be given a time heading. With one prefix (C-u),
don't add a new heading.

If the date is in the future, create a schedule entry, unless two universal prefix
arguments (C-u C-u) are given. In that case insert just the heading."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (or (ignore-errors (org-journal--calendar-date->time (calendar-cursor-to-date t event)))
                   (org-time-string-to-time (org-read-date nil nil nil "Date:")))))
    (if (time-less-p time (current-time))
        (org-journal-new-entry prefix time)
      (org-journal-new-scheduled-entry prefix time))))

;;;###autoload
(defun org-journal-new-scheduled-entry (prefix &optional scheduled-time)
  "Create a new entry in the future with an active timestamp.

With non-nil prefix argument create a regular entry instead of a TODO entry."
  (interactive "P")
  (let ((time (or scheduled-time (org-time-string-to-time (org-read-date nil nil nil "Date:"))))
        org-journal-carryover-items)
    (when (time-less-p time (current-time))
      (user-error "Scheduled time needs to be in the future"))
    (org-journal-new-entry nil time)
    (unless prefix
      (insert "TODO "))
    (save-excursion
      (insert "\n")
      (org-insert-time-stamp time t))))

;;;###autoload
(defun org-journal-reschedule-scheduled-entry (&optional time)
  "Reschedule an entry in the future."
  (interactive "P")
  (or time (setq time (org-time-string-to-time (org-read-date nil nil nil "Data:"))))
  (when (time-less-p time (current-time))
    (user-error "Scheduled time needs to be in the future"))
  (save-excursion
    (save-restriction
      (org-back-to-heading)
      (org-narrow-to-subtree)
      (if (re-search-forward org-ts-regexp (line-end-position 2) t)
          (replace-match "")
        (org-end-of-subtree)
        (insert "\n"))
      (org-insert-time-stamp time)
      (org-cut-subtree))
    (let (org-journal-carryover-items)
      (org-save-outline-visibility t
        (org-journal-new-entry t time)
        (when (looking-back "[^\t ]" (point-at-bol) t)
          (insert "\n"))
        (org-yank)))))

(defun org-journal--goto-entry (date)
  "Goto DATE entry in current journal file."
  (widen)
  (goto-char (point-min))
  (if (org-journal--daily-p)
      (outline-next-visible-heading 1)
    (org-journal--search-forward-created date))
  (org-journal--finalize-view))

(defun org-journal-sort-dates (dates calendar-date prev)
  "Sorts DATES to determine the order of journal entries.

Can be advised/replaced by a user."
  (unless (member calendar-date dates)
    (setq dates (copy-tree dates))
    (cl-loop
      for date in dates
      while (org-journal--calendar-date-compare date calendar-date)
      count t into cnt
      finally (if (> cnt 0)
                  ;; Insert new date into list
                  (setcdr (nthcdr (1- cnt) dates) (cons calendar-date (nthcdr cnt dates)))
                ;; Insert new date at front
                (setq dates (cons calendar-date dates)))))
  ;; Reverse list for previous search.
  (if prev (reverse dates) dates))

(defun org-journal--open-entry (&optional prev no-select)
  "Open journal entry.

If PREV is non-nil, open previous entry instead of next.
If NO-SELECT is non-nil, open it, but don't show it."
  (let* ((calendar-date (if (org-journal--daily-p)
                            (org-journal--file-name->calendar-date (file-truename (buffer-file-name)))
                          (while (org-up-heading-safe))
                          (org-journal--entry-date->calendar-date)))
         (view-mode-p view-mode)
         (dates (org-journal-sort-dates (org-journal--list-dates) calendar-date prev)))
    (while (and dates (car dates)
                (or (if prev
                        (org-journal--calendar-date-compare calendar-date (car dates))
                      (org-journal--calendar-date-compare (car dates) calendar-date))
                    (calendar-date-equal (car dates) calendar-date)))
      (setq dates (cdr dates)))
    (if (and dates (car dates))
        (let ((filename (org-journal--get-entry-path
                         (org-journal--calendar-date->time (car dates)))))
          (if (find-buffer-visiting filename)
              (progn
                (if no-select
                    (set-buffer (find-buffer-visiting filename))
                  (switch-to-buffer (find-buffer-visiting filename)))
                (setq org-journal--kill-buffer nil))
            (push (if no-select
                      (set-buffer (find-file-noselect filename))
                    (find-file filename))
                  org-journal--kill-buffer))
          (org-journal--goto-entry (car dates))
          (view-mode (if view-mode-p 1 -1))
          t)
      nil)))

;;;###autoload
(defun org-journal-open-current-journal-file ()
  "Open the current journal file"
  (interactive)
  (let ((org-journal-file (org-journal--get-entry-path)))
    (if (file-exists-p org-journal-file)
        (progn
          (funcall org-journal-find-file org-journal-file)
          (unless (org-journal--daily-p)
            (let ((last-entry-date (car (org-journal--file->calendar-dates org-journal-file))))
              (when last-entry-date
                (org-journal--goto-entry last-entry-date)))))
      (message "Journal file %s not found" org-journal-file))))

(defun org-journal--list-files ()
  "Returns a list of all files in the journal directory."
  (org-journal--create-journal-dir)
  ;; grab the file list. We can’t use directory-files-recursively’s
  ;; regexp facility to filter it, because that only checks the
  ;; regexp against the base filenames, and we need to check it
  ;; against filenames relative to org-journal-dir.
  (let ((file-list (directory-files-recursively
                    (file-truename (expand-file-name
                                    (file-name-as-directory org-journal-dir))) "\.*"))
        (predicate (lambda (file-path)
                     (and (string-match-p (org-journal--dir-and-file-format->pattern) file-path)
                          (or org-journal-encrypt-journal
                              (not (string-match-p "\.gpg$" file-path)))))))
    (seq-filter predicate file-list)))

(defconst org-journal--cache-file
  (expand-file-name "org-journal.cache" user-emacs-directory)
  "Cache file for `org-journal--dates'.")

(defvar org-journal--dates (make-hash-table :test 'equal)
  "Hash table for journal dates.

The key is a journal date entry, and the value of the key is of the form
\(FILENAME \(FILE MODIFICATION TIME\)\).")

;;;###autoload
(defun org-journal-invalidate-cache ()
  "Clear `org-journal--dates' hash table, and the cache file."
  (interactive)
  (clrhash org-journal--dates)
  (when org-journal-enable-cache
    (org-journal--serialize)))

(defun org-journal--file-modification-time (file)
  (nth 5 (file-attributes file)))

(defun org-journal--dates-puthash (&optional file)
  (or file (setq file (buffer-file-name)))
  (let ((mtime (org-journal--file-modification-time file)))
    (if (org-journal--daily-p)
        (puthash (org-journal--file-name->calendar-date file) (list file mtime) org-journal--dates)
      ;; Remove any key where (car value) equals FILE
      (cl-loop for key being the hash-keys of org-journal--dates
        when (string-equal (car (gethash key org-journal--dates)) file)
        do (remhash key org-journal--dates))
      (dolist (date (org-journal--file->calendar-dates file))
        (puthash date (list file mtime) org-journal--dates)))))

(defun org-journal--serialize ()
  "Write hashmap to file."
  (when org-journal-enable-cache
    (unless (file-directory-p (file-name-directory org-journal--cache-file))
      (make-directory (file-name-directory org-journal--cache-file) t))
    (if (file-writable-p org-journal--cache-file)
        (with-temp-file org-journal--cache-file
          (let (print-length)
            (insert (prin1-to-string org-journal--dates))))
      (error "%s is not writable" org-journal--cache-file)))
  (org-journal--sort-dates))

(defun org-journal--deserialize ()
  "Read hashmap from file."
  (when org-journal-enable-cache
    (with-demoted-errors
        "Error during file deserialization: %S"
      (when (file-exists-p org-journal--cache-file)
        (with-temp-buffer
          (insert-file-contents org-journal--cache-file)
          (setq org-journal--dates (read (buffer-substring (point-at-bol) (point-at-eol))))))))
  (org-journal--sort-dates))

(defvar org-journal--sorted-dates nil)

(defun org-journal--sort-dates ()
  "Flatten and sort dates, and assign the result to `org-journal--sorted-dates'."
  (setq org-journal--sorted-dates (sort (hash-table-keys org-journal--dates) 'org-journal--calendar-date-compare)))

(defun org-journal--list-dates ()
  "Return all journal dates.

The list ((month day year) ...) contains calendar dates, and is sorted
from oldest to newest."
  (let ((files (org-journal--list-files))
        reparse-files serialize-p
        rem-keys)
    (when (hash-table-empty-p org-journal--dates)
      (org-journal--deserialize)
      (when (hash-table-empty-p org-journal--dates)
        (dolist (file files)
          (org-journal--dates-puthash file))
        (setq serialize-p t)))
    ;; Verify modification time is unchanged, if we have already data.
    (unless serialize-p
      (cl-loop
        with (value files-in-hash file)
        for key being the hash-keys of org-journal--dates
        always (setq value (gethash key org-journal--dates)
                     file (car value))
        do
          (unless (member (car value) files)
            (unless (member key rem-keys)
              (push key rem-keys)))
          (unless (member file files-in-hash)
            (push file files-in-hash)
            (unless (equal (cadr value) (org-journal--file-modification-time file))
              (when (and (member file files) (not (member file reparse-files)))
                (push file reparse-files))))
        finally (dolist (file files) ;; Are there any new files
                  (unless (member file files-in-hash)
                    (push file reparse-files)))))
    (when rem-keys
      (dolist (k rem-keys)
        (remhash k org-journal--dates))
      (setq serialize-p t))
    (when reparse-files
      (dolist (f reparse-files)
        (org-journal--dates-puthash f))
      (setq serialize-p t))
    (when serialize-p
      (org-journal--serialize))
    org-journal--sorted-dates))

;;;###autoload
(defun org-journal-mark-entries ()
  "Mark days in the calendar for which a journal entry is present."
  (interactive)
  (when (file-exists-p org-journal-dir)
    (let ((current-time (current-time)))
      (dolist (journal-entry (org-journal--list-dates))
        (if (calendar-date-is-visible-p journal-entry)
            (if (time-less-p (org-journal--calendar-date->time journal-entry)
                             current-time)
                (calendar-mark-visible-date journal-entry 'org-journal-calendar-entry-face)
              (calendar-mark-visible-date journal-entry 'org-journal-calendar-scheduled-face)))))))

;;;###autoload
(defun org-journal-read-entry (_arg &optional event)
  "Open journal entry for selected date for viewing."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal--calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-read-or-display-entry time nil)))

;;;###autoload
(defun org-journal-display-entry (_arg &optional event)
  "Display journal entry for selected date in another window."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal--calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-read-or-display-entry time t)))

(defun org-journal--finalize-view ()
  "Finalize visability of entry."
  (org-journal--decrypt)
  (if (org-journal--is-date-prefix-org-heading-p)
      (progn
        (org-up-heading-safe)
        (org-back-to-heading)
        (outline-hide-other)
        (outline-show-subtree))
    (outline-show-all)))

;;;###autoload
(defun org-journal-read-or-display-entry (time &optional noselect)
  "Read an entry for the TIME and either select the new window when NOSELECT
is nil or avoid switching when NOSELECT is non-nil."
  (let* ((org-journal-file (org-journal--get-entry-path time))
         (buf-exists (find-buffer-visiting org-journal-file))
         buf point)
    (if (and (when (file-exists-p org-journal-file)
               (setq buf (find-file-noselect org-journal-file)))
             ;; If daily continue with than clause of if condition
             (or (org-journal--daily-p)
                 ;; Search for journal entry
                 (with-current-buffer buf
                   (save-mark-and-excursion
                     (goto-char (point-min))
                     (setq time (decode-time time))
                     (setq point (org-journal--search-forward-created
                                  (list (nth 4 time) (nth 3 time) (nth 5 time))
                                  nil t))))))
        (progn
          ;; Use `find-file-noselect' instead of `view-file' as it does not respect `auto-mode-alist'
          (with-current-buffer buf
            ;; Open file in view-mode if not opened already.
            (unless buf-exists
              (view-mode)
              (setq view-exit-action 'kill-buffer))
            (set (make-local-variable 'org-hide-emphasis-markers) t)
            (if (org-journal--daily-p)
                (when (org-journal--is-date-prefix-org-heading-p)
                  (goto-char (point-min))
                  (re-search-forward (concat org-journal-date-prefix
                                             (if (functionp org-journal-date-format)
                                                 (funcall org-journal-date-format time)
                                               (format-time-string org-journal-date-format time)))))
              (goto-char point))
            (org-journal--finalize-view)
            (setq point (point)))
          (if noselect
              (display-buffer buf t)
            (funcall org-journal-find-file org-journal-file))
          (set-window-point (get-buffer-window (find-buffer-visiting org-journal-file)) point)
          buf)
      (message "No journal entry for this date."))))

(defun org-journal--next-entry (&optional prev)
  "Go to next entry.

If prev is non-nil open previous entry instead of next."
  (unless (cond
            ((eq major-mode 'calendar-mode)
             (let ((dates (if prev
                              (reverse (org-journal--list-dates))
                            (org-journal--list-dates))))
               (while (and dates
                           (not (if prev
                                    (org-journal--calendar-date-compare (car dates) (calendar-cursor-to-date))
                                  (org-journal--calendar-date-compare (calendar-cursor-to-date) (car dates)))))
                 (setq dates (cdr dates)))
               (when dates
                 (calendar-goto-date (car dates))
                 (when org-journal-follow-mode
                   (org-journal-display-entry nil)))))
            ((eq major-mode 'org-journal-mode)
             (org-journal--open-entry prev))
            (t
             (user-error
              (concat "org-journal-" (if prev "previous" "next")
                      "-entry called outside calendar/org-journal mode"))))
    (message (concat "No journal entry " (if prev "before" "after") " this one"))))

;;;###autoload
(defun org-journal-next-entry ()
  "Go to the next journal entry."
  (interactive)
  (org-journal--next-entry))

;;;###autoload
(defun org-journal-previous-entry ()
  "Go to the previous journal entry."
  (interactive)
  (org-journal--next-entry t))

;;; Journal search facilities

;;;###autoload
(defun org-journal-search (str &optional period-name)
  "Search for a string in the journal files.

See `org-read-date' for information on ways to specify dates.
If a prefix argument is given, search all dates."
  (interactive
   (list (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (let* ((period-pair (org-journal--read-period (if current-prefix-arg 'forever period-name)))
         (start (org-journal--calendar-date->time (car period-pair)))
         (end (org-journal--calendar-date->time (cdr period-pair))))
    ;; Including period-start in search
    (setcar (cdr start) (1- (cadr start)))
    ;; Including period-end in search
    (setcar (cdr end) (1+ (cadr end)))
    (org-journal--search-by-string str start end)))

(defvar org-journal-search-history nil)

;;;###autoload
(defun org-journal-search-calendar-week (str)
  "Search for a string within a current calendar-mode week entries."
  (interactive
   (list
    (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'week))

;;;###autoload
(defun org-journal-search-calendar-month (str)
  "Search for a string within a current calendar-mode month entries."
  (interactive
   (list
    (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'month))

;;;###autoload
(defun org-journal-search-calendar-year (str)
  "Search for a string within a current calendar-mode year entries."
  (interactive
   (list
    (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'year))

;;;###autoload
(defun org-journal-search-forever (str)
  "Search for a string within all entries."
  (interactive
   (list
    (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'forever))

;;;###autoload
(defun org-journal-search-future (str)
  "Search for a string within all future entries."
  (interactive
   (list
    (read-string "Enter a string to search for: " nil 'org-journal-search-history)))
  (org-journal-search str 'future))

;;;###autoload
(defun org-journal-search-future-scheduled ()
  "Search for TODOs within all future entries."
  (interactive)
  (org-journal-search "TODO" 'future))

;; This macro is needed for many of the following functions.
(defmacro org-journal--with-find-file (file &rest body)
  "Executes BODY in FILE. Use this to insert text into FILE.

The buffer is disposed after the macro exits (unless it already
existed before)."
  (declare (indent 1))
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
(def-edebug-spec org-journal--with-find-file (form body))

(defun org-journal--update-org-agenda-files ()
  "Adds the current and future journal files to `org-agenda-files' containing TODOs,
and cleans out past org-journal files."
  (when org-journal-enable-agenda-integration
    (let ((not-org-journal-agenda-files
           (seq-filter
            (lambda (fname)
              (not (string-match (org-journal--dir-and-file-format->pattern) fname)))
            (org-agenda-files)))
          (org-journal-agenda-files
           (let* ((future (org-journal--read-period 'future))
                  (beg (car future))
                  (end (cdr future)))
             (setcar (cdr beg) (1- (cadr beg))) ;; Include today; required for `org-journal--search-build-file-list'
             (when (< (nth 2 (decode-time (current-time))) org-extend-today-until)
               (setq beg (decode-time (apply #'encode-time `(0 59 -1 ,(nth 1 beg) ,(nth 0 beg) ,(nth 2 beg))))
                     beg (list (nth 4 beg) (nth 3 beg) (nth 5 beg))))
             (org-journal--search-build-file-list
              (org-journal--calendar-date->time beg)
              (org-journal--calendar-date->time end)))))
      (org-store-new-agenda-file-list (append not-org-journal-agenda-files
					      org-journal-agenda-files)))))

(defvar org-journal--schedule-buffer-name "*Org-journal schedule*")

(defun org-journal-schedule-view ()
  "Opens a new window with all scheduled journal entries.

Think of this as a faster, less fancy version of your `org-agenda'."
  (interactive)

  (when (get-buffer org-journal--schedule-buffer-name)
    (kill-buffer org-journal--schedule-buffer-name))

  (with-current-buffer (get-buffer-create org-journal--schedule-buffer-name)
    (org-mode)
    (insert "#+TITLE: Org-Journal Schedule\n\n")
    (goto-char (point-max)))

  (cl-loop
    with copy-mapper = (lambda ()
                         (let ((subtree (org-journal--carryover-item-with-parents)))
                           ;; since the next subtree now starts at point,
                           ;; continue mapping from before that, to include it
                           ;; in the search
                           (backward-char)
                           (setq org-map-continue-from (point))
                           subtree))
    with (content-to-copy journal-buffers)
    with today = (current-time)
    for date in (org-journal--list-dates)
    always (setq date (org-journal--calendar-date->time date))
    when (time-less-p today date)
    do
      (cl-pushnew (org-journal-read-or-display-entry date) journal-buffers)
      (with-current-buffer org-journal--schedule-buffer-name
        (if (functionp org-journal-date-format)
            (insert (funcall org-journal-date-format date))
          (insert org-journal-date-prefix
                  (format-time-string org-journal-date-format date)
                  "\n")))
      (save-restriction
        (org-narrow-to-subtree)
        (setq content-to-copy (org-map-entries
                               copy-mapper
                               "+TIMESTAMP>=\"<now>\"|+SCHEDULED>=\"<now>\"")))
      (when content-to-copy
        (with-current-buffer org-journal--schedule-buffer-name
          (insert (mapconcat (lambda (item) (cddar item)) content-to-copy "")
                  "\n")))
    finally
      (mapc (lambda (b)
              (with-current-buffer b
                (when view-mode
                  (kill-buffer))))
            journal-buffers))

  (with-current-buffer org-journal--schedule-buffer-name
    (set-buffer-modified-p nil)
    (view-mode t)
    (goto-char (point-min)))

  (switch-to-buffer org-journal--schedule-buffer-name))

(defun org-journal--read-period (period-name)
  "Return read period.

If the PERIOD-NAME is nil, then ask the user for period start/end.
If PERIOD-NAME is 'forever, set the period from the beginning of time
to eternity. If PERIOD-NAME is a symbol equal to 'week, 'month or 'year
then use current week, month or year from the calendar, accordingly."
  (cond
    ;; no period-name? ask the user for input
    ((not period-name)
     (let* ((org-read-date-prefer-future nil)
            (absolute-start (time-to-days (org-read-date nil t nil "Enter the search start")))
            (absolute-end (time-to-days (org-read-date nil t nil "Enter the search end")))
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

    (t (user-error "Wrong period-name given or not in the calendar mode"))))

(defun org-journal--search-by-string (str &optional period-start period-end)
  "Search for a string within a given time interval.

If STR is empty, search for all entries using `org-journal-time-prefix'."
  (when (time-less-p period-end period-start)
    (user-error "Period end cannot be before the start"))
  (let* ((search-str (if (string= "" str) org-journal-time-prefix str))
         (files (org-journal--search-build-file-list period-start period-end))
         (results (org-journal--search-do-search search-str files))
         (buf (get-buffer-create org-journal--search-buffer))
         (inhibit-read-only t))
    (unless (get-buffer-window buf 0)
      (switch-to-buffer buf))
    (with-current-buffer buf
      (org-journal-search-mode)
      (erase-buffer)
      (org-journal--search-print-results str results period-start period-end)
      (goto-char (point-min))
      (forward-button 1)
      (button-activate (button-at (point))))))

(defun org-journal--search-build-file-list (period-start period-end)
  "Build a list of journal files within a given time interval."
  (unless (and period-start period-end ;; Check for null values
               (car period-start) (cdr period-start)
               (car period-end) (cdr period-end))
    (user-error "Time `%s' and/or `%s' are not valid" period-start period-end))

  (let (result filetime)
    (dolist (file (org-journal--list-files))
      (setq filetime (org-journal--calendar-date->time
                      (org-journal--file-name->calendar-date file)))
      (when (and
             (time-less-p
              period-start
              ;; Convert to period-start boundary.
              (pcase org-journal-file-type
                ;; For daily, filetime is period-start boundary.
                (`daily filetime)
                ;; For weekly, filetime +6 days is period-start boundary.
                (`weekly
                 (let* ((time (decode-time filetime))
                        (day (+ 6 (nth 3 time))) ;; End of week
                        (month (nth 4 time))
                        (year (nth 5 time))
                        (last-day-of-month (calendar-last-day-of-month month year)))
                   (when (> day last-day-of-month)
                     (setq day (- day last-day-of-month))
                     (when (= month  12)
                       (setq month 0)
                       (setq year (1+ year)))
                     (setq month (1+ month)))
                   (org-journal--calendar-date->time (list month day year))))
                ;; For monthly, end of month is period-start boundary.
                (`monthly
                 (let* ((time (decode-time filetime))
                        (month (nth 4 time))
                        (year (nth 5 time))
                        (day (calendar-last-day-of-month month year)))
                   (org-journal--calendar-date->time (list month day year))))
                ;; For yearly, end of year is period-start boundary.
                (`yearly
                 (org-journal--calendar-date->time (list 12 31 (nth 5 (decode-time filetime)))))))
             (time-less-p filetime period-end))
        (push file result)))
    result))

(defun org-journal--search-do-search (str files)
  "Search for a string within a list of files, return match pairs (PATH . LINENUM)."
  (let (results result)
    (dolist (fname (reverse files))
      (setq result (org-journal--with-journal
                       fname
                     (when org-journal-enable-encryption
                       (goto-char (point-min))
                       (while (search-forward ":crypt:" nil t)
                         (org-decrypt-entry)))
                     (goto-char (point-min))
                     (while (funcall org-journal-search-forward-fn str nil t)
                       (push
                        (list
                         (let ((date
                                (if (org-journal--daily-p)
                                    (org-journal--file-name->calendar-date fname)
                                  (save-excursion
                                    (when (re-search-backward org-journal--created-re nil t)
                                      (when (= (save-excursion (org-back-to-heading) (org-outline-level)) 1)
                                        (org-journal--entry-date->calendar-date)))))))
                           (when date
                             (org-journal--calendar-date->time date)))
                         (- (point) (length str))
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
                        result))
                     result))
      (when result
        (mapc (lambda (res) (push res results)) result)))
    (cond
      ((eql org-journal-search-results-order-by :desc) results)
      (t (reverse results)))))

(defun org-journal--search-format-date (time)
  "Format TIME according to `org-journal-search-result-date-format'."
  (format-time-string org-journal-search-result-date-format time))

(defun org-journal--search-next ()
  (interactive)
  (forward-button 1 t)
  (button-activate (button-at (point))))

(defun org-journal--search-prev ()
  (interactive)
  (backward-button 1 t)
  (button-activate (button-at (point))))

(defvar org-journal-search-mode-map nil
  "Keymap for *Org-journal search* buffers.")
(unless org-journal-search-mode-map
  (setq org-journal-search-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map "q" 'kill-this-buffer)
          (define-key map (kbd "<tab>") 'org-journal--search-next)
          (define-key map (kbd "<backtab>") 'org-journal--search-prev)
          (define-key map "n" 'org-journal--search-next)
          (define-key map "p" 'org-journal--search-prev)
          map)))
(fset 'org-journal-search-mode-map org-journal-search-mode-map)

(define-derived-mode org-journal-search-mode special-mode
  "org-journal-search"
  "Major mode for displaying org-journal search results.
\\{org-journal-search-mode-map}."
  (use-local-map org-journal-search-mode-map)
  (setq truncate-lines t
        buffer-undo-list t)
  (hl-line-mode 1))

(defun org-journal--search-print-results (str results period-start period-end)
  "Print search results using text buttons."
  (let ((label-start (org-journal--search-format-date period-start))
        (label-end (org-journal--search-format-date period-end)))
    (insert (concat "Search results for \"" str "\" between "
                    label-start " and " label-end
                    ": \n\n")))
  (let (point fullstr time label)
    (dolist (res results)
      (setq time (nth 0 res)
            point (nth 1 res)
            fullstr (nth 2 res)
            label (and time (org-journal--search-format-date time)))
      ;; Filter out entries not within period-start/end for weekly/monthly/yearly journal files.
      (when (or (org-journal--daily-p)
                (and time
                     (time-less-p period-start time)
                     (time-less-p time period-end)))
        (insert-text-button label
                            'action 'org-journal--search-follow-link-action
                            'org-journal-link (cons point time))
        (insert "\t" fullstr "\n"))))
  (org-journal-highlight str))

(defun org-journal--search-follow-link-action (button)
  "Follow the link using info saved in button properties."
  (let* ((target (button-get button 'org-journal-link))
         (point (car target))
         (time (cdr target))
         (buf (org-journal-read-or-display-entry time t)))
    (set-window-point (get-buffer-window buf) point)))

(defun org-journal-re-encrypt-journals (recipient)
  "Re-encrypt journal files."
  (interactive (list (epa-select-keys (epg-make-context epa-protocol)
			              "Select new recipient for encryption.
Only one recipient is supported.  ")))

  (unless recipient
    (user-error "You need to specify exactly one recipient"))

  (unless org-journal-encrypt-journal
    (user-error "org-journal encryption not enabled"))

  (cl-loop
    with buf
    with kill-buffer
    for journal in (org-journal--list-files)
    do
      (setq buf (find-buffer-visiting journal)
            kill-buffer nil)

      (when (and buf
                 (buffer-modified-p buf)
                 (y-or-n-p (format "Journal \"%s\" modified, save before re-encryption?"
                                   (file-name-nondirectory journal))))
        (save-buffer buf))

      (unless buf
        (setq kill-buffer t
              buf (find-file-noselect journal)))

      (with-current-buffer buf
        (let ((epa-file-encrypt-to (epg-sub-key-id (car (epg-key-sub-key-list (car recipient))))))
          (set-buffer-modified-p t)
          (save-buffer)
          (when kill-buffer
            (kill-buffer))))))

(defun org-journal--decrypt ()
  "Decrypt journal entry at point."
  (when org-journal-enable-encryption
    (let ((buffer-read-only nil))
      (org-decrypt-entries))))

(defun org-journal-encryption-hook ()
  "The function added to the hook specified by `org-journal-encrypt-on'."
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
