;;; org-journal.el --- a simple org-mode based journaling mode -*- lexical-binding: t; -*-

;; Author: Bastian Bechtold
;;         Christian Schwarzgruber

;; URL: http://github.com/bastibe/org-journal
;; Version: 2.1.0
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
(require 'cal-iso)
(require 'org)
(require 'org-crypt)
(require 'seq)
(require 'subr-x)

;; Silent byte-compiler
(defvar view-exit-action)

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

(defvar org-journal-file-pattern
  (expand-file-name "~/Documents/journal/\\(?1:[0-9]\\{4\\}\\)\\(?2:[0-9][0-9]\\)\\(?3:[0-9][0-9]\\)\\'")
  "This matches journal files in your journal directory.

This variable is created and updated automatically by
org-journal. Use `org-journal-file-format' instead.")

;; use this function to update auto-mode-alist whenever
;; org-journal-dir or org-journal-file-pattern change.
;;;###autoload
(defun org-journal-update-auto-mode-alist ()
  "Update `auto-mode-alist' to open journal files in `org-journal-mode'."
  (add-to-list 'auto-mode-alist
               (cons org-journal-file-pattern 'org-journal-mode)))

;;;###autoload
(add-hook 'org-mode-hook 'org-journal-update-auto-mode-alist)
(add-hook 'org-agenda-mode-hook 'org-journal-update-org-agenda-files)

;;;###autoload
(defun org-journal-dir-and-format->regex (dir format)
  "Update `org-journal-file-pattern' with the current `org-journal-file-format'."
  (concat
   (file-truename (expand-file-name (file-name-as-directory dir)))
   (org-journal-format->regex format)
   "\\(\\.gpg\\)?\\'"))

(defun org-journal-format->regex (format)
  (replace-regexp-in-string
   "%[aA]" "\\\\(?4:[a-zA-Z]\\\\{3,\\\\}\\\\)"
   (replace-regexp-in-string
    "%d" "\\\\(?3:[0-9][0-9]\\\\)"
    (replace-regexp-in-string
     "%m" "\\\\(?2:[0-9][0-9]\\\\)"
     (replace-regexp-in-string
      "%Y" "\\\\(?1:[0-9]\\\\{4\\\\}\\\\)"
      (regexp-quote format))))))

;;; Customizable variables
(defgroup org-journal nil
  "Settings for the personal journal"
  :version "1.15.1"
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
  "What day of the week to start a weekly journal.

When `org-journal-file-type' is set to 'weekly, start the week on
this day.  Default is Monday."
  :type '(choice
	  (const :tag "Sunday" 0)
	  (const :tag "Monday" 1)
	  (const :tag "Tuesday" 2)
	  (const :tag "Wednesday" 3)
	  (const :tag "Thursday" 4)
	  (const :tag "Friday" 5)
	  (const :tag "Saturday" 6)))

(defcustom org-journal-dir "~/Documents/journal/"
  "Directory containing journal entries.

Setting this will update the internal `org-journal-file-pattern' to a regex
that matches the directory, using `org-journal-dir-and-format->regex', and
update `auto-mode-alist' using `org-journal-update-auto-mode-alist'.

This variable needs to be set using the customize interface,
`customize-set-variable' or before loading `org-journal'."
  :type 'directory
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
  "Format string for journal file names (Default \"YYYYMMDD\").

This pattern MUST include `%Y', `%m' and `%d' when `org-journal-file-type' is
`daily' or `weekly'. When `org-journal-file-type' is `monthly' this pattern
MUST at least include `%Y' and `%m', and at least `%Y' when
`org-journalf-file-type' is `yearly'.

Setting this will update the internal `org-journal-file-pattern' to a regex
that matches the directory, using `org-journal-dir-and-format->regex', and
update `auto-mode-alist' using `org-journal-update-auto-mode-alist'.

This variable needs to be set using the customize interface,
`customize-set-variable' or before loading `org-journal'."
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
  "String that is put before every date at the top of a journal file.

By default, this is a org-mode heading. Another good idea would be
\"#+TITLE: \" for org titles.

Setting `org-journal-date-prefix' to something other than \"* \"
for weekly/monthly/yearly journal files won't work correctly."
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
  "If true, `org-journal-mode' will hide all but the current entry when creating a new one."
  :type 'boolean)

(defcustom org-journal-enable-encryption nil
  "If non-nil, new journal entries will have a `org-crypt-tag-matcher' tag for encrypting.

Whenever a user saves/opens these journal entries, emacs asks a user passphrase
to encrypt/decrypt it."
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
  "If non-nil, automatically adds current and future org-journal files to `org-agenda-files'."
  :type 'boolean)

(defcustom org-journal-find-file 'find-file-other-window
  "The function to use when opening an entry.

Set this to `find-file' if you don't want org-journal to split your window."
  :type 'function)

(defcustom org-journal-carryover-items "TODO=\"TODO\""
  "Carry over items that match these criteria from the previous entry to new entries.

See agenda tags view match description for the format of this."
  :type 'string)

(defcustom org-journal-carryover-delete-empty-journal 'never
  "Delete empty journal entry/file after carryover.

Default is to `never' delete an empty journal entry/file. Other options are `always',
i.e. don't prompt, just delete or `ask'"
  :type '(choice
          (const :tag "never" never)
          (const :tag "always" always)
          (const :tag "ask" ask)))

(defcustom org-journal-search-results-order-by :asc
  "When :desc, make search results ordered by date descending, otherwise date ascending."
  :type 'symbol)

(defcustom org-journal-tag-alist nil
  "Default tags for use in Org-Journal mode.

This is analogous to `org-tag-alist', and uses the same format.
If nil, the default, then `org-tag-alist' is used instead.
This can also be overridden on a file-local level by using a “#+TAGS:”
keyword."
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
  "The function used by `org-journal-search` to look for the string forward in a buffer.

Defaults to search-forward.
You can, for example, set it to `search-forward-regexp` so the
search works with regexps."
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

%Y is the year.
%m is the numeric month.
%d is the day of the month, zero-padded.
%a is the locale’s abbreviated name of the day of week, %A the full name.

You must call `org-journal-convert-created-property-timestamps' afterwards,
if you have existing journal entries."
  :type 'string)

(defvar org-journal-after-entry-create-hook nil
  "Hook called after journal entry creation.")

(defvar org-journal-search-buffer "*Org-journal search*")


;; Automatically switch to journal mode when opening a journal entry file
(setq org-journal-file-pattern
      (org-journal-dir-and-format->regex org-journal-dir org-journal-file-format))
(org-journal-update-auto-mode-alist)

(add-hook 'calendar-today-visible-hook 'org-journal-mark-entries)
(add-hook 'calendar-today-invisible-hook 'org-journal-mark-entries)

;; Journal mode definition
;;;###autoload
(define-derived-mode org-journal-mode org-mode
  "Journal"
  "Mode for writing or viewing entries written in the journal."
  (turn-on-visual-line-mode)
  ;; Call to `org-journal-serialize' needs to be after the call to `org-journal-journals-puthash'
  (add-hook 'after-save-hook 'org-journal-serialize nil t)
  (add-hook 'after-save-hook 'org-journal-update-org-agenda-files nil t)
  (add-hook 'after-save-hook 'org-journal-journals-puthash nil t)
  (add-hook 'before-save-hook 'org-journal-dates-puthash nil t)
  (when (or org-journal-tag-alist org-journal-tag-persistent-alist)
    (org-journal-set-current-tag-alist))
  (run-mode-hooks))

;; Key bindings
(define-key org-journal-mode-map (kbd "C-c C-f") 'org-journal-open-next-entry)
(define-key org-journal-mode-map (kbd "C-c C-b") 'org-journal-open-previous-entry)
(define-key org-journal-mode-map (kbd "C-c C-j") 'org-journal-new-entry)
(define-key org-journal-mode-map (kbd "C-c C-s") 'org-journal-search)

;;;###autoload
(eval-after-load "calendar"
  '(progn
    (define-key calendar-mode-map "m" 'org-journal-mark-entries)
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

(global-set-key (kbd "C-c C-j") 'org-journal-new-entry)

(defmacro org-journal-with-journal (file &rest body)
  "Opens JOURNAL-FILE in fundamental mode, or switches to the buffer which is visiting JOURNAL-FILE.

Returns the last value from BODY. If the buffer didn't exist before it will be deposed."
  ;; Use find-file... instead of view-file... since
  ;; view-file does not respect auto-mode-alist
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

(defvar org-journal-created-re "^ *:CREATED: +.*$"  "Regex to find created property.")

(defun org-journal-search-forward-created (date &optional bound noerror count)
  "Search for CREATED tag with date."
  (re-search-forward
   (format-time-string
    (concat "[ \t]*:CREATED:[ \t]+"
            (regexp-quote org-journal-created-property-timestamp-format)
            "[ \t]*$")
    (org-journal-calendar-date->time date))
   bound noerror count))

(defsubst org-journal-daily-p ()
  "Returns t if `org-journal-file-type' is set to `'daily'."
  (eq org-journal-file-type 'daily))

(defun org-journal-org-heading-p ()
  "Returns t if `org-journal-date-prefix' starts with \"* \"."
  (string-match "^\* " org-journal-date-prefix))

;;;###autoload
(defun org-journal-convert-created-property-timestamps (old-format)
  "Convert CREATED property timestamps to `org-journal-created-property-timestamp-format'."
  (interactive "sEnter old format: ")
  (if (org-journal-daily-p)
      (message "Nothing to do, org-journal-file-type is 'daily")
    (dolist (file (org-journal-list-files))
      (let* ((inhibit-read-only)
             (buffer (get-buffer (file-name-nondirectory file)))
             (buffer-modefied (when buffer (buffer-modified-p buffer))))
        (with-current-buffer (if buffer buffer (find-file-noselect file))
          (goto-char (point-min))
          (ignore-errors
            (dolist (date (reverse (let ((org-journal-created-property-timestamp-format old-format))
                                     (org-journal-file->calendar-dates file))))
              (unless (let ((org-journal-created-property-timestamp-format old-format))
                        (org-journal-search-forward-created date nil t))
                (error "Did not find journal entry in file (%s), date was (%s) " file date))
              (org-set-property "CREATED" (format-time-string
                                           org-journal-created-property-timestamp-format
                                           (org-journal-calendar-date->time date)))))
          (unless buffer-modefied (save-buffer))
          (unless buffer (kill-buffer)))))))

(defun org-journal-convert-time-to-file-type-time (&optional time)
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
       (org-journal-calendar-date->time date)))
    ;; Round to the first day of the month, e.g. 20190301
    (`monthly
     (org-journal-calendar-date->time
      (mapcar 'string-to-number (split-string (format-time-string "%m 1 %Y" time) " "))))
    ;; Round to the first day of the year, e.g. 20190101
    (`yearly
     (org-journal-calendar-date->time
      (mapcar 'string-to-number (split-string (format-time-string "1 1 %Y" time) " "))))))

(defun org-journal-get-entry-path (&optional time)
  "Return the path to an entry matching TIME, if no TIME is given, uses the current time."
  (let ((file (file-truename
               (expand-file-name
                (format-time-string org-journal-file-format
                                    (org-journal-convert-time-to-file-type-time time))
                (file-name-as-directory org-journal-dir)))))
    (when (and org-journal-encrypt-journal (not (file-exists-p file)))
      (setq file (concat file ".gpg")))
    file))

(defun org-journal-dir-check-or-create ()
  "Check existence of `org-journal-dir'. If it doesn't exist, try to make directory."
  (unless (file-exists-p org-journal-dir)
    (if (yes-or-no-p (format "Journal directory %s not found. Create one? " org-journal-dir))
        (make-directory org-journal-dir t)
      (error "Journal directory is necessary to use org-journal."))))

(defun org-journal-set-current-tag-alist ()
  "Set `org-current-tag-alist' for the current journal file.
This allows the use of `org-journal-tag-alist' and
`org-journal-tag-persistent-alist', which when non-nil override
`org-tag-alist' and `org-journal-tag-persistent-alist' respectively."
  (setq org-current-tag-alist ; this var is always buffer-local
        (org--tag-add-to-alist
         (or org-journal-tag-persistent-alist org-tag-persistent-alist)
         (let* ((alist (org--setup-collect-keywords
                        (org-make-options-regexp
                         '("FILETAGS" "TAGS" "SETUPFILE"))))
                (tags (cdr (assq 'tags alist))))
           (if (and alist tags)
               (org-tag-string-to-alist tags)
             (or org-journal-tag-alist org-tag-alist))))))

;;;###autoload
(defun org-journal-new-entry (prefix &optional time)
  "Open today's journal file and start a new entry.

With a PREFIX arg, open the today's file, create a heading if it doesn't exist yet,
but do not create a new entry.

If given a TIME, create an entry for the time's day. If no TIME was given,
use the current time (which is interpreted as belonging to yesterday if
smaller than `org-extend-today-until`).

Whenever a journal entry is created the `org-journal-after-entry-create-hook'
hook is run."
  (interactive "P")
  (org-journal-dir-check-or-create)

  ;; if time is before org-extend-today-until, interpret it as
  ;; part of the previous day:
  (let (oetu-active-p) ;; org-extend-today-until-active-p
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
           (should-add-entry-p (not prefix))
           match)

      ;; Open journal file
      (unless (string= entry-path (buffer-file-name))
        (funcall org-journal-find-file entry-path))

      ;; Insert org-journal-file-header
      (if (and (or (functionp org-journal-file-header)
                   (and (stringp org-journal-file-header)
                        (not (string-empty-p org-journal-file-header))))
               (= (buffer-size) 0))
          (insert (if (functionp org-journal-file-header)
                      (funcall org-journal-file-header time)
                    (format-time-string org-journal-file-header time))))

      ;; Create new journal entry if there isn't one.
      (let ((entry-header
             (if (functionp org-journal-date-format)
                 (funcall org-journal-date-format time)
               (when (string-empty-p org-journal-date-format)
                 (error "org-journal-date-format is empty, this won't work"))
               (concat org-journal-date-prefix
                       (format-time-string org-journal-date-format time)))))
        (goto-char (point-min))
        (unless (search-forward entry-header nil t)
          ;; Insure we insert the new journal header at the correct location
          (unless (org-journal-daily-p)
            (let ((date (decode-time time))
                  (dates (sort (org-journal-file->calendar-dates (buffer-file-name))
                               (lambda (a b)
                                 (calendar-date-compare (list b) (list a))))))
              (setq date (list (nth 4 date) (nth 3 date) (nth 5 date)))
              (while dates
                (when (calendar-date-compare dates (list date))
                  (org-journal-search-forward-created (car dates))
                  (outline-end-of-subtree)
                  (insert "\n")
                  (setq match t
                        dates nil))
                (setq dates (cdr dates)))))
          ;; True if entry must be inserted at the end of the journal file.
          (unless match
            (goto-char (point-max))
            (forward-line))
          (when (looking-back "[^\t ]" (point-at-bol))
            (insert "\n"))
          (beginning-of-line)
          (insert entry-header)
          ;; For 'weekly, 'monthly and 'yearly journal entries
          ;; create a "CREATED" property with the current date.
          (unless (org-journal-daily-p)
            (org-set-property "CREATED"
                              (format-time-string
                               org-journal-created-property-timestamp-format time)))
          (when org-journal-enable-encryption
            (unless (member org-crypt-tag-matcher (org-get-tags))
              (org-set-tags org-crypt-tag-matcher)))))
      (org-journal-decrypt)

      ;; Move TODOs from previous day to new entry
      (when (and org-journal-carryover-items
                 (not (string-blank-p org-journal-carryover-items))
                 (string= entry-path (org-journal-get-entry-path (current-time))))
        (org-journal-carryover))

      (if (org-journal-org-heading-p)
          (outline-end-of-subtree)
        (goto-char (point-max)))

      ;; Insert the header of the entry
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
          (insert org-journal-time-prefix timestamp))
        (run-hooks 'org-journal-after-entry-create-hook))

      (if (and org-journal-hide-entries-p (org-journal-time-entry-level))
          (outline-hide-sublevels (org-journal-time-entry-level))
        (save-excursion (org-journal-finalize-view)))

      (when should-add-entry-p
        (outline-show-entry)))))

(defvar org-journal--kill-buffer nil
  "Will be set to the `t' if `org-journal-open-entry' is visiting a
buffer not open already, otherwise `nil'.")

(defun org-journal-empty-journal-p (prev-buffer)
  (let (entry)
    (with-current-buffer prev-buffer (save-buffer))
    (save-excursion
      (org-journal-open-previous-entry 'no-select)
      (setq entry (if (org-journal-org-heading-p)
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

(defun org-journal-carryover-delete-empty-journal (prev-buffer)
  "Check if the previous entry/file is empty after we carried over the
items, and delete or not delete the empty entry/file based on
`org-journal-carryover-delete-empty-journal'."
  (when (and (org-journal-empty-journal-p prev-buffer)
             (or (and (eq org-journal-carryover-delete-empty-journal 'ask)
                      (y-or-n-p "Delete empty journal entry/file?"))
                 (eq org-journal-carryover-delete-empty-journal 'always)))

    (let ((inhibit-message t))
      ;; Check if the file doesn't contain any other entry, by comparing the
      ;; new filename with the previous entry filename and the next entry filename.
      (if (and (save-excursion
                 (org-journal-open-previous-entry 'no-select)
                 (or (not (org-journal-open-previous-entry 'no-select))
                     (not (eq (current-buffer) prev-buffer))))
               (not (eq (current-buffer) prev-buffer)))
          (progn
            (delete-file (buffer-file-name prev-buffer))
            (kill-buffer prev-buffer)
            (when org-journal-enable-cache (org-journal-list-dates)))
        (save-excursion
          (org-journal-open-previous-entry 'no-select)
          (kill-region (point) (progn (outline-end-of-subtree) (point)))
          (save-buffer))))))

(defun org-journal-carryover-items (text entries prev-buffer)
  "Carryover items.

Will insert `entries', and delete the inserted entries from `prev-buffer'.
If the parent heading has no more content delete it is well."
  (when entries
    (if (org-journal-org-heading-p)
        (progn
          (while (org-up-heading-safe))
          (outline-end-of-subtree))
      (goto-char (point-max)))

    (unless (eq (current-column) 0) (insert "\n"))

    (insert text)

    ;; Delete carried over items
    (with-current-buffer prev-buffer
      (mapc (lambda (x)
              (unless (save-excursion
                        (goto-char (1- (cadr x)))
                        (org-goto-first-child))
                (kill-region (car x) (cadr x))))
            (reverse entries)))

    (while (org-up-heading-safe))
    (outline-end-of-subtree)))

(defun org-journal-carryover ()
  "Moves all items matching `org-journal-carryover-items' from the
previous day's file to the current file."
  (interactive)
  (let* ((org-journal-find-file 'find-file)
         (mapper (lambda ()
                   (let ((headings (org-journal-carryover-item-with-parents)))
                     ;; Since the next subtree now starts at point,
                     ;; continue mapping from before that, to include it
                     ;; in the search
                     (setq org-map-continue-from (point))
                     headings)))
         carryover-paths prev-buffer)

    ;; Get carryover paths
    (save-excursion
      (save-restriction
        (when (let ((inhibit-message t)) (org-journal-open-previous-entry 'no-select))
          (setq prev-buffer (current-buffer))
          (unless (org-journal-daily-p)
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
                 with counter = 0
                 do (progn
                      (when (or (not (and prev-paths (nth counter prev-paths)))
                                (> (car path) (car (nth counter prev-paths))))
                        (setq text (concat text (cddr path)))
                        (if cleared-paths
                            (setcdr (last cleared-paths) (list path))
                          (setq cleared-paths (list path))))
                      (setq counter (1+ counter)))
                 finally (progn
                           (if cleared-carryover-paths
                               (setcdr (last cleared-carryover-paths) cleared-paths)
                             (setq cleared-carryover-paths cleared-paths))
                           (setq prev-paths paths))))
        (org-journal-carryover-items text cleared-carryover-paths prev-buffer))
      (org-journal-carryover-delete-empty-journal prev-buffer))

    (when org-journal--kill-buffer
      (mapc 'kill-buffer org-journal--kill-buffer)
      (setq org-journal--kill-buffer nil))))

(defun org-journal-carryover-item-with-parents ()
  "Return carryover item inclusive the parents.

      The parents ...            The carryover item
;; ((START END . \"TEXT\") ... (START END . \"TEXT\"))
"
  (let (start end text carryover-item-with-parents)
    (save-excursion
      (while (> (org-outline-level) (org-journal-time-entry-level))
        (org-up-heading-safe)
        (setq start (point)
              end (save-excursion (outline-next-heading) (point))
              text (buffer-substring-no-properties start end))
        (push (cons start (cons end text)) carryover-item-with-parents)))
    (setq start (point-at-bol)
          end (progn (outline-end-of-subtree) (outline-next-heading) (point))
          text (buffer-substring-no-properties start end))
    (setq carryover-item-with-parents (append carryover-item-with-parents (list (cons start (cons end text)))))))

(defun org-journal-time-entry-level ()
  "Return the headline level of time entries based on the number
of leading asterisks in `org-journal-time-prefix'.

Return nil when it's impossible to figure out the level."
  (when (string-match "\\(^\*+\\)" org-journal-time-prefix)
    (length (match-string 1 org-journal-time-prefix))))

(defun org-journal-calendar-date->time (date)
  "Convert a date as returned from the calendar (MONTH DAY YEAR) to a time."
  (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date)))

(defun org-journal-file-name->calendar-date (file-name)
  "Convert an org-journal file name to a calendar date.

Month and Day capture group default to 1."
  (let ((day 1) (month 1) year)
    (setq year (string-to-number
                (replace-regexp-in-string org-journal-file-pattern "\\1" file-name)))

    (when (integerp (string-match "\(\?2:" org-journal-file-pattern))
      (setq month (string-to-number
                   (replace-regexp-in-string org-journal-file-pattern "\\2" file-name))))

    (when (integerp (string-match "\(\?3:" org-journal-file-pattern))
      (setq day (string-to-number
                 (replace-regexp-in-string org-journal-file-pattern "\\3" file-name))))
    (list month day year)))

(defun org-journal-entry-date->calendar-date ()
  "Return journal calendar-date from current buffer.

This is the counterpart of `org-journal-file-name->calendar-date' for
'weekly, 'monthly and 'yearly journal files."
  (let ((re (org-journal-format->regex org-journal-created-property-timestamp-format))
        date)
    (setq date (org-entry-get (point) "CREATED"))
    (unless date
      (error "Entry at \"%s:%d\" doesn't have a \"CREATED\" property." (buffer-file-name) (point)))
    (string-match re date)
    (list (string-to-number (match-string 2 date))   ;; Month
          (string-to-number (match-string 3 date))  ;; Day
          (string-to-number (match-string 1 date))))) ;; Year

(defun org-journal-file->calendar-dates (file)
  "Return journal dates from FILE."
  (interactive "P")
  (org-journal-with-journal
   file
   (let (dates)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward org-journal-created-re nil t)
         (push (org-journal-entry-date->calendar-date) dates))
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
  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (if (time-less-p time (current-time))
        (org-journal-new-entry prefix time)
      (org-journal-new-scheduled-entry prefix (format-time-string "%Y-%m-%d" time)))))

;;;###autoload
(defun org-journal-new-scheduled-entry (prefix &optional scheduled-time)
  "Create a new entry in the future."
  (interactive "P")
  (let ((scheduled-time (or scheduled-time (org-read-date nil nil nil "Date:")))
        (raw (prefix-numeric-value prefix)))
    (org-journal-new-entry (= raw 16) (org-time-string-to-time scheduled-time))
    (unless (= raw 16)
      (if (not prefix)
          (insert "TODO "))
      (save-excursion
        (insert "\n<" scheduled-time ">")))))

(defun org-journal-open-entry (msg &optional prev no-select)
  "Open journal entry.

If no next/previous entry was found print MSG."
  (let ((calendar-date (if (org-journal-daily-p)
                           (org-journal-file-name->calendar-date (file-truename (buffer-file-name)))
                         (while (org-up-heading-safe))
                         (org-journal-entry-date->calendar-date)))
        (view-mode-p view-mode)
        (dates (org-journal-list-dates)))
    (unless (member calendar-date dates)
      ;; Insert calendar-date into dates list keeping it in order.
      (setq dates (cl-loop
                     for date in dates
                     while (calendar-date-compare (list date) (list calendar-date))
                     collect date into result and count t into cnt
                     finally return (if result
                                        ;; Front
                                        `(,@result ,calendar-date)
                                      ;; Somewhere enbetween or end of dates
                                      `(,calendar-date ,@result ,@(nthcdr cnt dates))))))
    ;; Reverse list for previous search.
    (when prev
      (setq dates (reverse dates)))
    (while (and dates (car dates)
                (or (if prev
                        (calendar-date-compare (list calendar-date) dates)
                      (calendar-date-compare dates (list calendar-date)))
                    (calendar-date-equal (car dates) calendar-date)))
      (setq dates (cdr dates)))
    (if (and dates (car dates))
        (let* ((date (car dates))
               (time (org-journal-calendar-date->time date))
               (filename (org-journal-get-entry-path time)))
          (if (get-file-buffer filename)
              (progn
                (if (eq 'no-select no-select)
                    (set-buffer (get-file-buffer filename))
                  (switch-to-buffer (get-file-buffer filename)))
                (setq org-journal--kill-buffer nil))
            (push (if (eq 'no-select no-select)
                      (set-buffer (find-file-noselect filename))
                    (find-file filename))
                  org-journal--kill-buffer))
          (widen)
          (goto-char (point-min))
          (if (org-journal-daily-p)
              (outline-next-visible-heading 1)
            (org-journal-search-forward-created date))
          (org-journal-finalize-view)
          (view-mode (if view-mode-p 1 -1))
          t)
      (message msg)
      nil)))

(defun org-journal-open-next-entry (&optional no-select)
  "Open the next journal entry starting from a currently displayed one."
  (interactive)
  (org-journal-open-entry "No next journal entry after this one" nil no-select))

(defun org-journal-open-previous-entry (&optional no-select)
  "Open the previous journal entry starting from a currently displayed one."
  (interactive)
  (org-journal-open-entry "No previous journal entry before this one" t no-select))


;;; Functions to browse existing journal entries using the calendar

;;;###autoload
(defun org-journal-list-files ()
  "Returns a list of all files in the journal directory."
  (org-journal-dir-check-or-create)
  ;; grab the file list. We can’t use directory-files-recursively’s
  ;; regexp facility to filter it, because that only checks the
  ;; regexp against the base filenames, and we need to check it
  ;; against filenames relative to org-journal-dir.
  (let ((file-list (directory-files-recursively
                    (file-truename (expand-file-name
                                    (file-name-as-directory org-journal-dir))) "\.*"))
        (predicate (lambda (file-path)
                     (and (string-match-p org-journal-file-pattern (file-truename file-path))
                          (or org-journal-encrypt-journal
                              (not (string-match-p "\.gpg$" (file-truename file-path))))))))
    (seq-filter predicate file-list)))

(defvar org-journal-cache-file
  (expand-file-name "org-journal.cache" user-emacs-directory)
  "Cache file for `org-journal-dates' and `org-journal-journals' hash maps.")

(defvar org-journal-journals (make-hash-table :test 'equal)
  "Hash map for journal file modification time. The key is the journal
file and the value the modification time.")

(defvar org-journal-dates (make-hash-table :test 'equal)
  "Hash map for journal dates. The key is the journal file and the
value the journal file dates.")

;;;###autoload
(defun org-journal-invalidate-cache ()
  "Reset `org-journal-journals', `org-journal-dates' and remove the
file `org-journal-cache-file'."
  (interactive)
  (setq org-journal-journals (make-hash-table :test 'equal)
        org-journal-dates (make-hash-table :test 'equal))
  (when (file-exists-p org-journal-cache-file)
    (delete-file org-journal-cache-file)))

(defun org-journal-file-modification-time (file)
  (nth 5 (file-attributes file)))

(defun org-journal-journals-puthash (&optional file)
  (or file (setq file (buffer-file-name)))
  (puthash file (org-journal-file-modification-time file) org-journal-journals))

(defun org-journal-dates-puthash (&optional file)
  (or file (setq file (buffer-file-name)))
  (let ((dates (if (org-journal-daily-p)
                   (org-journal-file-name->calendar-date file)
                 (nreverse (org-journal-file->calendar-dates file)))))
    (puthash file dates org-journal-dates)))

(defun org-journal-serialize ()
  "Write hashmap to file."
  (unless (file-directory-p (file-name-directory org-journal-cache-file))
    (make-directory (file-name-directory org-journal-cache-file) t))
  (if (file-writable-p org-journal-cache-file)
      (with-temp-file org-journal-cache-file
        (let (print-length)
          (insert (prin1-to-string org-journal-dates)
                  "\n"
                  (prin1-to-string org-journal-journals))))
    (error "%s is not writable" org-journal-cache-file))
  (org-journal-flatten-dates))

(defun org-journal-deserialize ()
  "Read hashmap from file."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p org-journal-cache-file)
      (with-temp-buffer
        (insert-file-contents org-journal-cache-file)
        (setq org-journal-dates (read (buffer-substring (point-at-bol) (point-at-eol))))
        (forward-line)
        (setq org-journal-journals (read (buffer-substring (point-at-bol) (point-at-eol)))))))
  (org-journal-flatten-dates))

(defvar org-journal-flatten-dates nil
  "Holds a list of all journal dates.
It's used only when `org-journal-file-type' is not 'daily.")

(defun org-journal-flatten-dates-recursive (dates)
  "Recursively flatten dates into a single list."
  (when (consp dates)
    (append (car dates) (org-journal-flatten-dates-recursive (cdr dates)))))

(defun org-journal-flatten-dates ()
  "Flatten dates if `org-journal-file-type' is not `'daily'."
  (unless (org-journal-daily-p)
    (setq org-journal-flatten-dates
          (org-journal-flatten-dates-recursive (hash-table-values org-journal-dates)))))

(defun org-journal-list-dates ()
  "Loads the list of files in the journal directory, and converts
it into a list of calendar date elements."
  (if org-journal-enable-cache
      (let ((files (org-journal-list-files)))
        (when (or (hash-table-empty-p org-journal-dates)
                  (hash-table-empty-p org-journal-journals))
          (org-journal-deserialize)
          (when (or (hash-table-empty-p org-journal-dates)
                    (hash-table-empty-p org-journal-journals))
            (dolist (file files)
              (org-journal-journals-puthash file)
              (org-journal-dates-puthash file))
            (org-journal-serialize)))
        ;; Verify modification time is unchanged, otherwise parse journal dates.
        (let ((keys (hash-table-keys org-journal-dates)))
          (dolist (file files)
            (unless (equal (gethash file org-journal-journals)
                           (org-journal-file-modification-time file))
              (org-journal-journals-puthash file)
              (org-journal-dates-puthash file)
              (org-journal-serialize))
            (when (member file keys)
              (setq keys (delete file keys))))
          (when keys
            (dolist (key keys)
              (remhash key org-journal-dates)
              (remhash key org-journal-journals))
            (org-journal-serialize)))
        (if (org-journal-daily-p)
            (hash-table-values org-journal-dates)
          org-journal-flatten-dates))
    (let ((dates (mapcar (if (org-journal-daily-p)
                             'org-journal-file-name->calendar-date
                           'org-journal-file->calendar-dates)
                         (org-journal-list-files))))
      ;; Need to flatten the list and bring dates in correct order.
      (unless (org-journal-daily-p)
        (let ((flattened-date-l '())
              flattened-date-reverse-l file-dates)
          (while dates
            (setq file-dates (car dates))
            (setq flattened-date-reverse-l '())
            (while file-dates
              (push (car file-dates) flattened-date-reverse-l)
              (setq file-dates (cdr file-dates)))
            ;; Correct order of journal entries from file by pushing it to a new list.
            (mapc (lambda (p)
                    (push p flattened-date-l))
                  flattened-date-reverse-l)
            (setq dates (cdr dates)))
          (setq dates (reverse flattened-date-l))))
      dates)))

;;;###autoload
(defun org-journal-mark-entries ()
  "Mark days in the calendar for which a diary entry is present"
  (interactive)
  (when (file-exists-p org-journal-dir)
    (dolist (journal-entry (org-journal-list-dates))
      (if (calendar-date-is-visible-p journal-entry)
          (if (time-less-p (org-journal-calendar-date->time journal-entry)
                           (current-time))
              (calendar-mark-visible-date journal-entry 'org-journal-calendar-entry-face)
            (calendar-mark-visible-date journal-entry 'org-journal-calendar-scheduled-face))))))

;;;###autoload
(defun org-journal-read-entry (_arg &optional event)
  "Open journal entry for selected date for viewing"
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-read-or-display-entry time nil)))

;;;###autoload
(defun org-journal-display-entry (_arg &optional event)
  "Display journal entry for selected date in another window."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let* ((time (org-journal-calendar-date->time
                (calendar-cursor-to-date t event))))
    (org-journal-read-or-display-entry time t)))

(defun org-journal-finalize-view ()
  "Finalize visability of entry."
  (org-journal-decrypt)
  (if (org-journal-org-heading-p)
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
  (let* ((org-journal-file (org-journal-get-entry-path time))
         (buf-exists (get-file-buffer org-journal-file))
         buf point)
    (if (and (when (file-exists-p org-journal-file)
               (setq buf (find-file-noselect org-journal-file)))
             ;; If daily continue with than clause of if condition
             (or (org-journal-daily-p)
                 ;; Search for journal entry
                 (with-current-buffer buf
                   (save-mark-and-excursion
                     (goto-char (point-min))
                     (setq time (decode-time time))
                     (setq point (org-journal-search-forward-created
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
            (if (org-journal-daily-p)
                (when (org-journal-org-heading-p)
                  (goto-char (point-min))
                  (re-search-forward (if (functionp org-journal-date-format)
                                         (funcall org-journal-date-format time)
                                       (format-time-string org-journal-date-format time))))
              (goto-char point))
            (org-journal-finalize-view)
            (setq point (point)))
          (if noselect
              (display-buffer buf t)
            (funcall org-journal-find-file org-journal-file))
          (set-window-point (get-buffer-window (get-file-buffer org-journal-file)) point)
          buf)
      (message "No journal entry for this date."))))

(defun org-journal--next-entry (&optional prev)
  "Go to next entry.

If prev is non-nil open previous entry instead of next."
  (let ((dates (if prev
                   (reverse (org-journal-list-dates))
                 (org-journal-list-dates))))
    (while (and dates
                (not (if prev
                         (calendar-date-compare dates (list (calendar-cursor-to-date)))
                       (calendar-date-compare (list (calendar-cursor-to-date)) dates))))
      (setq dates (cdr dates)))
    (when dates
      (calendar-goto-date (car dates))
      (when org-journal-follow-mode
        (org-journal-display-entry nil)))))

;;;###autoload
(defun org-journal-next-entry ()
  "Go to the next date with a journal entry."
  (interactive)
  (org-journal--next-entry))

;;;###autoload
(defun org-journal-previous-entry ()
  "Go to the previous date with a journal entry."
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
  (let* ((period-pair (org-journal-read-period (if current-prefix-arg 'forever period-name)))
         (start (org-journal-calendar-date->time (car period-pair)))
         (end (org-journal-calendar-date->time (cdr period-pair))))
    ;; Including period-start in search
    (setcar (cdr start) (1- (cadr start)))
    ;; Including period-end in search
    (setcar (cdr end) (1+ (cadr end)))
    (org-journal-search-by-string str start end)))

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

(defun org-journal-search-future-scheduled ()
  "Search for TODOs within all future entries."
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

(defun org-journal-update-org-agenda-files ()
  "Adds the current and future journal files to `org-agenda-files' containing TODOs,
and cleans out past org-journal files."
  (when org-journal-enable-agenda-integration
    (let ((not-org-journal-agenda-files
           (seq-filter
            (lambda (fname)
              (not (string-match org-journal-file-pattern fname)))
            (org-agenda-files)))
          (org-journal-agenda-files
           (let* ((future (org-journal-read-period 'future))
                  (beg (car future))
                  (end (cdr future)))
             (setcar (cdr beg) (1- (cadr beg)))
             (org-journal-search-build-file-list
              (org-journal-calendar-date->time beg)
              (org-journal-calendar-date->time end)))))
      (setq org-agenda-files (append not-org-journal-agenda-files
                                     org-journal-agenda-files)))))

(defun org-journal-schedule-view ()
  "Opens a new window with all scheduled journal entries.

Think of this as a faster, less fancy version of your `org-agenda'."
  (interactive)
  (find-file-other-window "*Org-journal schedule*")
  (view-mode -1)
  (erase-buffer)
  (org-mode)
  (insert "#+TITLE: Org-Journal Schedule\n\n")
  (let* ((period-pair (org-journal-read-period 'future))
         (start (org-journal-calendar-date->time (car period-pair)))
         (end (org-journal-calendar-date->time (cdr period-pair)))
         (file-list (org-journal-search-build-file-list start end)))
    (dolist (filename (sort file-list
                            (lambda (x y)
                              (time-less-p
                               (org-journal-calendar-date->time
                                (org-journal-file-name->calendar-date x))
                               (org-journal-calendar-date->time
                                (org-journal-file-name->calendar-date y))))))
      (let ((time (org-journal-calendar-date->time
                   (org-journal-file-name->calendar-date filename)))
            (copy-mapper
             (lambda ()
               (let ((subtree (org-journal-carryover-item-with-parents)))
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
         (setq content-to-copy (org-map-entries
                                copy-mapper
                                "+TIMESTAMP>=\"<now>\"|+SCHEDULED>=\"<now>\"")))
        (if content-to-copy
            (insert (mapconcat 'identity content-to-copy "") "\n")
          (insert "N/A\n"))))
    (set-buffer-modified-p nil)
    (view-mode t)
    (goto-char (point-min))))

(defun org-journal-read-period (period-name)
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

    (t (error "Wrong period-name given or not in the calendar mode"))))

(defun org-journal-search-by-string (str &optional period-start period-end)
  "Search for a string within a given time interval.

If STR is empty, search for all entries using `org-journal-time-prefix'."
  (when (time-less-p period-end period-start)
    (error "Period end cannot be before the start"))
  (let* ((search-str (if (string= "" str) org-journal-time-prefix str))
         (files (org-journal-search-build-file-list period-start period-end))
         (results (org-journal-search-do-search search-str files))
         (buf (get-buffer-create org-journal-search-buffer))
         (inhibit-read-only t))
    (unless (get-buffer-window buf 0)
      (switch-to-buffer buf))
    (with-current-buffer buf
      (org-journal-search-mode)
      (erase-buffer)
      (org-journal-search-print-results str results period-start period-end)
      (goto-char (point-min))
      (forward-button 1)
      (button-activate (button-at (point))))))

(defun org-journal-search-build-file-list (period-start period-end)
  "Build a list of journal files within a given time interval."
  (unless (and period-start period-end ;; Check for null values
               (car period-start) (cdr period-start)
               (car period-end) (cdr period-end))
    (error "Time `%s' and/or `%s' are not valid" period-start period-end))

  (let (result filetime)
    (dolist (file (org-journal-list-files))
      (setq filetime (org-journal-calendar-date->time
                      (org-journal-file-name->calendar-date file)))
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
                   (org-journal-calendar-date->time (list month day year))))
                ;; For monthly, end of month is period-start boundary.
                (`monthly
                 (let* ((time (decode-time filetime))
                        (month (nth 4 time))
                        (year (nth 5 time))
                        (day (calendar-last-day-of-month month year)))
                   (org-journal-calendar-date->time (list month day year))))
                ;; For yearly, end of year is period-start boundary.
                (`yearly
                 (org-journal-calendar-date->time (list 12 31 (nth 5 (decode-time filetime)))))))
             (time-less-p filetime period-end))
        (push file result)))
    result))

(defun org-journal-search-do-search (str files)
  "Search for a string within a list of files, return match pairs (PATH . LINENUM)."
  (let (results result)
    (dolist (fname (reverse files))
      (setq result (org-journal-with-journal
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
                               (if (org-journal-daily-p)
                                   (org-journal-file-name->calendar-date fname)
                                 (save-excursion
                                   (when (re-search-backward org-journal-created-re nil t)
                                     (org-journal-entry-date->calendar-date))))))
                          (when date
                            (org-journal-calendar-date->time date)))
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

(defun org-journal-search-format-date (time)
  "Format TIME according to `org-journal-search-result-date-format'."
  (format-time-string org-journal-search-result-date-format time))

(defun org-journal-search-next ()
  (interactive)
  (forward-button 1 t)
  (button-activate (button-at (point))))

(defun org-journal-search-prev ()
  (interactive)
  (backward-button 1 t)
  (button-activate (button-at (point))))

(defvar org-journal-search-mode-map nil
  "Keymap for *Org-journal search* buffers.")
(unless org-journal-search-mode-map
  (setq org-journal-search-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map "q" 'kill-this-buffer)
          (define-key map (kbd "<tab>") 'org-journal-search-next)
          (define-key map (kbd "<backtab>") 'org-journal-search-prev)
          (define-key map "n" 'org-journal-search-next)
          (define-key map "p" 'org-journal-search-prev)
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

(defun org-journal-search-print-results (str results period-start period-end)
  "Print search results using text buttons."
  (let ((label-start (org-journal-search-format-date period-start))
        (label-end (org-journal-search-format-date period-end)))
    (insert (concat "Search results for \"" str "\" between "
                    label-start " and " label-end
                    ": \n\n")))
  (let (point fullstr time label)
    (dolist (res results)
      (setq time (nth 0 res)
            point (nth 1 res)
            fullstr (nth 2 res)
            label (and time (org-journal-search-format-date time)))
      ;; Filter out entries not within period-start/end for weekly/monthly/yearly journal files.
      (when (or (org-journal-daily-p)
                (and time
                     (time-less-p period-start time)
                     (time-less-p time period-end)))
        (insert-text-button label
                            'action 'org-journal-search-follow-link-action
                            'org-journal-link (cons point time))
        (insert "\t" fullstr "\n"))))
  (org-journal-highlight str))

(defun org-journal-search-follow-link-action (button)
  "Follow the link using info saved in button properties."
  (let* ((target (button-get button 'org-journal-link))
         (point (car target))
         (time (cdr target))
         (buf (org-journal-read-or-display-entry time t)))
    (set-window-point (get-buffer-window buf) point)))

(defun org-journal-decrypt ()
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
