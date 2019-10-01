;;; org-journal.el --- a simple org-mode based journaling mode -*- lexical-binding: t; -*-

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
(require 'cal-iso)
(require 'org)
(require 'org-crypt nil 'noerror)
(require 'seq)
(require 'subr-x)

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

(defcustom org-journal-dir "~/Documents/journal/"
  "Directory containing journal entries.

Setting this will update the internal `org-journal-file-pattern' to a regex
that matches the directory, using `org-journal-dir-and-format->regex', and
update `auto-mode-alist' using `org-journal-update-auto-mode-alist'."
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
  "Format string for journal file names (Default \"YYYYMMDD\").

This pattern must include `%Y', `%m' and `%d'. Setting this will update the internal
`org-journal-file-pattern' to a regex that matches the format string, using
`org-journal-dir-and-format->regex', and update `auto-mode-alist' using
`org-journal-update-auto-mode-alist'."
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
appropriate way to format days in your language. If you define it as
a function, it is evaluated and inserted."
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
  "The function used by `org-journal-search` to look for the string
forward in a buffer.
Defaults to search-forward.
You can, for example, set it to `search-forward-regexp` so the
search works with regexps."
  :type 'function)

(defcustom org-journal-follow-mode nil
  "If `t', follow journal entry in calendar."
  :type 'boolean)

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

(defmacro org-journal-with-journal (journal-file &rest body)
  "Opens JOURNAL-FILE in fundamental mode, or switches to the buffer which is visiting JOURNAL-FILE.

Returns the last value from BODY. If the buffer didn't exist before it will be deposed."
  ;; Use find-file... instead of view-file... since
  ;; view-file does not respect auto-mode-alist
  `(let* ((buffer-exists (get-buffer (file-name-nondirectory ,journal-file)))
          (buf (if buffer-exists buffer-exists
                 (generate-new-buffer (file-name-nondirectory ,journal-file))))
          result)
     (with-current-buffer buf
       (unless buffer-exists
         (insert-file-contents ,journal-file))
       (setq result (progn ,@body)))
     (unless buffer-exists
       (kill-buffer buf))
     result))

(defvar org-journal-created-re " *:CREATED: *[0-9]\\{8\\}"
  "Regex to find created property.")

(defsubst org-journal-search-forward-created (date)
  "Search for CREATED tag with date.

DATE should be a calendar date list (MONTH DAY YEAR)."
  (re-search-forward
   (format " *:CREATED: *%.4d%.2d%.2d" (nth 2 date) (car date) (cadr date))))

(defun org-journal-daily-p ()
  "Returns t if `org-journal-file-type' is set to `'daily'."
  (eq org-journal-file-type 'daily))

(defun org-journal-org-heading-p ()
  "Returns t if `org-journal-date-prefix' starts with \"* \"."
  (string-match "^\* " org-journal-date-prefix))

(defun org-journal-convert-time-to-file-type-time (&optional time)
  "Converts TIME to the file type format date.

If `org-journal-file-type' is 'weekly the TIME will be rounded to
the first date of the week.

If `org-journal-file-type' is 'monthly the TIME will be rounded to
the first date of the month.

If `org-journal-file-type' is 'yearly the TIME will be rounded to
the first date of the year."
  (or time (setq time (current-time)))
  (pcase org-journal-file-type
    ;; Do nothing for daily
    (`daily time)
    ;; Round to the monday of the current week, e.g. 20181231 is the first week of 2019
    (`weekly
     (let ((date
            (calendar-gregorian-from-absolute
             (calendar-iso-to-absolute
              (mapcar 'string-to-number
                      (split-string (format-time-string "%V 1 %G" time) " "))))))
       (encode-time 0 0 0 (nth 1 date) (nth 0 date) (nth 2 date))))
    ;; Round to the first day of the month, e.g. 20190301
    (`monthly
     (apply 'encode-time
            `(0 0 0 ,@(mapcar 'string-to-number
                              (split-string (format-time-string "1 %m %Y" time) " ")))))
    ;; Round to the first day of the year, e.g. 20190101
    (`yearly
     (apply 'encode-time
            `(0 0 0 ,@(mapcar 'string-to-number
                              (split-string (format-time-string "1 1 %Y" time) " ")))))))

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
      (error "Journal directory is necessary to use org-journal.")))
  t)

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

      ;; Create new journal entry if there isn't one.
      (let ((entry-header
             (if (functionp org-journal-date-format)
                 (funcall org-journal-date-format time)
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
            (org-set-property "CREATED" (format-time-string "%Y%m%d" time)))
          (when org-journal-enable-encryption
            (unless (member org-crypt-tag-matcher (org-get-tags))
              (org-set-tags org-crypt-tag-matcher)))))
      (org-journal-decrypt)

      ;; move TODOs from previous day here
      (when (and (not (string-blank-p org-journal-carryover-items))
                 (string= entry-path (org-journal-get-entry-path (current-time))))
        (org-journal-carryover))

      (if (org-journal-org-heading-p)
          (outline-end-of-subtree)
        (goto-char (point-max)))

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
         carryover-items-with-parents
         carryover-items-non-parents
         prev-entry-buffer)
    (save-excursion
      (save-restriction
        (when (let ((inhibit-message t))
                (org-journal-open-previous-entry 'no-select))
          (setq prev-entry-buffer (current-buffer))
          (unless (org-journal-daily-p) ;; (org-journal-org-heading-p) should work to
            (org-narrow-to-subtree))
          ;; Create a sorted list with duplicates removed from the value returned
          ;; from `org-map-entries'. The returned value from `org-map-entries',
          ;; is a list where each element is list containing points, which are representing
          ;; the headers to carryover -- cddr contains the text.
          (mapc (lambda (carryover-path)
                  (push (car carryover-path) carryover-items-non-parents)
                  (mapc (lambda (heading)
                          (unless (member heading carryover-items-with-parents)
                            (push heading carryover-items-with-parents)))
                        carryover-path))
                (org-map-entries mapper org-journal-carryover-items))
          (setq carryover-items-with-parents (sort carryover-items-with-parents
                                                   (lambda (x y)
                                                     (< (car x) (car y))))))))
    (when carryover-items-with-parents
      (when (org-journal-org-heading-p)
        (outline-end-of-subtree))
      (unless (eq (current-column) 0) (insert "\n"))
      (mapc (lambda (x) (insert (cddr x)))
            carryover-items-with-parents)
      ;; Delete carryover items
      (with-current-buffer prev-entry-buffer
        (mapc (lambda (x)
                (kill-region (car x) (cadr x)))
              carryover-items-non-parents)
        (save-buffer)
        (when org-journal--kill-buffer
          (kill-buffer))))))

(defun org-journal-carryover-item-with-parents ()
  "Return carryover item inclusive the parents.

    The carryover item       The parents
          |                  /---------\
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
    (push (cons start (cons end text)) carryover-item-with-parents)))

(defun org-journal-time-entry-level ()
  "Return the headline level of time entries based on the number
of leading asterisks in `org-journal-time-prefix'.

Return nil when it's impossible to figure out the level."
  (when (string-match "\\(^\*+\\)" org-journal-time-prefix)
    (length (match-string 1 org-journal-time-prefix))))

(defun org-journal-calendar-date->time (calendar-date)
  "Convert a date as returned from the calendar to a time."
  (encode-time 0 0 0                   ; second, minute, hour
               (nth 1 calendar-date)   ; day
               (nth 0 calendar-date)   ; month
               (nth 2 calendar-date))) ; year

(defun org-journal-file-name->calendar-date (file-name)
  "Convert an org-journal file name to a calendar date.

If `org-journal-file-pattern' does not contain capture groups,
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

(defun org-journal-entry-date->calendar-date ()
  "Return journal calendar-date from current buffer.

This is the counterpart of `org-journal-file-name->calendar-date' for
'weekly, 'monthly and 'yearly journal files."
  (let (date)
    (setq date (org-entry-get (point) "CREATED"))
    (unless date
      (error "Entry at \"%s:%d\" doesn't have a \"CREATED\" property." (buffer-file-name) (point)))
    (string-match "\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" date)
    (list (string-to-number (match-string 2 date))
          (string-to-number (match-string 3 date))
          (string-to-number (match-string 1 date)))))

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

If no next/PREVious entry was found print MSG."
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
            (setq org-journal--kill-buffer (if (eq 'no-select no-select)
                                               (set-buffer (find-file-noselect filename))
                                             (find-file filename))))
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
  (file-attribute-modification-time (file-attributes file)))

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
  (when (file-writable-p org-journal-cache-file)
    (with-temp-file org-journal-cache-file
      (let (print-length)
        (insert (prin1-to-string org-journal-dates)
                "\n"
                (prin1-to-string org-journal-journals)))))
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
      org-journal-flatten-dates)))

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

;; silence compiler warning.
(defvar view-exit-action)

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
             ;; If daily continoue with body of if condition
             (or (org-journal-daily-p)
                 ;; Search for journal entry
                 (with-current-buffer buf
                   (save-mark-and-excursion
                     (goto-char (point-min))
                     (setq point (re-search-forward
                                  (format-time-string " *:CREATED: *%Y%m%d" time) nil t))))))
        (progn
          ;; Use `find-file-noselect' instead of `view-file' as it does not respect `auto-mode-alist'
          (with-current-buffer buf
            ;; Open file in view-mode if not opened already.
            (unless buf-exists
              (view-mode)
              (setq view-exit-action 'kill-buffer))
            (set (make-local-variable 'org-hide-emphasis-markers) t)
            (unless (org-journal-daily-p)
              (goto-char point))
            (org-journal-finalize-view)
            (setq point (point)))
          (if noselect
              (display-buffer buf t)
            (funcall org-journal-find-file org-journal-file))
          (set-window-point (get-buffer-window (get-file-buffer org-journal-file)) point)
          buf)
      (message "No journal entry for this date."))))

;;;###autoload
(defun org-journal-next-entry ()
  "Go to the next date with a journal entry."
  (interactive)
  (let ((dates (org-journal-list-dates)))
    (while (and dates (not (calendar-date-compare
                            (list (calendar-cursor-to-date)) dates)))
      (setq dates (cdr dates)))
    (when dates
      (calendar-goto-date (car dates))
      (when org-journal-follow-mode
        (org-journal-display-entry nil)))))

;;;###autoload
(defun org-journal-previous-entry ()
  "Go to the previous date with a journal entry."
  (interactive)
  (let ((dates (reverse (org-journal-list-dates))))
    (while (and dates
                (not (calendar-date-compare dates (list (calendar-cursor-to-date)))))
      (setq dates (cdr dates)))
    (when dates
      (calendar-goto-date (car dates))
      (when org-journal-follow-mode
        (org-journal-display-entry nil)))))

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
                   (encode-time 0 0 0 day month year)))
                ;; For monthly, end of month is period-start boundary.
                (`monthly
                 (let* ((time (decode-time filetime))
                        (month (nth 4 time))
                        (year (nth 5 time)))
                   (encode-time 0 0 0 (calendar-last-day-of-month month year) month year)))
                ;; For yearly, end of year is period-start boundary.
                (`yearly
                 (encode-time 0 0 0 31 12 (nth 5 (decode-time filetime))))))
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

(defun org-journal-format-date (time)
  "Format TIME according to `org-journal-date-format'."
  (format-time-string "%A, %x" time))

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
  (let ((label-start (org-journal-format-date period-start))
        (label-end (org-journal-format-date period-end)))
    (insert (concat "Search results for \"" str "\" between "
                    label-start " and " label-end
                    ": \n\n")))
  (let (point fullstr time label)
    (dolist (res results)
      (setq time (nth 0 res)
            point (nth 1 res)
            fullstr (nth 2 res)
            label (and time (org-journal-format-date time)))
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
  (when (fboundp 'org-decrypt-entries)
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
