;;; cellar.el --- Manage a wine cellar with Emacs and SQLite  -*- lexical-binding: t; -*-

;; Author: M. Rincon
;; Version: 0.0.1
;; URL: https://github.com/foxfriday/cellar
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; The package uses a `sqlite' database to manage a collection of wines in two
;; different locations.
;;
;; Note: the SQL queries use `concat_ws' which requires SQLite 3.44.0 or later.

;;; Code:

(require 'cl-lib)
(require 'sqlite)
(require 'transient)

;;; Customization
(defgroup cellar nil
  "Manage a wine cellar with sqlite."
  :group 'applications
  :prefix "cellar-")

(defcustom cellar-database "~/Repos/wiki/db/inventory.db"
  "The location of the database."
  :type 'file
  :group 'cellar)

(defcustom cellar-note-dir "~/Repos/wiki/docs/cellar/"
  "Directory for tasting note files."
  :type 'directory
  :group 'cellar)

;;; Internal Helpers
(defmacro cellar--with-db (db-var &rest body)
  "Open cellar database, bind to DB-VAR, execute BODY, close on exit."
  (declare (indent 1))
  `(progn
     (unless (file-exists-p cellar-database)
       (error "Database %s does not exist" cellar-database))
     (let ((,db-var (sqlite-open cellar-database)))
       (unwind-protect
           (progn (sqlite-execute ,db-var "PRAGMA foreign_keys = ON") ,@body)
         (sqlite-close ,db-var)))))

;;; Tasting Note Helpers
(defun cellar--parse-note ()
  "Parse a tasting note buffer and return a plist of its data.
Reads `iid', `date', and the five score keys from the YAML
frontmatter only; the note body is ignored.  The buffer must start
with a `---' line and the frontmatter must be closed by another
`---' line.  Returns a plist with :iid, :date, :appearance, :aroma,
:taste, :aftertaste, :impression.  Signal an error if the
frontmatter is missing or unclosed, or if any field is missing."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (looking-at "---[ \t]*\n")
        (error "Missing frontmatter (the first line must be ---)"))
      (let ((start (match-end 0)))
        (goto-char start)
        (unless (re-search-forward "^---[ \t]*$" nil t)
          (error "Unclosed frontmatter (no closing ---)"))
        (narrow-to-region start (match-beginning 0)))
      (cl-flet ((field (key pattern)
                  (goto-char (point-min))
                  (when (re-search-forward
                         (format "^%s: *\\(%s\\)" key pattern) nil t)
                    (match-string 1))))
        (let* ((keys '("iid" "date" "appearance" "aroma" "taste" "aftertaste" "impression"))
               (vals (mapcar (lambda (key)
                                 (field key (if (equal key "date") "[0-9-]+" "[0-9]+")))
                               keys))
               (missing (cl-loop for key in keys
                                 for v in vals
                                 unless v collect key)))
          (when missing
            (error "Missing fields: %s" (string-join missing ", ")))
          (cl-mapcan (lambda (key v)
                       (list (intern (concat ":" key))
                             (if (equal key "date") v (string-to-number v))))
                     keys vals))))))

(defun cellar--upsert-tasting (db data filename)
  "Upsert tasting DATA with FILENAME into the tastings table in DB."
  (sqlite-execute
   db
   "INSERT INTO tastings(iid, date, appearance, aroma, taste, aftertaste, impression, filename)
    VALUES(?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(filename) DO UPDATE SET
      iid=excluded.iid, date=excluded.date, appearance=excluded.appearance,
      aroma=excluded.aroma, taste=excluded.taste, aftertaste=excluded.aftertaste,
      impression=excluded.impression"
   (list (plist-get data :iid)
         (plist-get data :date)
         (plist-get data :appearance)
         (plist-get data :aroma)
         (plist-get data :taste)
         (plist-get data :aftertaste)
         (plist-get data :impression)
         filename)))

;;;###autoload
(defun cellar-sync-note ()
  "Sync the current tasting note buffer into the tastings table."
  (interactive)
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file"))
  (let ((data (cellar--parse-note))
        (filename (file-name-nondirectory (buffer-file-name))))
    (cellar--with-db db
      (cellar--upsert-tasting db data filename))
    (message "Synced %s" filename)))

(defun cellar--sync-note-quietly ()
  "Sync the current note, demoting errors to messages.
Used on `after-save-hook' so a bad note never blocks saving."
  (condition-case err
      (cellar-sync-note)
    (error (message "cellar: note not synced: %s"
                    (error-message-string err)))))

;;;###autoload
(define-minor-mode cellar-note-mode
  "Sync tasting note into the database every time it is saved."
  :lighter " Cellar"
  (if cellar-note-mode
      (add-hook 'after-save-hook #'cellar--sync-note-quietly nil t)
    (remove-hook 'after-save-hook #'cellar--sync-note-quietly t)))

;;; Selection Helpers
(defun cellar--read-producer (db)
  "Prompt for country, region, and producer in DB.
Return a cons cell of (PRODUCER . REGION)."
  (let* ((countries (sqlite-select
                     db
                     "SELECT DISTINCT country FROM producers
                      ORDER BY country"))
         (country (completing-read "Country: " countries))
         (regions (sqlite-select
                   db
                   "SELECT DISTINCT region FROM producers
                    WHERE country = ? ORDER BY region"
                   (list country)))
         (region (completing-read "Region: " regions))
         (producers (sqlite-select
                     db
                     "SELECT DISTINCT producer FROM producers
                      WHERE country = ? AND region = ? ORDER BY producer"
                     (list country region)))
         (producer (completing-read "Producer: " producers)))
    (cons producer region)))

(defun cellar--select-from-rows (rows prompt)
  "Select an id from ROWS using PROMPT.
Each row is a list of (ID LABEL); return the ID for the chosen LABEL.
The ID is appended to each label so that rows with identical labels
remain distinct."
  (let ((choices (make-hash-table :size (safe-length rows) :test 'equal)))
    (dolist (row rows)
      (puthash (format "%s (%s)" (cadr row) (car row)) (car row) choices))
    (or (gethash (completing-read prompt choices nil t) choices nil)
        (user-error "No matching wine"))))

(defun cellar--select-pid (db)
  "Select a producer id from the database DB."
  (let* ((pr (cellar--read-producer db))
         (rows (sqlite-select
                db
                "SELECT pid, concat_ws(' ',producer,region,subregion,
                                       vineyard,designation,varietal,category)
                 FROM producers WHERE producer = ? AND region = ?
                 ORDER BY subregion, vineyard"
                (list (car pr) (cdr pr)))))
    (cellar--select-from-rows rows "Bottle: ")))

(defun cellar--select-iid (db)
  "Select an inventory id from the database DB."
  (let* ((pr (cellar--read-producer db))
         (rows (sqlite-select
                db
                "SELECT iid, concat_ws(' ',vint,producer,subregion,vineyard,
                                       designation,varietal,category,btl)
                 FROM wines WHERE producer = ? AND region = ?
                 ORDER BY vint, subregion"
                (list (car pr) (cdr pr)))))
    (cellar--select-from-rows rows "Bottle: ")))

(defun cellar--check-inventory (db id offsite-min home-min)
  "Return non-nil if wine ID in DB has enough bottles in stock.
Requires at least OFFSITE-MIN bottles offsite (oi) and HOME-MIN
bottles at home (hi).  Returns nil when ID does not exist."
  (let ((winv (car (sqlite-select
                    db
                    "SELECT oi, hi FROM inventory WHERE iid = ?"
                    (list id)))))
    (when winv
      (and (>= (car winv) offsite-min) (>= (cadr winv) home-min)))))

(defun cellar--check-prefix (prefix)
  "Return the bottle count from the raw PREFIX argument.
A non-numeric prefix (nil or \\[universal-argument]) counts as 1.
Signal a `user-error' for counts below one."
  (let ((n (if (numberp prefix) prefix 1)))
    (when (< n 1)
      (user-error "Number of bottles must be at least 1"))
    n))

(cl-defun cellar--set-prod-value (db column value &key required where)
  "Return VALUE, or read a value for COLUMN of the producers table in DB.
Existing values of COLUMN are offered as completions, restricted to
rows matching WHERE, an alist of (COLUMN . VALUE).  When REQUIRED
is non-nil, prompt again until the input is not empty; otherwise
empty input returns nil."
  (or value
      (let* ((sql (format "SELECT DISTINCT %s
                           FROM producers
                           WHERE %s IS NOT NULL%s
                           ORDER BY %s"
                          column column
                          (mapconcat (lambda (w) (format " AND %s = ?" (car w)))
                                     where "")
                          column))
             (choices (sqlite-select db sql (mapcar #'cdr where)))
             (prompt (if required
                         (format "%s: " column)
                       (format "%s (empty for none): " column)))
             (ret (string-trim (completing-read prompt choices))))
        (while (and required (string-empty-p ret))
          (setq ret (string-trim (completing-read prompt choices))))
        (unless (string-empty-p ret) ret))))

(defun cellar--read-validated (given prompt label min max &optional current)
  "Return a validated number for an optional database field.
Use GIVEN when non-nil, otherwise read a number with PROMPT.  Empty
input returns CURRENT, and a value of 0 means \"unset\" and returns
nil.  Signal an error built from LABEL when the input is not a
number or the value is below MIN or above MAX."
  (let* ((in (unless given
               (string-trim
                (read-from-minibuffer
                 (if current
                     (format "%s (empty keeps %s, 0 clears): " prompt current)
                   (format "%s: " prompt))))))
         (n (cond (given)
                  ((string-empty-p in) current)
                  ((string-match-p "\\`[0-9]+\\(?:\\.[0-9]*\\)?\\'" in)
                   (string-to-number in))
                  (t (error "%s of %s is not a number" label in)))))
    (cond ((or (null n) (= n 0)) nil)
          ((or (< n min) (> n max))
           (error "%s of %s is invalid" label n))
          (t n))))

(cl-defun cellar--read-integer (given prompt label min max &key choices optional)
  "Return a validated integer for a database field.
Use GIVEN when non-nil, otherwise read an integer with PROMPT,
offering CHOICES as completions when non-nil.  When OPTIONAL is
non-nil, empty input returns nil.  Signal an error built from LABEL
when the input is empty and required, is not a whole number, or is
below MIN or above MAX."
  (let* ((in (unless given
               (string-trim
                (if choices
                    (completing-read (format "%s: " prompt) choices)
                  (read-from-minibuffer (format "%s: " prompt))))))
         (n (cond (given)
                  ((string-empty-p in)
                   (unless optional (error "%s is required" label)))
                  ((string-match-p "\\`[0-9]+\\'" in) (string-to-number in))
                  (t (error "%s of %s is not a whole number" label in)))))
    (when (and n (or (< n min) (> n max)))
      (error "%s of %s is invalid" label n))
    n))

;;; Core Operations
(cl-defun cellar--move (&key offsite insite consume iid)
  "Add OFFSITE, move INSITE from offsite and CONSUME from home.
If IID is nil, prompt user to select a wine."
  (cellar--with-db wdb
    ;; Moving INSITE bottles onsite needs that many offsite (oi); consuming
    ;; CONSUME bottles needs that many at home (hi).
    (let* ((iid (or iid (cellar--select-iid wdb)))
           (iok (cellar--check-inventory wdb iid (or insite 0) (or consume 0))))
      (unless iok
        (error "Not enough inventory"))
      (when offsite
        (message "%s bottles added off site, %s database updates"
                 offsite
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET oi = oi + ?
                                  WHERE iid = ?"
                                 (list offsite iid))))
      (when insite
        (message "%s bottles moved in site, %s database updates"
                 insite
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET oi = oi - ?, hi = hi + ?
                                  WHERE iid = ?"
                                 (list insite insite iid))))
      (when consume
        (message "%s bottles consumed, %s database updates"
                 consume
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET hi = hi - ?, ci = ci + ?
                                  WHERE iid = ?"
                                 (list consume consume iid)))))))

;;; Interactive Commands
;;;###autoload
(defun cellar-add-offsite (number)
  "Add a NUMBER of bottles to the offsite inventory."
  (interactive "P")
  (cellar--move :offsite (cellar--check-prefix number)))

;;;###autoload
(defun cellar-move-onsite (number)
  "Move a NUMBER of bottles onsite."
  (interactive "P")
  (cellar--move :insite (cellar--check-prefix number)))

;;;###autoload
(defun cellar-consume (number)
  "Consume NUMBER of bottles from in house inventory."
  (interactive "P")
  (cellar--move :consume (cellar--check-prefix number)))

;;;###autoload
(cl-defun cellar-note (&key iid)
  "Create a Markdown tasting note for wine with id IID.
If IID is nil, prompt user to select a wine."
  (interactive)
  (unless (file-directory-p cellar-note-dir)
    (error "Note directory %s does not exist" cellar-note-dir))
  (let ((filepath (expand-file-name
                   (format-time-string "%Y-%m-%dT%H:%M.md")
                   cellar-note-dir)))
    (when (file-exists-p filepath)
      (error "Note file %s already exists" filepath))
    (cellar--with-db wdb
      (let* ((iid (or iid (cellar--select-iid wdb)))
             (row (car (sqlite-select
                        wdb
                        "SELECT vint, producer, subregion, vineyard,
                                designation, varietal, category, btl
                         FROM wines WHERE iid = ?"
                        (list iid)))))
        (pcase-let ((`(,vint ,producer ,subregion ,vineyard
                       ,designation ,varietal ,category ,btl)
                     row))
          (find-file filepath)
          (insert (concat "---\n"
                          (format "title: %s %s\n" (or vint "NV") producer)
                          "keywords: [wine note]\n"
                          (format "iid: %s\n" iid)
                          (format "date: %s\n" (format-time-string "%Y-%m-%d"))
                          "appearance: 0  # /3\n"
                          "aroma: 0       # /5\n"
                          "taste: 0       # /5\n"
                          "aftertaste: 0  # /3\n"
                          "impression: 0  # /4\n"
                          "---\n\n"
                          (when subregion
                            (format "- Subregion: %s\n" subregion))
                          (when vineyard
                            (format "- Vineyard: %s\n" vineyard))
                          (when designation
                            (format "- Designation: %s\n" designation))
                          (format "- Varietal: %s\n" varietal)
                          (when category
                            (format "- Category: %s\n" category))
                          (format "- Size: %s ml\n" btl)
                          "\n## Notes\n\n"))
          (goto-char (point-min))
          (cellar-note-mode 1))))))

;;;###autoload
(cl-defun cellar-add-producer (&key producer
                                    varietal
                                    vineyard
                                    designation
                                    subregion
                                    region
                                    country
                                    category)
  "Add producer to database.

Values are COUNTRY, REGION, SUBREGION, PRODUCER, VARIETAL,
VINEYARD, DESIGNATION and CATEGORY, prompted for in that order.
COUNTRY, REGION, PRODUCER and VARIETAL are required."
  (interactive)
  (cellar--with-db wdb
    (let* ((country (cellar--set-prod-value wdb "Country" country :required t))
           (region (cellar--set-prod-value wdb "Region" region :required t
                                           :where `(("country" . ,country))))
           (place `(("country" . ,country) ("region" . ,region)))
           (subregion (cellar--set-prod-value wdb "Subregion" subregion :where place))
           (producer (cellar--set-prod-value wdb "Producer" producer :required t
                                             :where place))
           (varietal (cellar--set-prod-value wdb "Varietal" varietal :required t))
           (vineyard (cellar--set-prod-value wdb "Vineyard" vineyard))
           (designation (cellar--set-prod-value wdb "Designation" designation))
           (category (cellar--set-prod-value wdb "Category" category))
           (dup (caar (sqlite-select
                       wdb
                       "SELECT pid FROM producers
                        WHERE producer = ? AND varietal = ?
                          AND IFNULL(vineyard, '') = IFNULL(?, '')
                          AND IFNULL(designation, '') = IFNULL(?, '')
                          AND IFNULL(category, '') = IFNULL(?, '')"
                       (list producer varietal vineyard designation category))))
           (val (list
                 producer varietal vineyard designation subregion region country category))
           (sql "INSERT INTO producers(producer,varietal,vineyard,
                                       designation,subregion,region,country,category)
                 VALUES(?,?,?,?,?,?,?,?)"))
      (when dup
        (user-error "Producer already exists (id %s)" dup))
      (message "%s database updates" (sqlite-execute wdb sql val)))))

;;;###autoload
(cl-defun cellar-add-bottle (&key vint btl oi alc w0 w1)
  "Add bottle to database.

Values are VINT, BTL, OI, ALC, W0, and W1.  An empty vintage
means a non-vintage (NV) wine."
  (interactive)
  (cellar--with-db wdb
    (let* ((sql "INSERT INTO inventory(pid,vint,btl,oi,alc,w0,w1)
                 VALUES(?,?,?,?,?,?,?)")
           (pid (cellar--select-pid wdb))
           (vint (cellar--read-integer vint "Vintage (empty for NV)" "Vintage"
                                       1900 2050 :optional t))
           (btl (cellar--read-integer btl "Size" "Bottle size" 187 3000
                                      :choices '("750" "375" "1500")))
           (dup (caar (sqlite-select
                       wdb
                       "SELECT iid FROM inventory
                        WHERE pid = ? AND IFNULL(vint, 0) = IFNULL(?, 0) AND btl = ?"
                       (list pid vint btl)))))
      (when dup
        (user-error "This wine is already in inventory (id %s); use `cellar-add-offsite'"
                    dup))
      (let* ((oi (cellar--read-integer
                  oi "Quantity" "Quantity" 1 most-positive-fixnum
                  :choices '("6" "12" "1" "2" "3" "4" "5" "7" "8" "9" "10" "11")))
             (alc (cellar--read-validated alc "Alcohol (e.g. 13.5)" "Alcohol" 0.0 100.0))
             (w0 (cellar--read-validated w0 "Window start" "Window start" 2000 2100))
             (w1 (cellar--read-validated w1 "Window end" "Window end" 2000 2100)))
        (when (and w0 w1 (> w0 w1))
          (error "Window start %s is after window end %s" w0 w1))
        (message "%s database updates"
                 (sqlite-execute wdb sql (list pid vint btl oi alc w0 w1)))))))

;;;###autoload
(cl-defun cellar-update-window (&key w0 w1 iid)
  "Update window (W0 to W1) for wine with id IID.
When prompting, empty input keeps the current value."
  (interactive)
  (cellar--with-db wdb
    (let* ((sql "UPDATE inventory SET w0=?, w1=? WHERE iid=?")
           (iid (or iid (cellar--select-iid wdb)))
           (cur (car (sqlite-select wdb "SELECT w0, w1 FROM inventory WHERE iid=?"
                                    (list iid))))
           (w0 (cellar--read-validated w0 "Window start" "Window start" 2000 2100
                                       (car cur)))
           (w1 (cellar--read-validated w1 "Window end" "Window end" 2000 2100
                                       (cadr cur))))
      (when (and w0 w1 (> w0 w1))
        (error "Window start %s is after window end %s" w0 w1))
      (message "%s database updates"
               (sqlite-execute wdb sql (list w0 w1 iid))))))

;;;###autoload
(cl-defun cellar-update-alcohol (&key alc iid)
  "Update alcohol to ALC for wine with id IID.
When prompting, empty input keeps the current value."
  (interactive)
  (cellar--with-db wdb
    (let* ((sql "UPDATE inventory SET alc=? WHERE iid=?")
           (iid (or iid (cellar--select-iid wdb)))
           (cur (caar (sqlite-select wdb "SELECT alc FROM inventory WHERE iid=?"
                                     (list iid))))
           (alc (cellar--read-validated alc "Alcohol (e.g. 13.5)" "Alcohol" 0.0 100.0
                                        cur)))
      (message "%s database updates"
               (sqlite-execute wdb sql (list alc iid))))))

;;;###autoload
(cl-defun cellar-add-bottle-annotation (&key note iid)
  "Add NOTE to bottle with id IID."
  (interactive)
  (let ((note (or note (read-from-minibuffer "Bottle note: "))))
    (cellar--with-db wdb
      (let ((iid (or iid (cellar--select-iid wdb))))
        (message "%s database updates"
                 (sqlite-execute wdb
                                 "INSERT INTO notes(iid, note) VALUES(?,?)"
                                 (list iid note)))))))

;;;###autoload
(defun cellar-sync-notes ()
  "Batch import all Markdown tasting notes into the tastings table.
Scans `cellar-note-dir' for *.md files, parses each one, and
upserts into the database.  Reports the number of notes synced,
and warns when tastings remain whose note file no longer exists
\(see `cellar-check-notes').

Every *.md file in the directory is treated as a tasting note.
The import is all or nothing: the first file that fails to parse
or insert aborts the sync, rolls back every change, and reports
the offending file."
  (interactive)
  (unless (file-directory-p cellar-note-dir)
    (error "Note directory %s does not exist" cellar-note-dir))
  (let ((files (directory-files cellar-note-dir t "\\.md\\'"))
        (count 0)
        (orphans nil))
    (cellar--with-db db
      (with-sqlite-transaction db
        (dolist (f files)
          (condition-case err
              (with-temp-buffer
                (insert-file-contents f)
                (cellar--upsert-tasting db (cellar--parse-note)
                                        (file-name-nondirectory f))
                (setq count (1+ count)))
            (error
             (user-error "Sync aborted, no notes synced.  %s: %s"
                         (abbreviate-file-name f)
                         (error-message-string err))))))
      (setq orphans (cellar--orphan-tastings db)))
    (if orphans
        (message "Synced %d tasting notes; %d tasting(s) with no note file (M-x cellar-check-notes)"
                 count (length orphans))
      (message "Synced %d tasting notes" count))))

(defun cellar--orphan-tastings (db)
  "Return the tastings in DB whose note file is not in `cellar-note-dir'.
Each element is a list (TID IID DATE FILENAME)."
  (let ((files (directory-files cellar-note-dir nil "\\.md\\'")))
    (cl-remove-if (lambda (row) (member (nth 3 row) files))
                  (sqlite-select db "SELECT tid, iid, date, filename FROM tastings
                                    ORDER BY date, filename"))))

;;;###autoload
(defun cellar-check-notes ()
  "List tastings whose note file no longer exists in `cellar-note-dir'.
This only reports; fix the database with sqlite3 if needed."
  (interactive)
  (unless (file-directory-p cellar-note-dir)
    (error "Note directory %s does not exist" cellar-note-dir))
  (let ((orphans (cellar--with-db db (cellar--orphan-tastings db))))
    (if (null orphans)
        (message "Every tasting has a note file")
      (let ((buf (get-buffer-create "*Cellar: missing notes*")))
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Tastings with no note file in %s (%d)\n"
                            (abbreviate-file-name cellar-note-dir) (length orphans)))
            (insert (make-string 40 ?-) "\n")
            (pcase-dolist (`(,tid ,iid ,date ,filename) orphans)
              (insert (format "tid %-4s iid %-4s %s  %s\n" tid iid date filename))))
          (goto-char (point-min)))
        (switch-to-buffer buf)))))

;;; Browse Mode
(defface cellar-list-ready '((t :inherit success))
  "Face for wines currently within their drinking window.")

(defface cellar-list-past-window '((t :inherit shadow))
  "Face for wines past their drinking window.")

(defvar-local cellar-list--filter nil
  "Current filter as (COLUMN . VALUE) or nil.")

(defun cellar-list--window-string (w0 w1)
  "Format the drinking window from year W0 to year W1.
Wines whose window includes the current year get
`cellar-list-ready'; wines past their window get
`cellar-list-past-window'."
  (let ((year (string-to-number (format-time-string "%Y")))
        (str (cond ((and w0 w1) (format "%s-%s" w0 w1))
                   (w0 (format "%s-" w0))
                   (w1 (format "-%s" w1))
                   (t ""))))
    (cond
     ((string-empty-p str) str)
     ((and w1 (< w1 year))
      (propertize str 'face 'cellar-list-past-window))
     ((and w0 (<= w0 year) (or (null w1) (<= year w1)))
      (propertize str 'face 'cellar-list-ready))
     (t str))))

(defun cellar-list--update-mode-line ()
  "Reflect the active filter (if any) in the mode line."
  (setq mode-line-process
        (when cellar-list--filter
          (format " [%s=%s]" (car cellar-list--filter)
                  (cdr cellar-list--filter))))
  (force-mode-line-update))

(defun cellar-list--entries ()
  "Fetch wine entries from database for tabulated list."
  (cellar--with-db db
    (let* ((where (when cellar-list--filter
                    (format " WHERE %s = ?" (car cellar-list--filter))))
           (sql (concat "SELECT iid, producer, region, subregion, vineyard, "
                        "varietal, designation, vint, tot, w0, w1 "
                        "FROM wines"
                        where
                        " ORDER BY producer, vint"))
           (params (when cellar-list--filter
                     (list (cdr cellar-list--filter))))
           (rows (sqlite-select db sql params)))
      (mapcar (lambda (row)
                (list (nth 0 row)
                      (vector
                       (format "%s" (nth 0 row))
                       (or (nth 1 row) "")
                       (or (nth 2 row) "")
                       (or (nth 3 row) "")
                       (or (nth 4 row) "")
                       (or (nth 5 row) "")
                       (or (nth 6 row) "")
                       (format "%s" (or (nth 7 row) ""))
                       (format "%s" (or (nth 8 row) ""))
                       (cellar-list--window-string (nth 9 row) (nth 10 row)))))
              rows))))

(defvar-keymap cellar-list-mode-map
  :doc "Keymap for `cellar-list-mode'."
  "c" #'cellar-list-consume
  "m" #'cellar-list-move-onsite
  "a" #'cellar-list-add-offsite
  "w" #'cellar-list-update-window
  "A" #'cellar-list-update-alcohol
  "n" #'cellar-list-add-bottle-annotation
  "N" #'cellar-list-note
  "RET" #'cellar-list-detail
  "f" #'cellar-list-filter
  "F" #'cellar-list-clear-filter
  "?" #'cellar-list-menu)

(defun cellar-list--numeric-sorter (col)
  "Return a predicate sorting tabulated list entries numerically on COL."
  (lambda (a b)
    (< (string-to-number (aref (cadr a) col))
       (string-to-number (aref (cadr b) col)))))

(define-derived-mode cellar-list-mode tabulated-list-mode "Cellar"
  "Major mode for browsing wine cellar inventory.

Press \\[cellar-list-menu] for a menu of commands.

\\{cellar-list-mode-map}"
  (setq tabulated-list-format
        (vector `("iid" 5 ,(cellar-list--numeric-sorter 0) :right-align t)
                '("Producer" 25 t)
                '("Region" 15 t)
                '("Subregion" 15 t)
                '("Vineyard" 20 t)
                '("Varietal" 15 t)
                '("Designation" 12 t)
                `("Vint" 5 ,(cellar-list--numeric-sorter 7))
                `("Tot" 4 ,(cellar-list--numeric-sorter 8) :right-align t)
                '("Window" 10 t)))
  (setq tabulated-list-entries #'cellar-list--entries)
  (setq tabulated-list-sort-key (cons "Producer" nil))
  (tabulated-list-init-header)
  (cellar-list--update-mode-line)
  (hl-line-mode 1))

;;;###autoload
(defun cellar-browse ()
  "Browse wine cellar inventory in a tabulated list."
  (interactive)
  (let ((buf (get-buffer-create "*Cellar*")))
    (with-current-buffer buf
      (cellar-list-mode)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun cellar-list--label (iid)
  "Return a \"VINT PRODUCER (id IID)\" label for the row at point."
  (let* ((entry (tabulated-list-get-entry))
         (vint (aref entry 7))
         (producer (aref entry 1)))
    (format "%s %s (id %s)"
            (if (string= vint "") "NV" vint)
            producer iid)))

(defun cellar-list--iid-at-point ()
  "Return the inventory id of the row at point or signal a `user-error'."
  (or (tabulated-list-get-id) (user-error "No wine at point")))

(defun cellar-list--move-at-point (prompt key number)
  "Confirm with PROMPT, then move NUMBER bottles of the wine at point.
PROMPT is a format string taking the bottle count and the wine
label.  KEY is one of :offsite, :insite or :consume, passed to
`cellar--move'."
  (let ((iid (cellar-list--iid-at-point))
        (n (cellar--check-prefix number)))
    (when (yes-or-no-p (format prompt n (cellar-list--label iid)))
      (cellar--move key n :iid iid)
      (tabulated-list-revert))))

(defun cellar-list-consume (number)
  "Consume NUMBER bottles of wine at point."
  (interactive "P")
  (cellar-list--move-at-point "Consume %d bottle(s) of %s? " :consume number))

(defun cellar-list-move-onsite (number)
  "Move NUMBER bottles from offsite to home for wine at point."
  (interactive "P")
  (cellar-list--move-at-point "Move %d bottle(s) onsite for %s? " :insite number))

(defun cellar-list-add-offsite (number)
  "Add NUMBER bottles to offsite for wine at point."
  (interactive "P")
  (cellar-list--move-at-point "Add %d bottle(s) offsite for %s? " :offsite number))

(defun cellar-list-update-window ()
  "Update drinking window for wine at point."
  (interactive)
  (cellar-update-window :iid (cellar-list--iid-at-point))
  (tabulated-list-revert))

(defun cellar-list-update-alcohol ()
  "Update alcohol percentage for wine at point."
  (interactive)
  (cellar-update-alcohol :iid (cellar-list--iid-at-point))
  (tabulated-list-revert))

(defun cellar-list-add-bottle-annotation ()
  "Add a database note for wine at point."
  (interactive)
  (cellar-add-bottle-annotation :iid (cellar-list--iid-at-point))
  (tabulated-list-revert))

(defun cellar-list-note ()
  "Create a tasting note for wine at point."
  (interactive)
  (cellar-note :iid (cellar-list--iid-at-point)))

(defun cellar-list-detail ()
  "Show full details and notes for the wine at point."
  (interactive)
  (let* ((iid (cellar-list--iid-at-point))
         (data (cellar--with-db db
                 (list (car (sqlite-select db
                              "SELECT vint, producer, region, subregion, vineyard,
                                      designation, varietal, category, btl, oi, hi,
                                      ci, alc, w0, w1, country
                               FROM wines WHERE iid = ?" (list iid)))
                       (sqlite-select db
                         "SELECT note, created_at FROM notes
                          WHERE iid = ? ORDER BY created_at" (list iid)))))
         (notes (cadr data)))
    (pcase-let ((`(,vint ,producer ,region ,subregion ,vineyard ,designation
                   ,varietal ,category ,btl ,oi ,hi ,ci ,alc ,w0 ,w1 ,country)
                 (car data)))
      (let ((buf (get-buffer-create
                  (format "*Cellar: %s %s*" (or producer "Unknown") (or vint "NV")))))
        (with-current-buffer buf
          (special-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Wine Details (iid: %s)\n" iid))
            (insert (make-string 40 ?=) "\n\n")
            (pcase-dolist (`(,label . ,value)
                           `(("Producer" . ,producer)
                             ("Varietal" . ,varietal)
                             ("Vintage" . ,(or vint "NV"))
                             ("Country" . ,country)
                             ("Region" . ,region)
                             ("Subregion" . ,subregion)
                             ("Vineyard" . ,vineyard)
                             ("Designation" . ,designation)
                             ("Category" . ,category)
                             ("Bottle" . ,(and btl (format "%s ml" btl)))
                             ("Offsite" . ,(or oi 0))
                             ("Home" . ,(or hi 0))
                             ("Consumed" . ,(or ci 0))
                             ("Alcohol" . ,(and alc (format "%.1f%%" alc)))
                             ("Window" . ,(cellar-list--window-string w0 w1))))
              (insert (format "%-12s %s\n" (concat label ":") (or value ""))))
            (insert "\n")
            (if notes
                (progn
                  (insert "Notes\n")
                  (insert (make-string 40 ?-) "\n")
                  (pcase-dolist (`(,note ,created) notes)
                    (insert (format "[%s] %s\n" (or created "") note))))
              (insert "No notes.\n")))
          (goto-char (point-min)))
        (switch-to-buffer buf)))))

(defun cellar-list-filter ()
  "Filter wines by country, region, varietal, or producer."
  (interactive)
  (let ((field (completing-read "Filter by: "
                                '("country" "region" "varietal" "producer")
                                nil t)))
    (cellar--with-db db
      ;; Pull values from the wines view so only values present in the
      ;; inventory are offered.
      (let* ((choices (sqlite-select db
                        (format "SELECT DISTINCT %s FROM wines
                                 WHERE %s IS NOT NULL ORDER BY %s"
                                field field field)))
             (value (completing-read (format "%s: " (capitalize field)) choices)))
        (setq cellar-list--filter (cons field value)))))
  (cellar-list--update-mode-line)
  (tabulated-list-revert))

(defun cellar-list-clear-filter ()
  "Clear any active filter."
  (interactive)
  (setq cellar-list--filter nil)
  (cellar-list--update-mode-line)
  (tabulated-list-revert))

(transient-define-prefix cellar-list-menu ()
  "Menu of commands for `cellar-list-mode'."
  [["Inventory"
    ("a" "Add offsite" cellar-list-add-offsite)
    ("m" "Move onsite" cellar-list-move-onsite)
    ("c" "Consume" cellar-list-consume)]
   ["Edit"
    ("w" "Drinking window" cellar-list-update-window)
    ("A" "Alcohol" cellar-list-update-alcohol)
    ("n" "Annotate bottle" cellar-list-add-bottle-annotation)
    ("N" "Tasting note" cellar-list-note)]
   ["View"
    ("RET" "Details" cellar-list-detail)
    ("f" "Filter" cellar-list-filter)
    ("F" "Clear filter" cellar-list-clear-filter)]])

(provide 'cellar)
;;; cellar.el ends here
