;;; org-clock-projects-export-test.el --- Tests for the CSV export -*- lexical-binding: t; -*-

;; The export command is one seam and it covers the whole exporter.  Period
;; resolution, key interning, kind classification, midnight normalisation, the
;; inclusive end day, the archive-derived file set, both scan scopes, the
;; period and tag gates, whole-entry output, sorting, the header, every
;; filename form, the empty result, both raising checks and each prefix level
;; are all visible in the file that lands on disk, in the prompts the flow
;; raised, or in the error it signalled.  Nothing beneath the command is
;; reached for: no private helper is called and no internal data structure is
;; inspected.  A test that would still pass after the internals were rewritten,
;; and fail if the user-visible behaviour changed, is the target.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'org-clock-projects-test-harness)

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-clock-csv)
(require 'parse-time)
(require 'org-clock-projects)


;;;; The world the tests run in

(defconst org-clock-projects-export-test-now
  (org-clock-projects-test-time-at 2026 9 3 14 37)
  "The instant every test runs at: an afternoon in September 2026.
The time of day is deliberately not midnight.  The date picker reports
the day it was handed at the current wall-clock time, so an afternoon is
what makes a range that was never normalised lose its first morning and
its last evening.  September is deliberately the month after the one most
fixtures clock into, so `lastmonth' has an unambiguous answer.")

(defconst org-clock-projects-export-test-registry
  '((:tag "cafe" :name "Corner Café" :file "corner-cafe.org")
    (:tag "acme" :name "Acme Ltd" :file "acme.org"))
  "Two registered projects whose tag, display name and file base all differ.
Each identifier therefore has exactly one place it can legitimately come
from: the name reaches the prompt, the tag selects entries, the file base
names the export.  A second project makes a mis-selection visible as rows
that do not belong.")

(defconst org-clock-projects-export-test-project "Corner Café"
  "The answer to the project prompt for the project every test exports.")

(defconst org-clock-projects-export-test-name
  "2026-08-org-clock-corner-cafe.csv"
  "The file an August export of that project lands in.")

(defun org-clock-projects-export-test-tagged-file (tag &rest entries)
  "Return the text of an Org file carrying file-level TAG and holding ENTRIES.
The tag is a `#+FILETAGS:' keyword rather than a headline tag because
that is how a project file is tagged in practice, and matching it
requires inheritance to reach the file level."
  (concat (if tag (format "#+FILETAGS: :%s:\n" tag) "")
          (apply #'concat entries)))

(defun org-clock-projects-export-test-clocked (heading start end)
  "Return an Org entry for HEADING carrying one closed clock from START to END.
START and END are (YEAR MONTH DAY HOUR MINUTE) lists."
  (org-clock-projects-test-closed-entry
   heading
   (cons (apply #'org-clock-projects-test-time-at start)
         (apply #'org-clock-projects-test-time-at end))))

(defun org-clock-projects-export-test-running (heading start)
  "Return an Org entry for HEADING carrying an open clock started at START.
START is a (YEAR MONTH DAY HOUR MINUTE) list."
  (org-clock-projects-test-clock-at
   heading (apply #'org-clock-projects-test-time-at start)))

(defmacro org-clock-projects-export-test-with-registry (registry contents &rest body)
  "Run BODY over CONTENTS with REGISTRY as the projects the resolver returns.
The clock is frozen, because a relative period and the date picker are
both read off it.  `org-archive-location' points at a directory the
retired exporter's hardcoded `archive/<name>' layout would never look in,
so an archived entry can only be found by resolving the setting.
`org-extend-today-until' is deliberately non-zero: a billing period is a
calendar-day concept, and a period resolved without binding it away would
sit three hours off every boundary asserted here.  The export directory
does not exist, so the first export has to create it."
  (declare (indent 2) (debug t))
  `(org-clock-projects-test-with-data nil ,contents
     (let ((org-clock-projects-projects-function (lambda () ,registry))
           (org-clock-projects-export-directory
            (expand-file-name "exports" org-directory))
           (org-clock-projects-export-file-function
            #'org-clock-projects-export-default-file-name)
           (org-clock-projects-duration-threshold 10)
           (org-archive-location "attic/%s::")
           (org-agenda-files nil)
           (org-use-tag-inheritance t)
           (org-tags-exclude-from-inheritance nil)
           (org-extend-today-until 3))
       (org-clock-projects-test-at org-clock-projects-export-test-now
         ,@body))))

(defmacro org-clock-projects-export-test-with-project (contents &rest body)
  "Run BODY over CONTENTS with the two-project registry in place."
  (declare (indent 1) (debug t))
  `(org-clock-projects-export-test-with-registry
       org-clock-projects-export-test-registry ,contents
     ,@body))

(defun org-clock-projects-export-test-agenda (&rest files)
  "Point `org-agenda-files' at FILES inside the sandbox."
  (setq org-agenda-files
        (mapcar (lambda (file) (expand-file-name file org-directory)) files)))


;;;; Reading the written file back

(defun org-clock-projects-export-test-path (name)
  "Return the path the export named NAME takes."
  (expand-file-name name org-clock-projects-export-directory))

(defun org-clock-projects-export-test-exports ()
  "Return the names of the CSV files in the export directory, sorted.
Nil when nothing has been written, which is also the answer when the
directory was never created."
  (let ((directory org-clock-projects-export-directory))
    (and (file-directory-p directory)
         (sort (directory-files directory nil "\\.csv\\'") #'string<))))

(defun org-clock-projects-export-test-header (name)
  "Return the first line of the export named NAME."
  (car (split-string
        (or (org-clock-projects-test-file-contents
             (org-clock-projects-export-test-path name))
            "")
        "\n")))

(defun org-clock-projects-export-test-rows (name)
  "Return the data rows of the export named NAME as field lists, in file order.
No fixture heading contains a comma, so splitting on one recovers the
columns without reimplementing CSV quoting."
  (mapcar (lambda (row) (split-string row ","))
          (org-clock-projects-test-csv-rows
           (org-clock-projects-export-test-path name))))

(defun org-clock-projects-export-test-tasks (name)
  "Return the task column of the export named NAME, in file order."
  (mapcar #'car (org-clock-projects-export-test-rows name)))

(defun org-clock-projects-export-test-spans (name)
  "Return the (START END) columns of the export named NAME, in file order."
  (mapcar (lambda (fields) (list (nth 3 fields) (nth 4 fields)))
          (org-clock-projects-export-test-rows name)))

(defun org-clock-projects-export-test-minutes (name)
  "Return the total minutes the export named NAME accounts for.
Each row is measured between the two timestamp columns, so the total is
read off the file exactly as an auditor would read it."
  (apply #'+ (mapcar (lambda (span)
                       (/ (float-time
                           (time-subtract
                            (org-encode-time (parse-time-string (nth 1 span)))
                            (org-encode-time (parse-time-string (car span)))))
                          60))
                     (org-clock-projects-export-test-spans name))))


;;;; Shared fixtures

(defun org-clock-projects-export-test-boundary-contents ()
  "Return contents holding one entry at each interesting instant.
Around the July/August boundary sit an entry ending exactly on it, one
starting exactly on it, and a zero-length one sitting on it; around the
August/September boundary sits an entry crossing it.  No two of them
overlap, so the fixture says nothing about the overlap check."
  `(("corner-cafe.org"
     . ,(org-clock-projects-export-test-tagged-file
         "cafe"
         (org-clock-projects-export-test-clocked
          "July tail" '(2026 7 31 20 0) '(2026 8 1 0 0))
         (org-clock-projects-export-test-clocked
          "August head" '(2026 8 1 0 0) '(2026 8 1 3 0))
         (org-clock-projects-export-test-clocked
          "Boundary instant" '(2026 8 1 0 0) '(2026 8 1 0 0))
         (org-clock-projects-export-test-clocked
          "Crossing midnight" '(2026 8 31 22 0) '(2026 9 1 2 0))))))

(defun org-clock-projects-export-test-overlapping-contents ()
  "Return contents where the project's own file overlaps itself in August.
A second tagged file outside the project makes the scan scope visible in
the same fixture, so one set of files answers both what the checks did
and how far the scan reached."
  `(("corner-cafe.org"
     . ,(org-clock-projects-export-test-tagged-file
         "cafe"
         (org-clock-projects-export-test-clocked
          "First pour" '(2026 8 10 9 0) '(2026 8 10 11 0))
         (org-clock-projects-export-test-clocked
          "Second pour" '(2026 8 10 10 0) '(2026 8 10 12 0))))
    ("stray.org"
     . ,(org-clock-projects-export-test-tagged-file
         "cafe"
         (org-clock-projects-export-test-clocked
          "Filed elsewhere" '(2026 8 12 9 0) '(2026 8 12 10 0))))))

(defun org-clock-projects-export-test-scope-contents ()
  "Return contents where the narrow and the wide scan differ.
The project's own file, a second file carrying the project's tag, and a
third carrying somebody else's: widening reaches the second and must
still refuse the third."
  `(("corner-cafe.org"
     . ,(org-clock-projects-export-test-tagged-file
         "cafe"
         (org-clock-projects-export-test-clocked
          "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))))
    ("stray.org"
     . ,(org-clock-projects-export-test-tagged-file
         "cafe"
         (org-clock-projects-export-test-clocked
          "Filed elsewhere" '(2026 8 6 9 0) '(2026 8 6 10 0))))
    ("acme.org"
     . ,(org-clock-projects-export-test-tagged-file
         "acme"
         (org-clock-projects-export-test-clocked
          "Not ours" '(2026 8 7 9 0) '(2026 8 7 10 0))))))


;;;; Choosing a project

(ert-deftest org-clock-projects-export-test-a-project-is-chosen-by-its-registry-name ()
  "The prompt offers registry names, and each identifier lands where it belongs.
Nothing has been clocked in this session, so the candidates can only have
come from the registry rather than from projects the clocking side
created.  Answering with the display name is what proves the prompt shows
it: the tag and the file base are different strings, and either of them
would have to be answered instead had the prompt been built from them."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-scope-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal (list org-clock-projects-export-test-name)
                   (org-clock-projects-export-test-exports)))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-a-files-returning-resolver-still-exports ()
  "A resolver handing back bare file names keeps working.
Such a record has no tag, so there is nothing to gate on and every entry
in the period is exported, tagged or not.  Its display name and its file
base are both the file's base name, which is the only identifier a bare
file name can supply."
  (org-clock-projects-export-test-with-registry
      '("solo.org")
      `(("solo.org"
         . ,(org-clock-projects-export-test-tagged-file
             nil
             (org-clock-projects-export-test-clocked
              "Untagged work" '(2026 8 4 9 0) '(2026 8 4 10 0)))))
    (org-clock-projects-test-with-prompts '("solo" "2026-08")
      (org-clock-projects-export))
    (should (equal '("2026-08-org-clock-solo.csv")
                   (org-clock-projects-export-test-exports)))
    (should (equal '("Untagged work")
                   (org-clock-projects-export-test-tasks
                    "2026-08-org-clock-solo.csv")))))


;;;; Choosing a period

(ert-deftest org-clock-projects-export-test-a-relative-key-resolves-through-the-prompt ()
  "A key picked from the completion list resolves to its period.
The prompt hands back a string and the range resolver dispatches its
relative keys on symbols, so an uninterned answer raises \"No such time
block\" instead of exporting.  The prompt also has to name the typed forms
it accepts, since a completion list announces nothing about free text."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-scope-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "lastmonth")
      (org-clock-projects-export)
      (should (equal '("lastmonth" "thismonth" "lastq" "thisq"
                       "lastweek" "thisweek" "lastyear" "thisyear" "range")
                     (org-clock-projects-test-candidates)))
      (should (equal "Period (or YYYY-MM, YYYY-QN, YYYY, YYYY-Www): "
                     (nth 1 (org-clock-projects-test-prompts)))))
    (should (equal (list org-clock-projects-export-test-name)
                   (org-clock-projects-export-test-exports)))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-a-quarter-names-its-file ()
  "A quarter is classified as one and named for it.
Its bounds coincide with no month, so a quarter that failed to classify
would fall through to the two-date interval form."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "In the quarter" '(2026 7 9 9 0) '(2026 7 9 10 0))
             (org-clock-projects-export-test-clocked
              "After the quarter" '(2026 10 1 9 0) '(2026 10 1 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-Q3")
      (org-clock-projects-export))
    (should (equal '("2026-Q3-org-clock-corner-cafe.csv")
                   (org-clock-projects-export-test-exports)))
    (should (equal '("In the quarter")
                   (org-clock-projects-export-test-tasks
                    "2026-Q3-org-clock-corner-cafe.csv")))))

(ert-deftest org-clock-projects-export-test-a-year-names-its-file ()
  "A year is named by its number alone and spans the whole calendar year."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "New year" '(2026 1 1 9 0) '(2026 1 1 10 0))
             (org-clock-projects-export-test-clocked
              "Old year" '(2025 12 31 9 0) '(2025 12 31 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026")
      (org-clock-projects-export))
    (should (equal '("2026-org-clock-corner-cafe.csv")
                   (org-clock-projects-export-test-exports)))
    (should (equal '("New year")
                   (org-clock-projects-export-test-tasks
                    "2026-org-clock-corner-cafe.csv")))))

(ert-deftest org-clock-projects-export-test-a-week-is-named-by-its-iso-week-year ()
  "The first ISO week of 2026 begins in December 2025 and is still called 2026.
Naming the file from the calendar year of the week's first day would call
this week 2025-W01, which names a week that does not exist."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Between the years" '(2025 12 30 9 0) '(2025 12 30 11 0))
             (org-clock-projects-export-test-clocked
              "The week after" '(2026 1 5 9 0) '(2026 1 5 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-W01")
      (org-clock-projects-export))
    (should (equal '("2026-W01-org-clock-corner-cafe.csv")
                   (org-clock-projects-export-test-exports)))
    (should (equal '("Between the years")
                   (org-clock-projects-export-test-tasks
                    "2026-W01-org-clock-corner-cafe.csv")))))

(ert-deftest org-clock-projects-export-test-a-range-is-normalised-and-includes-its-end-day ()
  "Both picked dates are truncated to midnight and the end day is included.
The picker returns the picked day at the current wall-clock time, which
is an afternoon here.  Taken verbatim, the range would start after the
morning of its first day and stop before the evening of its last, so the
first and the last entry each fail on a different half of the
normalisation.  The day after the picked end stays out, which is what
makes the end inclusive rather than merely generous."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Before the range" '(2026 3 1 23 0) '(2026 3 1 23 30))
             (org-clock-projects-export-test-clocked
              "First morning" '(2026 3 2 8 0) '(2026 3 2 9 0))
             (org-clock-projects-export-test-clocked
              "Middle" '(2026 3 20 10 0) '(2026 3 20 11 0))
             (org-clock-projects-export-test-clocked
              "Last evening" '(2026 4 16 18 0) '(2026 4 16 19 0))
             (org-clock-projects-export-test-clocked
              "After the range" '(2026 4 17 9 0) '(2026 4 17 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "range"
              "2026-03-02" "2026-04-16")
      (org-clock-projects-export)
      (should (equal '("Range start (inclusive): " "Range end (inclusive): ")
                     (nthcdr 2 (org-clock-projects-test-prompts)))))
    (should (equal '("2026-03-02--2026-04-16-org-clock-corner-cafe.csv")
                   (org-clock-projects-export-test-exports)))
    (should (equal '("First morning" "Middle" "Last evening")
                   (org-clock-projects-export-test-tasks
                    "2026-03-02--2026-04-16-org-clock-corner-cafe.csv")))))

(ert-deftest org-clock-projects-export-test-a-single-day-is-refused ()
  "A day is refused, whether it was typed or named.
A bare date is far more likely a slip for a month than a deliberate
one-day invoice, and refusing the kind refuses `today' with it.  Nothing
is written, so the refusal cannot be mistaken for an empty period."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-scope-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08-15")
      (should-error (org-clock-projects-export) :type 'user-error))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "today")
      (should-error (org-clock-projects-export) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))))

(ert-deftest org-clock-projects-export-test-untilnow-is-refused ()
  "`untilnow' is refused because it resolves without a start.
It is a period with no beginning, which no membership decision can be
made against."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-scope-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "untilnow")
      (should-error (org-clock-projects-export) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))))


;;;; Which entries belong to a period

(ert-deftest org-clock-projects-export-test-membership-is-strict-at-both-ends ()
  "An entry belongs to a period when it truly overlaps it.
Read from July, August and September in turn, the same four entries make
every exact instant observable: the one ending on the July/August
boundary belongs to July alone, the one starting on it to August alone,
the zero-length one on it to neither, and the one crossing into
September to both months it touches."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-boundary-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-07")
      (org-clock-projects-export))
    (should (equal '("July tail")
                   (org-clock-projects-export-test-tasks
                    "2026-07-org-clock-corner-cafe.csv")))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("August head" "Crossing midnight")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-09")
      (org-clock-projects-export))
    (should (equal '("Crossing midnight")
                   (org-clock-projects-export-test-tasks
                    "2026-09-org-clock-corner-cafe.csv")))))

(ert-deftest org-clock-projects-export-test-a-crossing-entry-is-exported-whole-to-both-periods ()
  "An entry crossing a boundary appears in both exports with its own timestamps.
The CSV is a transcript of the CLOCK line, so an invoice can be checked
against the Org file line by line.  Clipping the entry to the period it
was exported for would break that, and it is the downstream consumer's
job in any case."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-boundary-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-09")
      (org-clock-projects-export))
    (should (equal '(("2026-08-01 00:00" "2026-08-01 03:00")
                     ("2026-08-31 22:00" "2026-09-01 02:00"))
                   (org-clock-projects-export-test-spans
                    org-clock-projects-export-test-name)))
    (should (equal '(("2026-08-31 22:00" "2026-09-01 02:00"))
                   (org-clock-projects-export-test-spans
                    "2026-09-org-clock-corner-cafe.csv")))))

(ert-deftest org-clock-projects-export-test-an-empty-period-writes-a-header-only-file ()
  "A period with no work still produces a valid file.
Downstream tooling looks the file up by name, so an error or a missing
file would turn a quiet month into a broken build."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Another month" '(2026 6 4 9 0) '(2026 6 4 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal (list org-clock-projects-export-test-name)
                   (org-clock-projects-export-test-exports)))
    (should (equal org-clock-csv-header
                   (org-clock-projects-export-test-header
                    org-clock-projects-export-test-name)))
    (should-not (org-clock-projects-test-csv-rows
                 (org-clock-projects-export-test-path
                  org-clock-projects-export-test-name)))))


;;;; Which files are scanned

(ert-deftest org-clock-projects-export-test-rows-are-sorted-across-the-archive ()
  "Rows come out ascending by start whatever order the files hold them in.
The archive is scanned alongside the project's own file, so archived work
is billed, and the two files' entries interleave rather than being
appended one set after the other."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Late" '(2026 8 20 9 0) '(2026 8 20 10 0))
             (org-clock-projects-export-test-clocked
              "Early" '(2026 8 5 9 0) '(2026 8 5 10 0))))
        ("attic/corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Archived" '(2026 8 12 9 0) '(2026 8 12 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("Early" "Archived" "Late")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-a-project-without-an-archive-exports ()
  "A project whose archive does not exist is not a special case.
The archive location resolves to a file that was never created, and a
scan that handed that path on would fail on a project that has simply
never been archived."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0)))))
    (should-not (file-exists-p (expand-file-name "attic/corner-cafe.org"
                                                 org-directory)))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-the-archive-follows-the-archive-location ()
  "The archive is wherever `org-archive-location' says it is.
Here it is a sibling of the project file under a name of its own;
everywhere else in this file it is a file of the same name in a
subdirectory.  Neither is a layout a hardcoded path could have guessed,
and both are found."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))))
        ("old-corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Archived" '(2026 8 12 9 0) '(2026 8 12 10 0)))))
    (let ((org-archive-location "old-%s::"))
      (org-clock-projects-test-with-prompts
          (list org-clock-projects-export-test-project "2026-08")
        (org-clock-projects-export)))
    (should (equal '("House blend" "Archived")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-a-subtree-archive-property-is-scanned ()
  "A subtree that archives somewhere of its own is followed there.
Per-subtree archive locations are part of what the archive machinery
resolves, so a project that files one tree apart from the rest still gets
that tree's time billed."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             "* TODO Roasting\n:PROPERTIES:\n:ARCHIVE: extra/%s::\n:END:\n"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))))
        ("extra/corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Archived roasting" '(2026 8 12 9 0) '(2026 8 12 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("House blend" "Archived roasting")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-one-prefix-widens-the-scan ()
  "One prefix takes the scan to the agenda files, with the tag still selecting.
Work carrying the project's tag but filed outside the project's own file
is only reachable this way, and work carrying somebody else's tag stays
out of reach however wide the scan gets."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-scope-contents)
    (org-clock-projects-export-test-agenda
     "corner-cafe.org" "stray.org" "acme.org")
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export '(4)))
    (should (equal '("House blend" "Filed elsewhere")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))


;;;; What stops an export

(ert-deftest org-clock-projects-export-test-an-open-clock-in-the-period-stops-the-export ()
  "A clock still running inside the period stops the export.
The CSV writer silently discards open clocks, so the failure this
prevents is time missing from the invoice with nothing in the output to
say so.  No file is written, because a partial export is exactly the
thing that would be believed."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))
             (org-clock-projects-export-test-running
              "Still pouring" '(2026 8 15 9 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (should-error (org-clock-projects-export) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))))

(ert-deftest org-clock-projects-export-test-a-clock-running-since-before-the-period-stops-it ()
  "A clock started before the period and never closed stops the export too.
It has no end, so nothing places it inside or outside the period; a clock
that began before the period ended may still be accruing time that
belongs to it."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))
             (org-clock-projects-export-test-running
              "Running since July" '(2026 7 20 9 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (should-error (org-clock-projects-export) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))))

(ert-deftest org-clock-projects-export-test-a-clock-opened-after-the-period-is-ignored ()
  "A clock that began after the period ended cannot hold time from it.
Invoicing an old month while clocked on today's work is the ordinary
case, and it has to succeed."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))
             (org-clock-projects-export-test-running
              "Today's work" '(2026 9 3 9 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-an-overlap-within-a-project-stops-the-export ()
  "Two entries of one project covering the same minutes stop the export.
Parallel clocks make overlap between projects the point of the exercise;
overlap inside one project bills the same minutes twice."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-overlapping-contents)
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (should-error (org-clock-projects-export) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))))

(ert-deftest org-clock-projects-export-test-an-overlap-with-the-archive-stops-the-export ()
  "The overlap check spans the scanned files rather than each on its own.
Archiving half of a double-booked pair would otherwise hide it."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "First pour" '(2026 8 10 9 0) '(2026 8 10 11 0))))
        ("attic/corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Archived pour" '(2026 8 10 10 0) '(2026 8 10 12 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (should-error (org-clock-projects-export) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))))

(ert-deftest org-clock-projects-export-test-an-overlap-outside-the-period-is-not-raised ()
  "Only the entries being exported are checked against each other.
An old month's mistake is not this month's invoice's problem, and the
check command is where the whole file gets looked over."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "July first" '(2026 7 10 9 0) '(2026 7 10 11 0))
             (org-clock-projects-export-test-clocked
              "July second" '(2026 7 10 10 0) '(2026 7 10 12 0))
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-a-long-entry-does-not-stop-the-export ()
  "An implausibly long entry is reported by the check, not refused by the export.
Only the two findings that corrupt the invoice on their own — time that
never reaches the file, and time counted twice — are worth refusing to
write for."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "Forgot to clock out" '(2026 8 10 8 0) '(2026 8 10 21 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("Forgot to clock out")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-widening-keeps-the-checks ()
  "Widening the scan widens what the checks look at.
The open clock lives in the file only the wide scan reaches, so the
narrow export is clean and the wide one refuses.  A prefix that widened
the rows but not the checks would write an export that is missing time
without saying so."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0))))
        ("stray.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-running
              "Still pouring" '(2026 8 15 9 0)))))
    (org-clock-projects-export-test-agenda "corner-cafe.org" "stray.org")
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (should-error (org-clock-projects-export '(4)) :type 'user-error))
    (should-not (org-clock-projects-export-test-exports))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-two-prefixes-skip-the-checks-and-stay-narrow ()
  "Two prefixes write the export the checks refused, without widening the scan.
Reproducing an invoice already issued over data known to be bad is the
reason to have the override at all, and it is all-or-nothing rather than
per-check."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-overlapping-contents)
    (org-clock-projects-export-test-agenda "corner-cafe.org" "stray.org")
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (should-error (org-clock-projects-export) :type 'user-error))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export '(16)))
    (should (equal '("First pour" "Second pour")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))

(ert-deftest org-clock-projects-export-test-three-prefixes-skip-the-checks-and-widen ()
  "Three prefixes do both: the wide scan, and none of the checks.
It is the hardest gesture because it is the least safe one."
  (org-clock-projects-export-test-with-project
      (org-clock-projects-export-test-overlapping-contents)
    (org-clock-projects-export-test-agenda "corner-cafe.org" "stray.org")
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export '(64)))
    (should (equal '("First pour" "Second pour" "Filed elsewhere")
                   (org-clock-projects-export-test-tasks
                    org-clock-projects-export-test-name)))))


;;;; What the file is made of

(ert-deftest org-clock-projects-export-test-the-csv-format-stays-the-csv-packages ()
  "The header and each row come from the CSV package's own settings.
Anyone who has customised their columns keeps them, so the export stays
one shape of file rather than becoming a second one."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0)))))
    (let ((org-clock-csv-header "task")
          (org-clock-csv-row-fmt (lambda (plist) (plist-get plist :task))))
      (org-clock-projects-test-with-prompts
          (list org-clock-projects-export-test-project "2026-08")
        (org-clock-projects-export)))
    (should (equal "task" (org-clock-projects-export-test-header
                           org-clock-projects-export-test-name)))
    (should (equal '("House blend")
                   (org-clock-projects-test-csv-rows
                    (org-clock-projects-export-test-path
                     org-clock-projects-export-test-name))))))

(ert-deftest org-clock-projects-export-test-the-file-name-is-a-setting ()
  "The name comes from a function handed the record and the period.
Both are needed: the record carries the identifiers, and the period's
kind distinguishes a quarter from a range that happens to align with one,
which its bounds alone cannot."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "House blend" '(2026 8 5 9 0) '(2026 8 5 10 0)))))
    (let ((org-clock-projects-export-file-function
           (lambda (record period)
             (format "%s-%s.csv"
                     (plist-get record :tag)
                     (plist-get period :kind)))))
      (org-clock-projects-test-with-prompts
          (list org-clock-projects-export-test-project "2026-08")
        (org-clock-projects-export)))
    (should (equal '("cafe-month.csv")
                   (org-clock-projects-export-test-exports)))
    (should (equal '("House blend")
                   (org-clock-projects-export-test-tasks "cafe-month.csv")))))


;;;; The golden test

(ert-deftest org-clock-projects-export-test-durations-match-the-clocktable ()
  "The exported minutes equal what Org's own clocktable makes of the same range.
This holds only because no entry in this fixture crosses either end of
August: the exporter reports whole entries where the clocktable clips
them at the period's edges, so a straddling entry would make the two
disagree by exactly its clipped share, legitimately.  The fixture also has
no archive and carries the project's tag throughout, so both sides are
reading the same file and the same entries.  `org-extend-today-until' is
bound away for the comparison for the same reason the exporter binds it
away: a billing period is a calendar-day concept."
  (org-clock-projects-export-test-with-project
      `(("corner-cafe.org"
         . ,(org-clock-projects-export-test-tagged-file
             "cafe"
             (org-clock-projects-export-test-clocked
              "July work" '(2026 7 28 9 0) '(2026 7 28 11 30))
             (org-clock-projects-export-test-clocked
              "Morning bake" '(2026 8 3 9 0) '(2026 8 3 12 15))
             (org-clock-projects-export-test-clocked
              "Long service" '(2026 8 11 13 0) '(2026 8 11 17 45))
             (org-clock-projects-export-test-clocked
              "Stock take" '(2026 8 27 8 30) '(2026 8 27 9 0))
             (org-clock-projects-export-test-clocked
              "September work" '(2026 9 2 10 0) '(2026 9 2 11 0)))))
    (org-clock-projects-test-with-prompts
        (list org-clock-projects-export-test-project "2026-08")
      (org-clock-projects-export))
    (let ((clocktable
           (let ((org-extend-today-until 0)
                 (path (expand-file-name "corner-cafe.org" org-directory)))
             (with-current-buffer (find-file-noselect path)
               (org-with-wide-buffer
                (nth 1 (org-clock-get-table-data
                        path (list :tstart "2026-08-01"
                                   :tend "2026-09-01"
                                   :maxlevel 5))))))))
      (should (> clocktable 0))
      (should (= clocktable
                 (org-clock-projects-export-test-minutes
                  org-clock-projects-export-test-name))))))

(provide 'org-clock-projects-export-test)
;;; org-clock-projects-export-test.el ends here
