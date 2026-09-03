;;; org-clock-projects-clockmatrix-test.el --- Tests for the clockmatrix dynamic block -*- lexical-binding: t; -*-

;; Every assertion here is on the rendered table: fixture files on disk, a
;; dynamic block in a buffer, a refresh, and the Org table that comes back.  No
;; private helper is called and no internal data structure inspected.
;;
;; Nothing here turns `org-clock-projects-mode' on, because neither the block
;; nor the range keywords it accepts belong to the mode: they extend Org's own
;; clock reporting and install when the package loads.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'org-clock-projects-clockmatrix-harness)

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-clock-projects)

;;; Matrix shape and cell attribution

(ert-deftest org-clock-projects-clockmatrix-renders-periods-against-projects ()
  "Periods are rows, projects are columns, with totals on both axes.
A column is headed by its project's tag and not by the `:name' it
declares, so that a heading can be pasted straight into a clocktable
`:match' when a cell is being reconciled."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                     :stepskip0 t")
                   '(("Month" "alpha" "beta" "Total")
                     hline
                     ("2026-05" "3:15" "1:45" "5:00")
                     ("2026-06" "3:30" "" "3:30")
                     hline
                     ("Total" "6:45" "1:45" "8:30"))))))

(ert-deftest org-clock-projects-clockmatrix-opens-every-step-at-midnight ()
  "Every step opens its periods at midnight, not at `org-extend-today-until'.
That variable moves the start of a day for capture and for the agenda.  A
matrix honouring it for some steps while others kept midnight would
disagree with itself about which period a clock belongs to, and with the
CSV cut from the same range.  Day, week and year are asserted separately
because they are three separate branches: an hour before dawn on the
first day of a period is the only place any of them shows.

The sandbox holds the variable at zero, under which every branch is
equivalent to every other, so each case raises it locally."
  ;; Day: the clock belongs to the day it was worked, not the one before.
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :file "alpha.org"))
      '(("alpha.org" . "\
* TODO Before dawn :alpha:
:LOGBOOK:
CLOCK: [2026-05-02 Sat 01:00]--[2026-05-02 Sat 02:00] =>  1:00
:END:
"))
    (let ((org-extend-today-until 3))
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":tstart \"2026-05-01\" :tend \"2026-05-03\" :step day")
                     '(("Day" "alpha" "Total")
                       hline
                       ("2026-05-01" "" "")
                       ("2026-05-02" "1:00" "1:00")
                       hline
                       ("Total" "1:00" "1:00"))))))
  ;; Week: a Monday's small hours open that week, not close the one before.
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :file "alpha.org"))
      '(("alpha.org" . "\
* TODO Before dawn :alpha:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 01:00]--[2026-05-04 Mon 02:00] =>  1:00
:END:
"))
    (let ((org-extend-today-until 3))
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":tstart \"2026-04-27\" :tend \"2026-05-11\" :step week")
                     '(("Week" "alpha" "Total")
                       hline
                       ("2026-04-27" "" "")
                       ("2026-05-04" "1:00" "1:00")
                       hline
                       ("Total" "1:00" "1:00"))))))
  ;; Year: New Year's small hours belong to the year that just began.
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :file "alpha.org"))
      '(("alpha.org" . "\
* TODO Before dawn :alpha:
:LOGBOOK:
CLOCK: [2026-01-01 Thu 01:00]--[2026-01-01 Thu 02:00] =>  1:00
:END:
"))
    (let ((org-extend-today-until 3))
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":tstart \"2025-01-01\" :tend \"2027-01-01\" :step year")
                     '(("Year" "alpha" "Total")
                       hline
                       ("2025" "" "")
                       ("2026" "1:00" "1:00")
                       hline
                       ("Total" "1:00" "1:00")))))))

(ert-deftest org-clock-projects-clockmatrix-keeps-a-project-in-historical-ranges ()
  "A project with no recent time still appears in a range that covers it."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2025-01-01\" :tend \"2026-01-01\" :step month
                     :stepskip0 t")
                   '(("Month" "gamma" "Total")
                     hline
                     ("2025-11" "1:45" "1:45")
                     hline
                     ("Total" "1:45" "1:45"))))))

(ert-deftest org-clock-projects-clockmatrix-keeps-all-zero-rows-by-default ()
  "Empty periods appear in the table without a `:stepskip0' override."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-04-01\" :tend \"2026-07-01\" :step month")
                   '(("Month" "alpha" "beta" "Total")
                     hline
                     ("2026-04" "" "" "")
                     ("2026-05" "3:15" "1:45" "5:00")
                     ("2026-06" "3:30" "" "3:30")
                     hline
                     ("Total" "6:45" "1:45" "8:30"))))))

(ert-deftest org-clock-projects-clockmatrix-drops-all-zero-rows-with-stepskip0-t ()
  "`:stepskip0 t' removes periods where every project shows no time."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-04-01\" :tend \"2026-07-01\" :step month
                     :stepskip0 t")
                   '(("Month" "alpha" "beta" "Total")
                     hline
                     ("2026-05" "3:15" "1:45" "5:00")
                     ("2026-06" "3:30" "" "3:30")
                     hline
                     ("Total" "6:45" "1:45" "8:30"))))))

(ert-deftest org-clock-projects-clockmatrix-suppresses-totals ()
  "`:total nil' drops the Total column and the Total row."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                     :total nil :stepskip0 t")
                   '(("Month" "alpha" "beta")
                     hline
                     ("2026-05" "3:15" "1:45")
                     ("2026-06" "3:30" ""))))))

(ert-deftest org-clock-projects-clockmatrix-honours-tags-override ()
  "`:tags' replaces the columns derived from the registered projects."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                     :tags (\"beta\") :stepskip0 t")
                   '(("Month" "beta" "Total")
                     hline
                     ("2026-05" "1:45" "1:45")
                     hline
                     ("Total" "1:45" "1:45"))))))

;;; The tag gate

(ert-deftest org-clock-projects-clockmatrix-gates-each-column-by-its-own-tag ()
  "A cell sums the entries carrying its own tag and no others.
Two projects keep entries in `alpha.org', so a column selecting by the
files it scans rather than by its tag would bill beta's hour to alpha.
The narrow scope reads each project's own file and `agenda-with-archives'
reads both; in either the tag decides, and the Total is the sum of the
columns printed beside it."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :name "Alpha")
        (:tag "beta" :name "Beta"))
      '(("alpha.org" . "\
* TODO Alpha task :alpha:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 11:00] =>  2:00
:END:
* TODO Beta task filed with alpha :beta:
:LOGBOOK:
CLOCK: [2026-05-05 Tue 09:00]--[2026-05-05 Tue 10:00] =>  1:00
:END:
")
        ("beta.org" . "\
* TODO Beta task :beta:
:LOGBOOK:
CLOCK: [2026-05-06 Wed 09:00]--[2026-05-06 Wed 09:30] =>  0:30
:END:
"))
    ;; Alpha's column opens alpha.org alone, so beta's hour kept there is
    ;; absent from the report rather than misattributed: the file set bounds
    ;; the scan and the tag decides the cell.
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month")
                   '(("Month" "alpha" "beta" "Total")
                     hline
                     ("2026-05" "2:00" "0:30" "2:30")
                     hline
                     ("Total" "2:00" "0:30" "2:30"))))
    (setq org-agenda-files
          (mapcar (lambda (f) (expand-file-name f org-directory))
                  '("alpha.org" "beta.org")))
    ;; Both columns scan both files here, so only the match keeps them apart.
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month
                     :scope agenda-with-archives")
                   '(("Month" "alpha" "beta" "Total")
                     hline
                     ("2026-05" "2:00" "1:30" "3:30")
                     hline
                     ("Total" "2:00" "1:30" "3:30"))))))

(ert-deftest org-clock-projects-clockmatrix-gives-a-doubly-tagged-entry-to-the-first-project ()
  "An entry carrying two projects' tags counts for the earlier one alone.
Registry order decides, so the same file read under the opposite order
reports that hour against the other column.  Under either order the
columns partition the file's clocked time, which is what keeps the Total
equal both to the sum of the columns and to the time worked."
  (let ((files '(("shared.org" . "\
* TODO Work for both :alpha:beta:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 10:00] =>  1:00
:END:
* TODO Work for beta :beta:
:LOGBOOK:
CLOCK: [2026-05-05 Tue 09:00]--[2026-05-05 Tue 09:30] =>  0:30
:END:
"))))
    (org-clock-projects-clockmatrix-test-with-data
        '((:tag "alpha" :name "Alpha" :file "shared.org")
          (:tag "beta" :name "Beta" :file "shared.org"))
        files
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month")
                     '(("Month" "alpha" "beta" "Total")
                       hline
                       ("2026-05" "1:00" "0:30" "1:30")
                       hline
                       ("Total" "1:00" "0:30" "1:30")))))
    ;; Beta declared first takes the doubly tagged hour, which leaves alpha
    ;; with nothing anywhere in the range and so with no column at all.
    (org-clock-projects-clockmatrix-test-with-data
        '((:tag "beta" :name "Beta" :file "shared.org")
          (:tag "alpha" :name "Alpha" :file "shared.org"))
        files
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month")
                     '(("Month" "beta" "Total")
                       hline
                       ("2026-05" "1:30" "1:30")
                       hline
                       ("Total" "1:30" "1:30")))))))

(ert-deftest org-clock-projects-clockmatrix-skips-a-registry-entry-without-a-tag ()
  "An entry declaring no tag is skipped with a warning and heads no column.
A column is a tag and the files to look for it in.  An entry without a
tag could only be given a match selecting everything, and the five hours
of unrelated work in the scan set would be reported as somebody's
billable time.  The registry drops such an entry before any column is
built, and says once which entry it dropped and what a project looks
like."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :name "Alpha")
        (:name "Nameless" :file "nameless.org"))
      '(("alpha.org" . "\
* TODO Alpha task :alpha:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 11:00] =>  2:00
:END:
")
        ("nameless.org" . "\
* TODO Work belonging to no project
:LOGBOOK:
CLOCK: [2026-05-05 Tue 09:00]--[2026-05-05 Tue 14:00] =>  5:00
:END:
"))
    (setq org-agenda-files
          (mapcar (lambda (f) (expand-file-name f org-directory))
                  '("alpha.org" "nameless.org")))
    (let ((warnings (org-clock-projects-clockmatrix-test-warnings
                     ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month
                      :scope agenda-with-archives")))
      (should (= 1 (length warnings)))
      (let ((warning (car warnings)))
        (should (string-match-p
                 (regexp-quote "(:name \"Nameless\" :file \"nameless.org\")")
                 warning))
        (should (string-match-p ":tag" warning))))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month
                     :scope agenda-with-archives")
                   '(("Month" "alpha" "Total")
                     hline
                     ("2026-05" "2:00" "2:00")
                     hline
                     ("Total" "2:00" "2:00"))))))

(ert-deftest org-clock-projects-clockmatrix-counts-a-repeated-tags-entry-once ()
  "A tag written twice in `:tags' heads one column and is totalled once.
Two columns selecting on the same match report the same minutes, and the
Total row would then count that project's time as often as its tag was
written."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                     :tags (\"alpha\" \"alpha\") :stepskip0 t")
                   '(("Month" "alpha" "Total")
                     hline
                     ("2026-05" "3:15" "3:15")
                     ("2026-06" "3:30" "3:30")
                     hline
                     ("Total" "6:45" "6:45"))))))

(ert-deftest org-clock-projects-clockmatrix-selects-an-unregistered-tag-on-that-tag-alone ()
  "A `:tags' column outside the registry refuses no other project's tag.
Refusing the tags declared ahead of it is what keeps registered columns
disjoint, and a tag the registry does not hold stands nowhere in that
order.  Such a column reports every entry carrying its tag, including
one a registered project also owns, so its Total is the sum of the
columns printed rather than the time worked once."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :name "Alpha" :file "shared.org"))
      '(("shared.org" . "\
* TODO Work for both :alpha:ghost:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 10:00] =>  1:00
:END:
"))
    (setq org-agenda-files (list (expand-file-name "shared.org" org-directory)))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month
                     :tags (\"alpha\" \"ghost\") :scope agenda-with-archives")
                   '(("Month" "alpha" "ghost" "Total")
                     hline
                     ("2026-05" "1:00" "1:00" "2:00")
                     hline
                     ("Total" "1:00" "1:00" "2:00"))))))

;;; Period stepping

(ert-deftest org-clock-projects-clockmatrix-labels-rows-per-step ()
  "Each `:step' names its label column and formats its row labels."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (pcase-dolist (`(,step ,tstart ,tend ,header ,label)
                   '(("day" "2026-05-04" "2026-05-05" "Day" "2026-05-04")
                     ("week" "2026-05-04" "2026-05-11" "Week" "2026-05-04")
                     ("semimonth" "2026-05-01" "2026-05-16" "Semimonth" "2026-05-01")
                     ("month" "2026-05-01" "2026-06-01" "Month" "2026-05")
                     ("quarter" "2026-04-01" "2026-07-01" "Quarter" "2026-Q2")
                     ("year" "2026-01-01" "2027-01-01" "Year" "2026")))
      (ert-info ((format "step=%s" step))
        (let ((table (org-clock-projects-clockmatrix-test-table
                      (format ":tstart \"%s\" :tend \"%s\" :step %s :tags (\"alpha\")"
                              tstart tend step))))
          (should (equal (car table) (list header "alpha" "Total")))
          (should (equal (car (nth 2 table)) label)))))))

(ert-deftest org-clock-projects-clockmatrix-steps-weeks-from-the-range-start ()
  "Weekly rows start on `:wstart', with a partial first week."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-05-15\" :step week
                     :stepskip0 t")
                   '(("Week" "alpha" "beta" "Total")
                     hline
                     ("2026-05-04" "2:00" "1:15" "3:15")
                     ("2026-05-11" "1:15" "" "1:15")
                     hline
                     ("Total" "3:15" "1:15" "4:30"))))))

(ert-deftest org-clock-projects-clockmatrix-splits-clocks-across-period-boundaries ()
  "A clock spanning two periods is split between them and sums to its length."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "delta" :name "Delta"))
      '(("delta.org" . "\
* TODO Delta task :delta:
:LOGBOOK:
CLOCK: [2026-05-31 Sun 22:15]--[2026-06-01 Mon 01:00] =>  2:45
:END:
"))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-07-01\" :step month")
                   '(("Month" "delta" "Total")
                     hline
                     ("2026-05" "1:45" "1:45")
                     ("2026-06" "1:00" "1:00")
                     hline
                     ("Total" "2:45" "2:45"))))))

(ert-deftest org-clock-projects-clockmatrix-steps-semimonths-twice-in-a-month ()
  "Semimonth periods split a month at the 16th."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "delta" :name "Delta"))
      '(("delta.org" . "\
* TODO Delta task :delta:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 10:00] =>  1:00
CLOCK: [2026-05-20 Wed 09:00]--[2026-05-20 Wed 11:00] =>  2:00
CLOCK: [2026-06-02 Tue 09:00]--[2026-06-02 Tue 09:30] =>  0:30
:END:
"))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-07-01\" :step semimonth
                     :stepskip0 t")
                   '(("Semimonth" "delta" "Total")
                     hline
                     ("2026-05-01" "1:00" "1:00")
                     ("2026-05-16" "2:00" "2:00")
                     ("2026-06-01" "0:30" "0:30")
                     hline
                     ("Total" "3:30" "3:30"))))))

(ert-deftest org-clock-projects-clockmatrix-caps-durations-at-hours ()
  "Durations past 24 hours read as hours and minutes, not days.
The ambient `org-duration-format' renders a day unit; the matrix drops it
so that monthly totals stay readable."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "delta" :name "Delta"))
      '(("delta.org" . "\
* TODO Delta task :delta:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 00:00]--[2026-05-05 Tue 01:30] => 25:30
:END:
"))
    (let ((org-duration-format '(("d") (special . h:mm))))
      ;; Emacs' default format would render this as "1d 1:30".
      (should (equal (org-duration-from-minutes 1530) "1d 1:30"))
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":tstart \"2026-05-01\" :tend \"2026-06-01\" :step month")
                     '(("Month" "delta" "Total")
                       hline
                       ("2026-05" "25:30" "25:30")
                       hline
                       ("Total" "25:30" "25:30")))))))

(ert-deftest org-clock-projects-clockmatrix-refuses-a-step-that-cannot-advance ()
  "An out-of-range `:wstart' is reported rather than looping forever."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    ;; `:wstart' is a `decode-time' day number, so Sunday is 0, not 7.  With 7
    ;; the week step lands back on its own start date.
    (let ((error-message
           (cadr (should-error
                  (org-clock-projects-clockmatrix-test-table
                   ":tstart \"2026-05-03\" :tend \"2026-05-18\" :step week :wstart 7")
                  :type 'user-error))))
      (should (string-match-p ":wstart" error-message)))))

(ert-deftest org-clock-projects-clockmatrix-honours-a-non-default-wstart ()
  "Weeks can start on a day other than Monday."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "delta" :name "Delta"))
      '(("delta.org" . "\
* TODO Delta task :delta:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 10:00] =>  1:00
CLOCK: [2026-05-10 Sun 09:00]--[2026-05-10 Sun 11:00] =>  2:00
:END:
"))
    ;; Weeks starting on Sunday put 2026-05-10 in its own row; starting on
    ;; Monday it falls in the week of 2026-05-04 together with the other clock.
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-03\" :tend \"2026-05-17\" :step week :wstart 0
                     :stepskip0 t")
                   '(("Week" "delta" "Total")
                     hline
                     ("2026-05-03" "1:00" "1:00")
                     ("2026-05-10" "2:00" "2:00")
                     hline
                     ("Total" "3:00" "3:00"))))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-03\" :tend \"2026-05-17\" :step week :wstart 1
                     :stepskip0 t")
                   '(("Week" "delta" "Total")
                     hline
                     ("2026-05-04" "3:00" "3:00")
                     hline
                     ("Total" "3:00" "3:00"))))))

(ert-deftest org-clock-projects-clockmatrix-clips-the-last-period-to-the-range-end ()
  "The final period stops at the range end rather than running past it."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "delta" :name "Delta"))
      '(("delta.org" . "\
* TODO Delta task :delta:
:LOGBOOK:
CLOCK: [2026-05-12 Tue 09:00]--[2026-05-12 Tue 10:00] =>  1:00
CLOCK: [2026-05-16 Sat 09:00]--[2026-05-16 Sat 12:00] =>  3:00
:END:
"))
    ;; The week starting 2026-05-11 runs to 2026-05-18, but the range ends on
    ;; 2026-05-15, so the clock on 2026-05-16 is outside the report entirely.
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-05-01\" :tend \"2026-05-15\" :step week
                     :stepskip0 t")
                   '(("Week" "delta" "Total")
                     hline
                     ("2026-05-11" "1:00" "1:00")
                     hline
                     ("Total" "1:00" "1:00"))))))

(ert-deftest org-clock-projects-clockmatrix-resolves-block-keyword-ranges ()
  "`:block' selects the range with clocktable's own vocabulary."
  (let ((today (format-time-string "%Y-%m-%d")))
    (org-clock-projects-clockmatrix-test-with-data
        '((:tag "alpha" :name "Alpha"))
        (list (cons "alpha.org"
                    (format "\
* TODO Alpha task :alpha:
:LOGBOOK:
CLOCK: [%s 09:00]--[%s 10:30] =>  1:30
:END:
" today today)))
      (should (equal (org-clock-projects-clockmatrix-test-table
                      ":block today :step day")
                     `(("Day" "alpha" "Total")
                       hline
                       (,today "1:30" "1:30")
                       hline
                       ("Total" "1:30" "1:30")))))))

;;; Scope and file resolution

(ert-deftest org-clock-projects-clockmatrix-agenda-with-archives-scope-agrees ()
  "The wider audit scope reports the same numbers as the project files."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (setq org-agenda-files
          (mapcar (lambda (f) (expand-file-name f org-directory))
                  '("alpha.org" "beta.org" "gamma.org")))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                     :scope agenda-with-archives")
                   (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month")))))

(ert-deftest org-clock-projects-clockmatrix-warns-about-a-missing-project-file ()
  "A project whose main file is absent warns instead of reporting zero."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :name "Alpha")
        (:tag "ghost" :name "Ghost"))
      '(("alpha.org" . "\
* TODO Alpha task :alpha:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 11:00] =>  2:00
:END:
"))
    (let ((warnings (org-clock-projects-clockmatrix-test-warnings
                     ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month")))
      (should (= 1 (length warnings)))
      (let ((warning (car warnings)))
        (should (string-match-p "ghost" warning))
        ;; The warning names the file the report would have read, which is a
        ;; truename: a sandbox reached through a symlink must not make the
        ;; assertion a comparison of two different spellings of one path.
        (should (string-match-p
                 (regexp-quote
                  (file-truename (expand-file-name "ghost.org" org-directory)))
                 warning))
        (should (string-match-p ":file" warning))
        (should (string-match-p "agenda-with-archives" warning))))))

(ert-deftest org-clock-projects-clockmatrix-does-not-warn-about-a-missing-archive ()
  "A project without an archive file is not a misconfiguration."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "alpha" :name "Alpha"))
      '(("alpha.org" . "\
* TODO Alpha task :alpha:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 11:00] =>  2:00
:END:
"))
    (should-not (org-clock-projects-clockmatrix-test-warnings
                 ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month"))))

(ert-deftest org-clock-projects-clockmatrix-reads-a-project-from-a-renamed-file ()
  "A project registered with `:file' is read from that file."
  (org-clock-projects-clockmatrix-test-with-data
      '((:tag "cafe" :name "Corner Cafe" :file "corner-cafe.org"))
      '(("corner-cafe.org" . "\
* TODO Cafe task :cafe:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 10:00] =>  1:00
:END:
"))
    (should-not (org-clock-projects-clockmatrix-test-warnings
                 ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                  :stepskip0 t"))
    (should (equal (org-clock-projects-clockmatrix-test-table
                    ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step month
                     :stepskip0 t")
                   '(("Month" "cafe" "Total")
                     hline
                     ("2026-05" "1:00" "1:00")
                     hline
                     ("Total" "1:00" "1:00"))))))

;;; Block plumbing

(ert-deftest org-clock-projects-clockmatrix-is-a-registered-dynamic-block ()
  "The block is offered by Org's dynamic-block insertion menu."
  (should (eq (cdr (assoc "clockmatrix" org-dynamic-block-alist))
              #'org-clock-projects-clockmatrix)))

(ert-deftest org-clock-projects-clockmatrix-requires-a-range ()
  "Without `:block' or `:tstart'/`:tend' the block says what is missing."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (let ((error-message
           (cadr (should-error
                  (org-clock-projects-clockmatrix-test-table ":step month")
                  :type 'user-error))))
      (should (string-match-p ":block" error-message))
      (should (string-match-p ":tstart" error-message)))))

(ert-deftest org-clock-projects-clockmatrix-rejects-an-unknown-step ()
  "An unusable `:step' is reported rather than silently ignored."
  (org-clock-projects-clockmatrix-test-with-data
      org-clock-projects-clockmatrix-test-projects
      org-clock-projects-clockmatrix-test-files
    (should-error (org-clock-projects-clockmatrix-test-table
                   ":tstart \"2026-01-01\" :tend \"2027-01-01\" :step fortnight")
                  :type 'user-error)))

(provide 'org-clock-projects-clockmatrix-test)

;;; org-clock-projects-clockmatrix-test.el ends here
