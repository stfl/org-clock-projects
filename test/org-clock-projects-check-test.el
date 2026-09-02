;;; org-clock-projects-check-test.el --- Tests for the check command -*- lexical-binding: t; -*-

;; The observable here is the text of the report buffer: which findings it
;; names and what each of them says.  Nothing beneath the command is called
;; and no finding is read as data, so a rewrite of the findings core that
;; leaves the report reading the same passes, and one that changes what the
;; reader is told fails.
;;
;; A finding is picked out of the buffer by its button rather than by its
;; line number, because a button is what the report promises one of per
;; finding; the prose around them is the report talking about itself.
;;
;; Story 28's jump is the one thing that text cannot show.  It is asserted by
;; taking the finding's own action and looking at where it lands.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'org-clock-projects-test-harness)

(require 'ert)
(require 'seq)
(require 'button)

;;;; Reading the report back

(defconst org-clock-projects-check-test-buffer "*org-clock-projects-check*"
  "The buffer the check renders into.")

(defconst org-clock-projects-check-test-kind-names
  '("open-clock" "overlap" "long-entry" "missing-tag")
  "The four problems a finding can report.
Which of them a reader is looking at is the one thing every finding has
to say, so it is what the assertions here read a finding by.")

(defun org-clock-projects-check-test-report ()
  "Return the text of the check report buffer, or nil when there is none."
  (let ((buffer (get-buffer org-clock-projects-check-test-buffer)))
    (and buffer
         (with-current-buffer buffer
           (buffer-substring-no-properties (point-min) (point-max))))))

(defun org-clock-projects-check-test-findings ()
  "Return one (LINE . BUTTON) cons per finding, in the order rendered.
A finding is what the report made jumpable, so its button is what picks
it out of the buffer.  Reading the findings off the buttons rather than
off the line numbers leaves the report free to say whatever it likes
around them: prose carries no button and is not a finding."
  (let ((buffer (get-buffer org-clock-projects-check-test-buffer)))
    (and buffer
         (with-current-buffer buffer
           (save-excursion
             (let ((position (point-min))
                   (findings nil)
                   button)
               (while (setq button (next-button position t))
                 (goto-char button)
                 (push (cons (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))
                             button)
                       findings)
                 (setq position (button-end button)))
               (nreverse findings)))))))

(defun org-clock-projects-check-test-kinds ()
  "Return the kind each finding names, in the order rendered.
A finding naming no kind, or more than one, is itself a failure: the kind
is what tells the reader which of the four problems the line is about."
  (mapcar (lambda (finding)
            (let ((named (seq-filter
                          (lambda (kind)
                            (string-match-p (regexp-quote kind) (car finding)))
                          org-clock-projects-check-test-kind-names)))
              (unless (= 1 (length named))
                (error "Finding names %d kinds: %s" (length named) (car finding)))
              (car named)))
          (org-clock-projects-check-test-findings)))

(defun org-clock-projects-check-test-kind-set ()
  "Return the distinct kinds the report names, sorted.
Findings of different kinds are rendered in no promised order, so the set
is what the report actually guarantees."
  (sort (delete-dups (org-clock-projects-check-test-kinds)) #'string<))

(defun org-clock-projects-check-test-rows ()
  "Return the buffer line each finding is rendered on, in the order rendered."
  (let ((buffer (get-buffer org-clock-projects-check-test-buffer)))
    (and buffer
         (with-current-buffer buffer
           (save-excursion
             (mapcar (lambda (finding)
                       (goto-char (cdr finding))
                       (line-number-at-pos))
                     (org-clock-projects-check-test-findings)))))))

(defun org-clock-projects-check-test-finding (kind)
  "Return the one finding of KIND, failing unless the report holds exactly one."
  (let ((matches (seq-filter (lambda (finding)
                               (string-match-p (regexp-quote kind) (car finding)))
                             (org-clock-projects-check-test-findings))))
    (unless (= 1 (length matches))
      (error "Expected one %s finding, found %d" kind (length matches)))
    (car matches)))

(defun org-clock-projects-check-test-says (kind)
  "Return the report line of the one finding of KIND."
  (car (org-clock-projects-check-test-finding kind)))

(defun org-clock-projects-check-test-jump (kind)
  "Take the action of the one finding of KIND."
  (button-activate (cdr (org-clock-projects-check-test-finding kind))))

(defun org-clock-projects-check-test-line-at-point ()
  "Return the line point rests on in the current buffer."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

;;;; Fixtures

(defconst org-clock-projects-check-test-cafe
  '(:tag "cafe" :name "Corner Café" :file "corner-cafe.org")
  "A project whose tag, display name and file base are three different strings.
Every test here answers the project prompt with the display name, has its
files found by the file name and has their contents judged by the tag, so
no two of the three can be silently interchanged.")

(defconst org-clock-projects-check-test-birds
  '(:tag "birds" :name "Bird Atlas" :file "bird-atlas.org")
  "A second project, for the overlap that runs between two of them.")

(defconst org-clock-projects-check-test-archive "archive/corner-cafe.org_archive"
  "Where `org-archive-location' puts the archive of the first project's file.
Deliberately not a sibling of the file it belongs to, so a scan that
derives it from the configuration is told apart from one that assumes a
layout.")

(defun org-clock-projects-check-test-now ()
  "Return the instant every test here runs at.
Mid-September, so `lastmonth' names a complete August and `thismonth' a
partial September, with a period boundary between the two.

The year is a settled one in the past rather than the running one, so a
relative key resolved against the wall clock instead of the frozen one
lands in a different August and finds nothing.  Sharing a month with the
day the suite happens to run on would let that pass by coincidence for a
few weeks and then start failing."
  (org-clock-projects-test-time-at 2024 9 15 12))

(defun org-clock-projects-check-test-tagged (tag &rest entries)
  "Return an Org file carrying TAG at file level and holding ENTRIES.
A project's tag is declared once at the top of its file and inherited by
everything beneath it, which is the arrangement the tag match relies on.
A nil TAG returns a file that carries no tag at all."
  (concat (if tag (format "#+FILETAGS: :%s:\n" tag) "")
          "\n"
          (apply #'concat entries)))

(defun org-clock-projects-check-test-hours (month day from hours)
  "Return a (START . END) cons for HOURS of work from FROM on MONTH DAY.
The year is the one `org-clock-projects-check-test-now' runs in.  Stating
a clock as a start and a length keeps the duration threshold legible at
the fixture rather than as arithmetic at the assertion."
  (cons (org-clock-projects-test-time-at 2024 month day from)
        (org-clock-projects-test-time-at 2024 month day (+ from hours))))

(defmacro org-clock-projects-check-test-with-projects (records contents &rest body)
  "Run BODY with RECORDS registered as the projects and CONTENTS on disk.
RECORDS is a list of project plists and CONTENTS an alist of (NAME . TEXT)
written under the sandbox's `org-directory'.  The clock is frozen at
`org-clock-projects-check-test-now', archives resolve inside the sandbox,
and the agenda is the sandbox directory alone, so a widened scan sees the
fixture and nothing of the machine it runs on."
  (declare (indent 2) (debug t))
  `(let ((registry ,records))
     (org-clock-projects-test-with-data
         (mapcar (lambda (record) (plist-get record :file)) registry)
         ,contents
       (setq org-clock-projects-projects-function (lambda () registry))
       (let ((org-archive-location "archive/%s_archive::")
             (org-agenda-files (list org-directory))
             (org-use-tag-inheritance t))
         (unwind-protect
             (org-clock-projects-test-at (org-clock-projects-check-test-now)
               ,@body)
           (when (get-buffer org-clock-projects-check-test-buffer)
             (kill-buffer org-clock-projects-check-test-buffer)))))))

;;;; A period with nothing wrong in it

(ert-deftest org-clock-projects-check-test-a-clean-period-says-so ()
  "An empty buffer is indistinguishable from a command that did nothing."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings))
      (should (org-string-nw-p (org-clock-projects-check-test-report))))))

;;;; The prompts

(ert-deftest org-clock-projects-check-test-the-period-prompt-names-the-typed-forms ()
  "Nothing in a completion prompt announces that free text is accepted."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      ;; Two prompts and no more: the project, then the period.  A typed
      ;; month resolves without a date pick.
      (should (equal (length (org-clock-projects-test-prompts)) 2))
      (should (equal (nth 1 (org-clock-projects-test-prompts))
                     "Period (or YYYY-MM, YYYY-QN, YYYY, YYYY-Www): "))
      (should (equal (org-clock-projects-test-candidates)
                     '("lastmonth" "thismonth" "lastq" "thisq" "lastweek"
                       "thisweek" "lastyear" "thisyear" "range"))))))

(ert-deftest org-clock-projects-check-test-a-relative-key-selects-its-period ()
  "The completion prompt hands back a string, and only a symbol resolves.
Which month the check looked at is visible in whether it found the long
entry that sits in one of them."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "August"
              (org-clock-projects-check-test-hours 8 5 8 11))
             (org-clock-projects-test-closed-entry
              "September"
              (org-clock-projects-check-test-hours 9 5 8 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "lastmonth")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kinds) '("long-entry"))))
    (org-clock-projects-test-with-prompts '("Corner Café" "thismonth")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

;;;; Open clocks

(ert-deftest org-clock-projects-check-test-an-open-clock-in-the-period-is-reported ()
  "An open clock never reaches the CSV, so nothing downstream can report it."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-clock-at
              "Still running"
              (org-clock-projects-test-time-at 2024 8 10 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kinds) '("open-clock")))
      (should (string-match-p
               "corner-cafe" (org-clock-projects-check-test-says "open-clock"))))))

(ert-deftest org-clock-projects-check-test-a-clock-started-before-the-period-is-reported ()
  "A clock left running since July is time August cannot account for either."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-clock-at
              "Running since July"
              (org-clock-projects-test-time-at 2024 7 20 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kinds) '("open-clock"))))))

(ert-deftest org-clock-projects-check-test-a-clock-started-after-the-period-is-not-reported ()
  "September's running clock is September's problem."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-clock-at
              "Started in September"
              (org-clock-projects-test-time-at 2024 9 10 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

;;;; Overlaps

(ert-deftest org-clock-projects-check-test-an-overlap-within-a-project-is-reported ()
  "Two clocks running at once inside one project bill the same minutes twice."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Morning"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-closed-entry
              "Also morning"
              (org-clock-projects-check-test-hours 8 3 11 2)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kind-set) '("overlap"))))))

(ert-deftest org-clock-projects-check-test-adjacent-entries-are-not-an-overlap ()
  "Clocking out and straight back in is the ordinary shape of a working day."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Morning"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-closed-entry
              "Afternoon"
              (org-clock-projects-check-test-hours 8 3 12 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

(ert-deftest org-clock-projects-check-test-an-overlap-between-projects-is-not-reported ()
  "Parallel per-project clocks are the point of the package, not a defect.
The widened scan puts both projects' entries in front of the check at
once, which is where a check judging by time alone would report them."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe
            org-clock-projects-check-test-birds)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        ("bird-atlas.org"
         . ,(org-clock-projects-check-test-tagged
             "birds"
             (org-clock-projects-test-closed-entry
              "Counting"
              (org-clock-projects-check-test-hours 8 3 10 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check '(4))
      (should-not (member "overlap" (org-clock-projects-check-test-kinds))))))

;;;; Long entries

(ert-deftest org-clock-projects-check-test-an-entry-under-the-threshold-is-not-reported ()
  "Nine hours is a long day, not a forgotten clock-out."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "A long day"
              (org-clock-projects-check-test-hours 8 5 8 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

(ert-deftest org-clock-projects-check-test-an-entry-over-the-threshold-is-reported ()
  "The duration guard catches a mis-entry wherever in the period it falls."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Forgot to clock out"
              (org-clock-projects-check-test-hours 8 5 8 11)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kinds) '("long-entry")))
      (should (string-match-p
               "corner-cafe" (org-clock-projects-check-test-says "long-entry"))))))

(ert-deftest org-clock-projects-check-test-the-duration-threshold-is-a-setting ()
  "What counts as implausibly long is the user's, not the package's."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "A long day"
              (org-clock-projects-check-test-hours 8 5 8 9)))))
    (let ((org-clock-projects-duration-threshold 4))
      (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
        (org-clock-projects-check)
        (should (equal (org-clock-projects-check-test-kinds) '("long-entry")))))))

;;;; Tags

(ert-deftest org-clock-projects-check-test-an-untagged-file-is-reported ()
  "The tag match is what finds the work; a file without it exports empty."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        (,org-clock-projects-check-test-archive
         . ,(org-clock-projects-check-test-tagged
             nil
             (org-clock-projects-test-closed-entry
              "Archived design"
              (org-clock-projects-check-test-hours 8 4 9 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kinds) '("missing-tag")))
      (should (string-match-p
               "corner-cafe.org_archive"
               (org-clock-projects-check-test-says "missing-tag"))))))

(ert-deftest org-clock-projects-check-test-a-headline-tag-does-not-tag-the-file ()
  "A tag worn by one headline leaves the entries beside it unreachable.
The export selects on inherited tags, so an archive whose tag sits on a
headline rather than at the top of the file yields that subtree and
silently drops the rest.  Calling such a file tagged would let the check
pass on exactly the arrangement it exists to catch."
  (let ((clock (org-clock-projects-check-test-hours 8 4 9 3)))
    (org-clock-projects-check-test-with-projects
        (list org-clock-projects-check-test-cafe)
        `(("corner-cafe.org"
           . ,(org-clock-projects-check-test-tagged
               "cafe"
               (org-clock-projects-test-closed-entry
                "Design"
                (org-clock-projects-check-test-hours 8 3 9 3))))
          (,org-clock-projects-check-test-archive
           . ,(concat "* Archived design :cafe:\n:LOGBOOK:\n"
                      (org-clock-projects-test-clock-line (car clock) (cdr clock))
                      ":END:\n* Unreachable\n")))
      (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
        (org-clock-projects-check)
        (should (equal (org-clock-projects-check-test-kinds) '("missing-tag")))
        (should (string-match-p
                 "corner-cafe.org_archive"
                 (org-clock-projects-check-test-says "missing-tag")))))))

(ert-deftest org-clock-projects-check-test-a-tagged-archive-is-clean ()
  "The archive is scanned, so a tagged one has to come back with nothing.
Paired with the untagged archive above, this is what shows the archive
was read rather than skipped."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        (,org-clock-projects-check-test-archive
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Archived design"
              (org-clock-projects-check-test-hours 8 4 9 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

(ert-deftest org-clock-projects-check-test-a-project-without-an-archive-is-not-a-special-case ()
  "A new project has no archive file, and that is not something to report."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

;;;; Everything at once

(ert-deftest org-clock-projects-check-test-all-four-kinds-render-together ()
  "One command reporting everything questionable is the point of the check.
The four are found in one pass over one project, so a report that can
only render one kind at a time fails here rather than in use."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Morning"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-closed-entry
              "Also morning"
              (org-clock-projects-check-test-hours 8 3 11 2))
             (org-clock-projects-test-closed-entry
              "Forgot to clock out"
              (org-clock-projects-check-test-hours 8 5 8 11))
             (org-clock-projects-test-clock-at
              "Still running"
              (org-clock-projects-test-time-at 2024 8 10 9))))
        (,org-clock-projects-check-test-archive
         . ,(org-clock-projects-check-test-tagged nil "* DONE Archived design\n")))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kind-set)
                     '("long-entry" "missing-tag" "open-clock" "overlap")))
      ;; One line per finding: four problems crowded onto one line would
      ;; leave three of them with nowhere to hang their own jump.
      (let ((rows (org-clock-projects-check-test-rows)))
        (should (equal rows (delete-dups (copy-sequence rows))))))))

;;;; Scope

(ert-deftest org-clock-projects-check-test-the-narrow-scan-stays-in-the-project ()
  "The default scope is the project's own files, which is what keeps the
check and the matrix agreeing about what they looked at."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        ("elsewhere.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Filed in the wrong place"
              (org-clock-projects-check-test-hours 8 5 8 11)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should-not (org-clock-projects-check-test-findings)))))

(ert-deftest org-clock-projects-check-test-the-prefix-widens-the-scan ()
  "The audit path finds work carrying the tag but filed outside the project."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        ("elsewhere.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Filed in the wrong place"
              (org-clock-projects-check-test-hours 8 5 8 11)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check '(4))
      (should (equal (org-clock-projects-check-test-kinds) '("long-entry")))
      (should (string-match-p
               "elsewhere" (org-clock-projects-check-test-says "long-entry"))))))

(ert-deftest org-clock-projects-check-test-the-prefix-does-not-widen-the-tag-check ()
  "The tag question is always asked of the project's own files.
Widening the scan is how work tagged for this project but filed elsewhere
is found, so the agenda files it reaches are not expected to carry the
tag and reporting them would bury the one file that should."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        (,org-clock-projects-check-test-archive
         . ,(org-clock-projects-check-test-tagged nil "* DONE Archived design\n"))
        ("elsewhere.org"
         . ,(org-clock-projects-check-test-tagged
             nil
             (org-clock-projects-test-closed-entry
              "Somebody else's work"
              (org-clock-projects-check-test-hours 8 5 9 3)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kinds) '("missing-tag")))
      (should (string-match-p "corner-cafe.org_archive"
                              (org-clock-projects-check-test-says "missing-tag"))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check '(4))
      ;; The widened scan reads elsewhere.org and still says nothing about it.
      (should (equal (org-clock-projects-check-test-kinds) '("missing-tag")))
      (should (string-match-p "corner-cafe.org_archive"
                              (org-clock-projects-check-test-says "missing-tag")))
      (should-not (string-match-p
                   "elsewhere" (org-clock-projects-check-test-report))))))

;;;; Jumping to a finding

(ert-deftest org-clock-projects-check-test-a-finding-jumps-to-its-clock-line ()
  "Fixing a finding should not mean searching the file for it again.
The landing place is the clock line the finding names, not the headline
above it: an entry can hold several clocks and only one of them is at
fault."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-clock-at
              "Still running"
              (org-clock-projects-test-time-at 2024 8 10 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (org-clock-projects-check-test-jump "open-clock")
      (should (equal (file-truename (buffer-file-name))
                     (file-truename (expand-file-name "corner-cafe.org" org-directory))))
      (should (string-match-p "^CLOCK: \\[2024-08-10 [^]]* 09:00\\]$"
                              (string-trim
                               (org-clock-projects-check-test-line-at-point)))))))

(ert-deftest org-clock-projects-check-test-a-finding-jumps-into-the-archive ()
  "An archive is one of the scanned files, so its findings jump like any other."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Design"
              (org-clock-projects-check-test-hours 8 3 9 3))))
        (,org-clock-projects-check-test-archive
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-clock-at
              "Archived and still running"
              (org-clock-projects-test-time-at 2024 8 12 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (org-clock-projects-check-test-jump "open-clock")
      (should (equal (file-truename (buffer-file-name))
                     (file-truename
                      (expand-file-name org-clock-projects-check-test-archive
                                        org-directory))))
      (should (string-match-p "^CLOCK: \\[2024-08-12 [^]]* 09:00\\]$"
                              (string-trim
                               (org-clock-projects-check-test-line-at-point)))))))

;;;; The check reports where the export refuses

(ert-deftest org-clock-projects-check-test-reports-what-the-export-refuses-to-write ()
  "The two share one findings core and differ only in what they do with it.
The check is the command to reach for once the export has stopped, so it
has to survive exactly the data that stopped it."
  (org-clock-projects-check-test-with-projects
      (list org-clock-projects-check-test-cafe)
      `(("corner-cafe.org"
         . ,(org-clock-projects-check-test-tagged
             "cafe"
             (org-clock-projects-test-closed-entry
              "Morning"
              (org-clock-projects-check-test-hours 8 3 9 3))
             (org-clock-projects-test-closed-entry
              "Also morning"
              (org-clock-projects-check-test-hours 8 3 11 2))
             (org-clock-projects-test-clock-at
              "Still running"
              (org-clock-projects-test-time-at 2024 8 10 9)))))
    (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
      (org-clock-projects-check)
      (should (equal (org-clock-projects-check-test-kind-set)
                     '("open-clock" "overlap"))))
    (let ((org-clock-projects-export-directory org-directory))
      (org-clock-projects-test-with-prompts '("Corner Café" "2024-08")
        (should-error (org-clock-projects-export) :type 'user-error)))))

(provide 'org-clock-projects-check-test)
;;; org-clock-projects-check-test.el ends here
