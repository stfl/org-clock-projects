;;; org-clock-projects.el --- Per-project parallel Org clocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Stefan Lendl

;; Author: Stefan Lendl <s@stfl.dev>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org "9.6") (org-clock-csv "1.2"))
;; Keywords: calendar, convenience
;; URL: https://github.com/stfl/org-clock-projects

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org tracks exactly one running clock, so switching between concurrent
;; engagements means clocking out of one to clock into the other.  This package
;; adds the notion of a *clock project* — an Org file that owns at most one open
;; clock and its own last clock-out time.  Several projects can hold open clocks
;; simultaneously; exactly one of them is Org's active clock at any moment, so
;; every stock Org clock command keeps working and operates on that one.
;;
;; Crossing a project boundary is always an explicit act: either
;; `org-clock-projects-fork', or answering the prompt a plain `org-clock-in'
;; raises when it targets a different project.  Switching to a project restores
;; that project's own clock-out time before the continuation logic runs, so
;; continuation is scoped to the engagement rather than to the session.
;;
;; The feature is dormant until a project is created.  With no project in the
;; session, every code path behaves exactly like stock Org clocking, and nothing
;; prompts, constrains, or displays.
;;
;; The projects come from `org-clock-projects-projects-function', which
;; defaults to reading `org-clock-projects-files'.  A project is a tag its
;; entries carry, a name it is called by and a file its time is kept in, and
;; the three need not agree; a bare file name is a project too.  Point the
;; function at your own registry to avoid maintaining the same list twice.
;;
;; That registry also feeds the reporting half of the package.
;; `org-clock-projects-export' writes one project's clock entries for a chosen
;; period — a month, a quarter, a year, an ISO week, or an arbitrary range —
;; to CSV, whole and in the order the work happened, so an invoice can be
;; checked against the Org file line by line.  `org-clock-projects-check'
;; reports what would make such an export untrustworthy: a clock still
;; running, two entries of one project covering the same minutes, an
;; implausibly long entry, and a project file that never mentions its own tag.
;; The `clockmatrix' dynamic block renders clocked time as calendar periods
;; against projects, and a family of `last<unit>s-N' keywords extends Org's
;; `:block' vocabulary with windows measured in whole periods.  Every one of
;; them resolves its period through one core, so a matrix cell and a CSV
;; describe the same stretch of time.
;;
;; This package is built on Org's own clock vocabulary rather than a parallel
;; implementation.  A clock is the cons of a marker and a start time — the shape
;; `org-find-open-clocks' returns and `org-clock-clock-in', `org-clock-clock-out'
;; and `org-clock-clock-cancel' consume.  Making a clock active is Org's own
;; clock-in with resume enabled, which reads the start time back out of the
;; existing clock line and writes nothing.  No clock global is assigned by hand.
;;
;; Set `org-clock-auto-clock-resolution' to nil alongside this package.  With
;; parallel clocks, dangling clock lines are the normal state, and Org's
;; resolution otherwise prompts about other projects on every clock-in made
;; while no clock is active.

;;; Code:

(require 'calendar)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'org-archive)
(require 'org-clock)
(require 'org-duration)
(require 'org-element)
(require 'org-table)
;; The CSV package is unmaintained and offers no filtering interface of any
;; kind, so the export reads two of its internals directly.  Both are named
;; where they are called, with the reasoning beside them.
(require 'org-clock-csv)

(defgroup org-clock-projects nil
  "Per-project parallel Org clocks."
  :group 'org-clock
  :prefix "org-clock-projects-")


;;;; Configuration

(defcustom org-clock-projects-files nil
  "Org files that may become clock projects.
Relative names are expanded against `org-directory'.  This is the list
the default `org-clock-projects-projects-function' turns into records."
  :type '(repeat file))

(defcustom org-clock-projects-continue-threshold 30
  "Minutes below which a project's clock-out time is resumed without asking.
Above it, clocking in on a project asks whether to start from that
project's stored clock-out time."
  :type 'integer)


;;;; Project records
;;
;; A project is three strings that need not agree: a tag its entries carry, a
;; name it is called by, and a file its time is kept in.  Every consumer here
;; wants a different one of them, so the registry is normalised once into a
;; record and no consumer applies a default of its own.

(defun org-clock-projects--record (entry)
  "Return ENTRY as a project record.
A record is a plist of `:tag', `:name' and `:file'.  ENTRY is either a
record already or a bare file name, and both shapes are answered for: a
registry that hands back plain file names keeps working, and one that has
learned to hand back records is understood the moment it does.

A bare file name is a record with no tag, called after the file.  A
record without a name is called by its tag, and one without a file keeps
its time in `<tag>.org'."
  (if (stringp entry)
      (list :tag nil :name (file-name-base entry) :file entry)
    (let* ((tag (plist-get entry :tag))
           (file (or (plist-get entry :file)
                     (and tag (format "%s.org" tag)))))
      (list :tag tag
            :name (or (plist-get entry :name)
                      tag
                      (and file (file-name-base file)))
            :file file))))

(defsubst org-clock-projects--record-tag (record)
  "Return the tag RECORD's entries carry, or nil when it has none."
  (plist-get record :tag))

(defsubst org-clock-projects--record-name (record)
  "Return the name RECORD is displayed under."
  (plist-get record :name))

(defsubst org-clock-projects--record-file (record)
  "Return the Org file RECORD keeps its time in."
  (plist-get record :file))

;; Declared ahead of the option it names, so that a setting made under the old
;; name is the setting the option starts life with.
(define-obsolete-variable-alias 'org-clock-projects-files-function
  'org-clock-projects-projects-function "0.2.0")

(defcustom org-clock-projects-projects-function
  #'org-clock-projects-default-projects
  "Function returning the projects this package knows about.
It takes no arguments and returns a list of entries, each either a plist
of `:tag', `:name' and `:file' or a bare file name.  Relative file names
are expanded against `org-directory'.  Point it at your own project
registry's accessor to keep one list rather than two."
  :type 'function)

(defun org-clock-projects-default-projects ()
  "Return `org-clock-projects-files' as records."
  (mapcar #'org-clock-projects--record org-clock-projects-files))

(define-obsolete-function-alias 'org-clock-projects-default-files
  #'org-clock-projects-default-projects "0.2.0")

(defun org-clock-projects--records ()
  "Return every registered project as a normalised record."
  (mapcar #'org-clock-projects--record
          (funcall org-clock-projects-projects-function)))


;;;; Session state

(defvar org-clock-projects--projects nil
  "Alist of (PROJECT . CLOCK-OUT-TIME) for every project created this session.
PROJECT is the truename of an Org file.  CLOCK-OUT-TIME is the time that
project's clock was last stopped, or nil.  A project exists only after it
is explicitly created; visiting or clocking in a file never enrols it.")

(defvar org-clock-projects--unscoped-out-time nil
  "The time unscoped work was last clocked out, or nil.
The nil scope is a scope like any other: it holds at most one open clock
and continues only from its own clock-out.  Reading Org's session-wide
`org-clock-out-time' here instead would let a project's clock-out flow
into an unenrolled client file.")

(defvar org-clock-projects--current nil
  "Truename of the project this session is scoped to, or nil.
This pointer outlives the project's clocks: it is what the modeline names
and what the boundary check compares against.")

(defvar org-clock-projects--inhibit nil
  "Non-nil while this package drives `org-clock-in' itself.")

(defvar org-clock-projects--parallel nil
  "Non-nil while `org-clock-projects-fork' drives `org-clock-in'.")

(defun org-clock-projects--live-p ()
  "Return non-nil once at least one project exists.
While no project exists, every path in this package defers to stock Org."
  (and org-clock-projects--projects t))


;;;; Files and scope

(defun org-clock-projects--truename (file)
  "Return the truename of FILE, expanded against `org-directory'."
  (when (stringp file)
    (file-truename (expand-file-name file (or org-directory default-directory)))))

(defun org-clock-projects--candidate-files ()
  "Return the truenames of every file that may become a project."
  (delete-dups
   (delq nil (mapcar (lambda (record)
                       (org-clock-projects--truename
                        (org-clock-projects--record-file record)))
                     (org-clock-projects--records)))))

(defun org-clock-projects--org-buffer (file)
  "Return a buffer visiting FILE in `org-mode'.
Org names an archive for itself — `<file>.org_archive' by default — and
no `auto-mode-alist' entry claims that name, so a buffer visiting one
arrives in Fundamental mode, where reading a tag raises an error and
reading an inherited property answers nil.  The file is an Org file by
construction, so the mode is put on it.

The mode is turned on in full rather than with the hooks held back: a
finding jumps into this buffer, and a reader who lands in a half-prepared
Org buffer to fix a clock line has been handed something stranger than a
slow command."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode) (org-mode)))
    buffer))

(defun org-clock-projects--candidate-p (file)
  "Return non-nil when FILE may become a project."
  (and file (member file (org-clock-projects--candidate-files)) t))

(defun org-clock-projects--buffer-file (&optional buffer)
  "Return the truename of the file BUFFER visits, or nil.
BUFFER defaults to the current buffer; indirect buffers resolve to their
base buffer."
  (let ((buffer (or buffer (current-buffer))))
    (org-clock-projects--truename
     (buffer-file-name (or (buffer-base-buffer buffer) buffer)))))

(defun org-clock-projects--trailing (file depth)
  "Return the last DEPTH path components of FILE, the last one sans extension.
Stops at the root, so a DEPTH beyond the path's own length renders the
whole of it."
  (let ((parts (list (file-name-base file)))
        (rest (directory-file-name (or (file-name-directory file) ""))))
    (while (and (< (length parts) depth)
                (not (member rest '("" "/" "."))))
      (push (file-name-nondirectory rest) parts)
      (setq rest (directory-file-name (or (file-name-directory rest) ""))))
    (string-join parts "/")))

(defun org-clock-projects-name (project)
  "Return the display name of PROJECT.
The base name, extended leftwards with as many enclosing directories as
it takes to tell it from every other candidate.  The resolver may draw
from any registry, so two project files named alike is a legal
arrangement rather than damage, and one name for both would make them
indistinguishable in the switcher, in the modeline and at every prompt.
Qualifying by a single directory is not enough: the directories can be
named alike too."
  ;; Distinct truenames always differ at some depth, which is what ends this;
  ;; the fixed-point test is a bound on it rather than the mechanism, for a
  ;; candidate list that turns out not to hold distinct paths after all.
  (let ((candidates (org-clock-projects--candidate-files))
        (depth 1)
        name previous)
    (while (progn
             (setq name (org-clock-projects--trailing project depth))
             (and (not (equal name previous))
                  (< 1 (seq-count
                        (lambda (candidate)
                          (equal name (org-clock-projects--trailing
                                       candidate depth)))
                        candidates))))
      (setq previous name
            depth (1+ depth)))
    name))

(defun org-clock-projects--scope (file)
  "Return the created project owning FILE, or nil.
A clock's scope is derived from its file every time it is needed, never
stored per clock, so it survives a restart and needs no bookkeeping."
  (car (assoc file org-clock-projects--projects)))

(defun org-clock-projects--create (file)
  "Create a project on FILE if it does not exist, and return it."
  (let ((file (org-clock-projects--truename file)))
    (unless (assoc file org-clock-projects--projects)
      (push (cons file nil) org-clock-projects--projects))
    file))

(defun org-clock-projects--out-time (project)
  "Return PROJECT's stored clock-out time, or nil.
A nil PROJECT is unscoped work, which keeps its own time rather than
Org's session-wide one."
  (if project
      (cdr (assoc project org-clock-projects--projects))
    org-clock-projects--unscoped-out-time))

(defun org-clock-projects--set-out-time (file time)
  "Store TIME as the clock-out time of the scope owning FILE."
  (let ((cell (assoc file org-clock-projects--projects)))
    (if cell
        (setcdr cell time)
      (setq org-clock-projects--unscoped-out-time time))))


;;;; Open clocks
;;
;; The set of open clocks is derived from the files every time it is needed and
;; never cached, so clocks left open across a restart, or started by another
;; tool, are still visible.

(defsubst org-clock-projects--entry-file (entry)
  "Return the file of open-clock ENTRY."
  (car entry))

(defsubst org-clock-projects--entry-clock (entry)
  "Return the clock of open-clock ENTRY."
  (cdr entry))

(defsubst org-clock-projects--entry-start (entry)
  "Return the start time of open-clock ENTRY."
  (cddr entry))

(defun org-clock-projects-open-clocks ()
  "Return every open clock, oldest first.
The value is an alist of (FILE . CLOCK), where CLOCK is the cons of a
marker and a start time.

Several clocks in one file are all reported.  A scope holding more than
one is damage — a crash, or an exit query that was declined — and only
paths that can see the surplus can close it; reporting one clock per file
would leave the rest permanently invisible to clock-out-all and to the
exit query, and so permanent.  The active clock is included even when its
file is not a project candidate, so an unscoped clock is never invisible."
  (let (entries)
    (dolist (file (org-clock-projects--candidate-files))
      (when (file-exists-p file)
        ;; `org-find-open-clocks' scans from `point-min' without widening, so
        ;; a narrowed buffer hides its own clocks — and everything here is
        ;; derived from this one scan, so the invariant, the bulk clock-out,
        ;; the exit query and the modeline would all go blind together.
        (with-current-buffer (or (get-file-buffer file) (find-file-noselect file))
          (save-restriction
            (widen)
            (dolist (clock (org-find-open-clocks file))
              (push (cons file clock) entries))))))
    (when (org-clock-is-active)
      (let ((file (org-clock-projects--buffer-file (org-clocking-buffer))))
        (when (and file
                   (not (seq-find
                         (lambda (entry)
                           (org-clock-projects--active-clock-p
                            (org-clock-projects--entry-clock entry)))
                         entries)))
          (push (cons file (cons (copy-marker org-clock-marker t)
                                 org-clock-start-time))
                entries))))
    (sort entries (lambda (a b)
                    (time-less-p (org-clock-projects--entry-start a)
                                 (org-clock-projects--entry-start b))))))

(defun org-clock-projects--scope-clocks (scope)
  "Return every open clock whose file belongs to SCOPE, oldest first.
SCOPE is a project or nil, nil meaning unscoped work."
  (mapcar #'org-clock-projects--entry-clock
          (seq-filter (lambda (entry)
                        (equal scope (org-clock-projects--scope
                                      (org-clock-projects--entry-file entry))))
                      (org-clock-projects-open-clocks))))

(defun org-clock-projects--file-clocks (file)
  "Return every open clock in FILE, oldest first."
  (mapcar #'org-clock-projects--entry-clock
          (seq-filter (lambda (entry)
                        (equal file (org-clock-projects--entry-file entry)))
                      (org-clock-projects-open-clocks))))

(defun org-clock-projects--active-clock-p (clock)
  "Return non-nil when CLOCK is the clock Org considers active.
`org-is-active-clock' compares marker positions alone.  Org can afford
that because it never holds two clocks at once; with parallel clocks in
different files, two markers at the same character offset compare equal,
and acting on the wrong one writes another project's time.  Tracked as
stfl/doom.d#5, to be dropped if Org takes a fix."
  (and (org-clock-is-active)
       (eq (marker-buffer org-clock-marker) (marker-buffer (car clock)))
       (= org-clock-marker (car clock))))

(defun org-clock-projects--clock-heading (clock)
  "Return the heading CLOCK is clocked on."
  (org-with-clock-position clock
    (org-back-to-heading t)
    (org-get-heading t t t t)))

(defun org-clock-projects--resumable-p (clock)
  "Return non-nil when Org can resume CLOCK's line in place.
`org-find-open-clocks' accepts any inactive timestamp, and the element
parser calls the line a clock, but Org's resume branch demands a full
`[YYYY-MM-DD DDD HH:MM]'.  A line the reader accepts and the resume
branch rejects is answered with a *new* clock line rather than a resumed
one, which would leave the scope holding two.

The pattern is the one in `org-clock-in's resume `cond', deliberately,
and not the stricter one in `org-clock-find-position': the `cond' is what
decides whether a line is written, and it allows the abbreviating dot
some day names carry.  Copying the stricter pattern would turn a clock
Org resumes perfectly well into one this refuses."
  (org-with-clock-position clock
    (looking-at (concat "^[ \t]*" org-clock-string
                        " \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
                        " *\\sw+.? +[012][0-9]:[0-5][0-9]\\][ \t]*$"))))

(defun org-clock-projects--running-time (start)
  "Return the time run since START, as Org formats a clock duration."
  (org-duration-from-minutes
   (/ (org-time-convert-to-integer (time-subtract (current-time) start)) 60)
   'h:mm))

(defun org-clock-projects--clock-description (clock)
  "Describe CLOCK by its heading, start time and how long it has run.
A clock that has been open since yesterday and one opened five minutes
ago are answered for with the same keystroke, so the answer needs to be
able to tell them apart."
  (format "%s (%s, %s)"
          (org-clock-projects--clock-heading clock)
          (format-time-string "%H:%M" (cdr clock))
          (org-clock-projects--running-time (cdr clock))))

(defun org-clock-projects--activate (clock &optional parallel)
  "Make CLOCK Org's active clock, and return non-nil when it becomes so.
This is Org's own clock-in with resume enabled: it reads the start time
back out of the existing clock line, restores effort and accumulated
total, and writes no new clock line.  With PARALLEL non-nil the clock
that was active is left open rather than stopped.

A clock whose line Org cannot resume is reported and left alone, because
the alternative is Org quietly writing a second clock beside it."
  (if (org-clock-projects--active-clock-p clock)
      t
    (if (not (org-clock-projects--resumable-p clock))
        (progn
          (display-warning
           'org-clock-projects
           (format "Cannot resume the clock on %s: its clock line carries no start time"
                   (org-clock-projects--clock-heading clock)))
          nil)
      (org-clock-projects--activate-1 clock parallel)
      t)))

(defun org-clock-projects--activate-1 (clock parallel)
  "Hand CLOCK to Org as the active clock.  See `org-clock-projects--activate'."
  (let ((org-clock-resolving-clocks-due-to-idleness parallel)
        ;; Resuming a clock this package located must never raise Org's
        ;; dangling-clock prompt; Org binds this the same way around the
        ;; resuming clock-in in `org-clock-load'.
        (org-clock-auto-clock-resolution nil)
        (org-clock-projects--inhibit t))
    (org-clock-clock-in clock t)))

(defun org-clock-projects--clock-out (clock &optional at-time)
  "Clock out of CLOCK at AT-TIME, without prompting for a note.
With no AT-TIME, Org applies the configured rounding itself.
Clock-outs this package performs are bookkeeping rather than the end of a
task, and several of them are followed by more work in the same command,
so they use the binding Org itself uses in `org-clock-out-if-current'.
This is `org-clock-clock-out' with `org-clock-projects--active-clock-p'
in place of `org-is-active-clock': the two differ exactly when another
project's clock sits at the same character offset, and there the stock
predicate sends the clock-out to the wrong file."
  (let ((org-log-note-clock-out nil)
        (org-clock-out-switch-to-state nil))
    (cond
     ((org-clock-projects--active-clock-p clock) (org-clock-out nil t at-time))
     ((org-clock-is-active)
      ;; `org-with-clock' redirects the marker, but `org-clock-out' tears down
      ;; the display, the timers and the remembered task of *the* clock — and
      ;; here that is a different one, still running.  Losing
      ;; `org-clock-current-task' is not cosmetic: Org stops recognising a
      ;; clock-in on the running task as the same task, and splits its clock
      ;; line instead of continuing it.  The nil timers are what stops Org
      ;; cancelling the running clock's ticker.
      (let ((org-clock-current-task org-clock-current-task)
            (org-clock-heading org-clock-heading)
            ;; Copied, not merely rebound: `delq' splices the list in
            ;; place, so restoring the variable would restore a list
            ;; that has already lost the entry.
            (global-mode-string (copy-sequence global-mode-string))
            (org-clock-mode-line-timer nil)
            (org-clock-idle-timer nil)
            (org-frame-title-format-backup nil)
            (temp (copy-marker (car clock) (marker-insertion-type (car clock)))))
        (org-with-clock clock (org-clock-out nil t at-time))
        (setcar clock temp)))
     (t
      (let ((temp (copy-marker (car clock) (marker-insertion-type (car clock)))))
        (org-with-clock clock (org-clock-out nil t at-time))
        (setcar clock temp))))))


;;;; Continuation
;;
;; Continuation is scoped to a project rather than to the session, so clocking
;; out of one engagement and into another never backdates the new clock to the
;; other engagement's clock-out.

(defun org-clock-projects--minutes-ago (time)
  "Return the whole minutes elapsed since TIME.
Deliberately not `org-current-time': called with no rounding argument it
falls back to `org-time-stamp-rounding-minutes', which governs `C-c .'
and has nothing to do with clocks.  A non-zero setting there would both
mis-state the gap in the prompt and move the threshold under it."
  (/ (org-time-convert-to-integer (time-subtract (current-time) time)) 60))

(defun org-clock-projects--minutes-ago-rounded (time)
  "Return the whole minutes since TIME, counted from the rounded now."
  (/ (org-time-convert-to-integer
      (time-subtract (org-current-time org-clock-rounding-minutes t) time))
     60))

(defun org-clock-projects--format-ago (time)
  "Describe TIME and how long ago it was."
  (format "%s (-%dm) (~%dm)"
          (format-time-string (org-time-stamp-format 'with-time t) time)
          (org-clock-projects--minutes-ago time)
          (org-clock-projects--minutes-ago-rounded time)))

(defun org-clock-projects--clock-out-at (clock at-time)
  "Clock out of CLOCK at AT-TIME, unless that would precede its start.
A clock that began after the time asked for is left open and reported,
rather than recorded as a negative duration."
  (if (time-less-p at-time (cdr clock))
      (display-warning
       'org-clock-projects
       (format "Left open, %s began after the time given"
               (org-clock-projects--clock-heading clock)))
    (org-clock-projects--clock-out clock at-time)))

(defun org-clock-projects--continue-p (time)
  "Return non-nil when a clock should start from TIME.
Below `org-clock-projects-continue-threshold' minutes this is silent;
above it the user is asked.  A nil TIME is never continued from, so a
project with no stored clock-out time raises no question at all."
  (and time
       (or (< (org-clock-projects--minutes-ago time)
              org-clock-projects-continue-threshold)
           (y-or-n-p (format "You stopped another clock at %s; start this one from then? "
                             (org-clock-projects--format-ago time))))))


;;;; Modeline

(defvar org-clock-projects-mode-line-string ""
  "Modeline segment naming the project this session is scoped to.
Empty while no project is current.  The symbol is appended to
`global-mode-string' by `org-clock-projects-mode'; Org's own active-clock
display is left untouched.")

(put 'org-clock-projects-mode-line-string 'risky-local-variable t)

(defun org-clock-projects--update-mode-line ()
  "Recompute `org-clock-projects-mode-line-string'.
Counting open clocks reads the project files, so this runs at state
changes rather than on every modeline redisplay."
  (setq org-clock-projects-mode-line-string
        (if (null org-clock-projects--current)
            ""
          (let ((others (cl-count-if-not
                         (lambda (entry)
                           (equal (org-clock-projects--entry-file entry)
                                  org-clock-projects--current))
                         (org-clock-projects-open-clocks))))
            ;; Padded on both sides: `global-mode-string' entries render
            ;; flush against each other, and Org's own clock string carries
            ;; no space of its own.
            (format " ⏱ %s%s "
                    (org-clock-projects-name org-clock-projects--current)
                    (if (> others 0) (format " [+%d]" others) "")))))
  (force-mode-line-update t)
  org-clock-projects-mode-line-string)


;;;; Clocking in

(defun org-clock-projects--clock-in-advice (orig-fn &rest args)
  "Scope ORIG-FN, `org-clock-in', to the project owning the target file.
ARGS are `org-clock-in's own.  Every call made while no project exists is
passed through with the session-wide continuation prompt alone."
  (let ((select (car args))
        (start-time (cadr args)))
    (cond
     (org-clock-projects--inhibit (apply orig-fn args))
     ;; Org resolves dangling clocks by clocking into them; scoping those
     ;; would raise this package's questions in the middle of a resolution.
     (org-clock-resolving-clocks (apply orig-fn args))
     ((not (org-clock-projects--live-p))
      ;; Dormant: stock Org, plus the session-wide continuation prompt.  This
      ;; is tested before the selection is intercepted below, so that while no
      ;; project exists every path really does reach Org unaltered.
      (let ((org-clock-continuously
             (if start-time
                 org-clock-continuously
               (or (org-clocking-p)
                   (org-clock-projects--continue-p org-clock-out-time)))))
        (apply orig-fn args)))
     ;; A task selection resolves to a marker in another file, so the target
     ;; cannot be read off the current buffer.  Resolve it here and clock in
     ;; at that task, which re-enters with the right file current.
     ((equal select '(4))
      (let ((task (org-clock-select-task "Clock-in on task: ")))
        (unless task (error "Abort"))
        (org-with-point-at task (org-clock-in nil start-time))))
     (t (org-clock-projects--clock-in-scoped orig-fn args)))))

(defun org-clock-projects--resolve-target (file scope parallel)
  "Decide which project a clock-in on FILE belongs to.
SCOPE is the project owning FILE, and PARALLEL is non-nil when the caller
has already settled what becomes of the running clock.  Returns the cons
of the target project and the parallel decision.

Crossing a boundary asks two independent questions: whether to stop the
current project's clock, and what to do about the target file."
  (cond
   ;; `org-clock-projects-fork' has already settled the scope.
   (parallel (cons scope parallel))
   ((equal scope org-clock-projects--current)
    (cons org-clock-projects--current parallel))
   (t
    ;; Question one: the clock that is running now.
    (when (org-clocking-p)
      (setq parallel
            (not (y-or-n-p (format "Stop the clock running in %s? "
                                   (org-clock-projects--active-label))))))
    ;; Question two: the file being clocked into.  Creating a project is only
    ;; on offer when the file may become one.
    (cons (if (and (null scope) (org-clock-projects--candidate-p file))
              (and (y-or-n-p (format "Create clock project %s? "
                                     (org-clock-projects-name file)))
                   (org-clock-projects--create file))
            scope)
          parallel))))

(defun org-clock-projects--clock-in-scoped (orig-fn args)
  "Run ORIG-FN, `org-clock-in', with ARGS, scoped to a project.
Crossing a project boundary asks two independent questions: whether to
stop the current project's clock, and what to do about the target file.
A start time supplied in ARGS is the caller's, and only the scope and the
one-clock-per-project invariant are decided here."
  (let* ((file (org-clock-projects--buffer-file))
         (start-time (cadr args))
         ;; Captured before anything is decided: nothing below is committed
         ;; until the clock-in itself succeeds.
         (previous-current org-clock-projects--current)
         (previous-projects org-clock-projects--projects)
         (committed nil))
    (unwind-protect
        (prog1
            (pcase-let ((`(,target . ,parallel)
                         (org-clock-projects--resolve-target
                          file (org-clock-projects--scope file)
                          org-clock-projects--parallel)))
              ;; One clock per project.
              (let ((existing (org-clock-projects--scope-clocks target)))
                (when existing
                  ;; More than one is damage rather than a state this package
                  ;; creates, so the surplus is closed rather than assumed away.
                  (if start-time
                      ;; The caller fixed when the new clock begins, so the
                      ;; clocks it replaces have to end at that same moment.
                      ;; Handing one to Org to interrupt would end it at now
                      ;; instead, and the difference would be billed to both.
                      (dolist (clock existing)
                        (org-clock-projects--clock-out-at clock start-time))
                    ;; The newest is handed to Org active, and Org's own
                    ;; interrupt handling closes it, exactly as it would for
                    ;; any ordinary clock-in within one file.
                    (dolist (clock (butlast existing))
                      (org-clock-projects--clock-out clock))
                    (when (org-clock-projects--activate
                           (car (last existing)) parallel)
                      ;; Org must now stop the clock it has just been handed.
                      (setq parallel nil)))))
              (setq org-clock-projects--current target)
              (cond
               ;; The caller fixed the start time, so it wins.  Leaving
               ;; `org-clock-continuously' ambient would let the clock-out Org
               ;; performs on the way in overwrite `org-clock-out-time' with
               ;; now and beat the caller's time to `org-clock-in's `or'.
               (start-time
                ;; Same reasoning for a clock still running elsewhere: Org
                ;; would close it at now while this one starts earlier.
                (when (and (org-clocking-p) (not parallel))
                  (org-clock-projects--clock-out-at
                   (cons org-clock-marker org-clock-start-time) start-time))
                (let ((org-clock-continuously nil)
                      (org-clock-leftover-time nil)
                      (org-clock-resolving-clocks-due-to-idleness parallel))
                  (apply orig-fn args)))
               ;; A clock is being stopped by this very clock-in, so the two
               ;; are contiguous and the new one starts exactly where that one
               ;; ends.  There is no gap to misattribute — and letting Org
               ;; compute a fresh start instead would round it *down*, below
               ;; the end just rounded *up*, billing one interval to both.
               ((and (org-clocking-p) (not parallel))
                (let ((org-clock-continuously t)
                      (org-clock-leftover-time nil)
                      (org-clock-resolving-clocks-due-to-idleness nil))
                  (apply orig-fn args)))
               ;; Start from the last clock-out: this project's own, never the
               ;; session's, which may belong to another engagement.
               ((equal (car args) '(64))
                (let ((org-clock-continuously nil)
                      (org-clock-leftover-time nil)
                      (org-clock-resolving-clocks-due-to-idleness parallel))
                  (funcall orig-fn nil (org-clock-projects--out-time target))))
               (t
                ;; No clock is ending here, so any gap belongs to whoever the
                ;; target is, and only the target's own clock-out time can
                ;; close it.  `org-clock-continuously' cannot be used:
                ;; `org-clock-out' overwrites `org-clock-out-time' with now
                ;; before `org-clock-in' reads it, which is how time bridges
                ;; between engagements.
                (let* ((out (org-clock-projects--out-time target))
                       (start (and (org-clock-projects--continue-p out) out))
                       (org-clock-continuously nil)
                       (org-clock-leftover-time nil)
                       (org-clock-resolving-clocks-due-to-idleness parallel))
                  (funcall orig-fn (car args) start)))))
          (setq committed t))
      (unless committed
        ;; A project created for a clock-in that never happened would suppress
        ;; the boundary prompt for that file ever after.  This covers the
        ;; repair as well as the clock-in: closing a surplus clock can fail.
        (setq org-clock-projects--current previous-current
              org-clock-projects--projects previous-projects))
      (org-clock-projects--update-mode-line))))

(defun org-clock-projects--active-label ()
  "Return a name for the scope of the clock running now."
  (let ((scope (org-clock-projects--scope
                (org-clock-projects--buffer-file (org-clocking-buffer)))))
    (if scope (org-clock-projects-name scope) "no project")))


;;;; Clocking out

(defun org-clock-projects--clock-out-advice (orig-fn &rest args)
  "Run ORIG-FN, `org-clock-out', with ARGS and store the time per project.
The project this session is scoped to is left alone: a project outlives
its clocks, so its stored clock-out time is still there on return."
  (let ((file (and (org-clocking-p)
                   (org-clock-projects--buffer-file (org-clocking-buffer)))))
    (prog1 (apply orig-fn args)
      (when (and file org-clock-out-time)
        (org-clock-projects--set-out-time file org-clock-out-time))
      (org-clock-projects--update-mode-line))))


;;;; Commands

;;;###autoload
(defun org-clock-projects-fork ()
  "Clock in at point, leaving the clock in the current project running.
Creates a project on this file if there is none, and switches to it.
When that project already has an open clock, offers to stop it first, so
that a project never holds two."
  (interactive)
  (let ((file (org-clock-projects--buffer-file)))
    (unless (org-clock-projects--candidate-p file)
      (user-error "%s is not a clock project file"
                  (if file (abbreviate-file-name file) "This buffer")))
    (let ((existing (org-clock-projects--file-clocks file)))
      (when (and existing
                 (not (y-or-n-p
                       (format "%s already has %s; clock out and start here? "
                               (org-clock-projects-name file)
                               (mapconcat
                                #'org-clock-projects--clock-description
                                existing ", ")))))
        (user-error "Aborted"))
      (org-clock-projects--create file)
      ;; Every one of them: leaving any behind keeps the project over its one
      ;; clock, and if the one left running is the heading at point, Org reads
      ;; the clock-in as a continuation of it and writes nothing at all.
      (dolist (clock existing) (org-clock-projects--clock-out clock)))
    (let ((org-clock-projects--parallel t))
      (org-clock-in))))

;;;###autoload
(defun org-clock-projects-switch ()
  "Make one of the open clocks active and jump to it.
With one open clock this jumps straight there, making it active first if
it is not.  With none, this falls back to `org-clock-goto'."
  (interactive)
  (let ((entries (org-clock-projects-open-clocks)))
    (cond
     ((null entries) (call-interactively #'org-clock-goto))
     ((null (cdr entries)) (org-clock-projects--goto (car entries)))
     (t (org-clock-projects--goto (org-clock-projects--read-clock entries))))))

(defun org-clock-projects--goto (entry)
  "Make open-clock ENTRY active and jump to its heading."
  (let ((clock (org-clock-projects--entry-clock entry)))
    (unless (org-clock-projects--resumable-p clock)
      ;; Go to the offending line so it can be repaired, then say plainly that
      ;; nothing was switched.  Reporting this quietly would leave the user
      ;; believing they had changed project while the previous clock runs on,
      ;; and every minute after that is billed to it.
      (pop-to-buffer-same-window (marker-buffer (car clock)))
      (widen)
      (goto-char (car clock))
      (org-back-to-heading t)
      (org-fold-show-context)
      (user-error "Cannot switch to %s: its clock line carries no start time"
                  (org-clock-projects-name
                   (org-clock-projects--entry-file entry))))
    ;; Every other open clock stays open: this switches which one Org considers
    ;; active, it does not end anything.
    (org-clock-projects--activate clock t)
    (setq org-clock-projects--current
          (org-clock-projects--scope (org-clock-projects--entry-file entry)))
    (org-clock-projects--update-mode-line)
    (org-clock-goto)))

(defun org-clock-projects--annotation (entry)
  "Return the annotation for open-clock ENTRY: heading, start and duration."
  (let ((start (org-clock-projects--entry-start entry)))
    (format "  %s  %s  %s"
            (format-time-string "%H:%M" start)
            (org-clock-projects--running-time start)
            (org-clock-projects--clock-heading
             (org-clock-projects--entry-clock entry)))))

(defun org-clock-projects--candidate (entry entries)
  "Return the candidate string for ENTRY among ENTRIES.
A project holding one clock is named by itself.  One holding several —
which only happens through damage — names each clock by its heading.
Two clocks on the *same* heading still read alike, and Org resumes the
first unclosed line of an entry whichever marker it is handed, so that
shape is repaired by clocking in on the project or by
`org-clock-projects-clock-out-all' rather than through the switcher."
  (let ((name (org-clock-projects-name (org-clock-projects--entry-file entry))))
    (if (< 1 (seq-count (lambda (other)
                          (equal (org-clock-projects--entry-file other)
                                 (org-clock-projects--entry-file entry)))
                        entries))
        (format "%s: %s" name
                (org-clock-projects--clock-heading
                 (org-clock-projects--entry-clock entry)))
      name)))

(defun org-clock-projects--read-clock (entries)
  "Read one of the open-clock ENTRIES and return it.
Candidates are project names, so typing filters on those alone and cannot
accidentally match a duration or a timestamp.  The completion category is
private on purpose: annotation packages override annotations for
categories they recognise and defer only for ones they do not, so a
well-known name would silently lose these annotations."
  (let* ((candidates (mapcar (lambda (entry)
                               (cons (org-clock-projects--candidate entry entries)
                                     entry))
                             entries))
         (annotations (mapcar (lambda (cell)
                                (cons (car cell)
                                      (org-clock-projects--annotation (cdr cell))))
                              candidates))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata
                        (category . org-clock-projects--clock)
                        (annotation-function
                         . ,(lambda (candidate) (cdr (assoc candidate annotations))))
                        ;; Pinned so the oldest-first order survives.
                        (display-sort-function . identity)
                        (cycle-sort-function . identity))
                    (complete-with-action action candidates string predicate)))))
    (cdr (assoc (completing-read "Clock: " table nil t) candidates))))

;;;###autoload
(defun org-clock-projects-clock-out-all ()
  "Clock out of every open clock.
No note is asked for, and Org applies the configured rounding itself.  A
clock whose rounded end would precede its start is skipped rather than
recorded as a negative duration."
  (interactive)
  (let ((now (org-current-time org-clock-rounding-minutes))
        (closed 0)
        skipped)
    (dolist (entry (org-clock-projects-open-clocks))
      (if (time-less-p now (org-clock-projects--entry-start entry))
          (push (org-clock-projects-name (org-clock-projects--entry-file entry))
                skipped)
        (org-clock-projects--clock-out (org-clock-projects--entry-clock entry))
        (cl-incf closed)))
    (org-clock-projects--update-mode-line)
    (when skipped
      (display-warning
       'org-clock-projects
       (format "Left open, the end time would precede the start: %s"
               (string-join (nreverse skipped) ", "))))
    (message "Clocked out %d clock%s" closed (if (= closed 1) "" "s"))))

;;;###autoload
(defun org-clock-projects-clear (&optional all)
  "Forget a project's session state, so its stored clock-out time stops applying.
Offers to clock out the project's open clock; declining keeps the clock
and leaves it unscoped.  With a prefix argument ALL, do this for every
project, returning the session to its untouched state."
  (interactive "P")
  (unless org-clock-projects--projects (user-error "No clock projects"))
  (dolist (project (if all
                       (mapcar #'car org-clock-projects--projects)
                     (list (org-clock-projects--read-project))))
    (org-clock-projects--clear-1 project))
  ;; A deliberate exception to leaving promotion alone: after clearing, the
  ;; clock that is left without a home becomes the active one.
  (let ((unscoped (car (org-clock-projects--scope-clocks nil))))
    (when unscoped (org-clock-projects--activate unscoped t)))
  (unless org-clock-projects--projects
    ;; Back to the untouched state, carrying nothing forward.
    (setq org-clock-projects--unscoped-out-time nil))
  (org-clock-projects--update-mode-line))

(defun org-clock-projects--clear-1 (project)
  "Forget PROJECT, deciding what becomes of its open clock."
  (let ((clocks (org-clock-projects--file-clocks project))
        (name (org-clock-projects-name project)))
    (when clocks
      (if (y-or-n-p (format "Clock out %s in %s? "
                            (mapconcat #'org-clock-projects--clock-description
                                       clocks ", ")
                            name))
          (dolist (clock clocks) (org-clock-projects--clock-out clock))
        (when (org-clock-projects--scope-clocks nil)
          (display-warning
           'org-clock-projects
           (format "The clock in %s is left without a home: an unscoped clock is already open"
                   name)))))
    (setq org-clock-projects--projects
          (assoc-delete-all project org-clock-projects--projects))
    (when (equal org-clock-projects--current project)
      (setq org-clock-projects--current nil))))

(defun org-clock-projects--read-project ()
  "Read a project, defaulting to the current one."
  (let* ((candidates (mapcar (lambda (cell)
                               (cons (org-clock-projects-name (car cell)) (car cell)))
                             org-clock-projects--projects))
         (default (and org-clock-projects--current
                       (org-clock-projects-name org-clock-projects--current))))
    (cdr (assoc (completing-read (format-prompt "Clear project" default)
                                 candidates nil t nil nil default)
                candidates))))

;;;###autoload
(defun org-clock-projects-kill-emacs-query ()
  "Ask about each open clock when Emacs is killed, and return t.
This replaces `org-clock-kill-emacs-query', which only ever inspects the
active clock and so sees nothing when none is active.  Clocks left open
are still there after a restart, because open clocks are read back from
the files.

`org-clock-ask-before-exiting' is consulted here rather than only when
this was installed, so turning it off takes effect without cycling the
mode."
  (when org-clock-ask-before-exiting
    (dolist (entry (org-clock-projects-open-clocks))
    (let* ((clock (org-clock-projects--entry-clock entry))
           (buffer (marker-buffer (car clock))))
      (when (and (buffer-live-p buffer)
                 (yes-or-no-p
                  (format "Clock out %s: %s? "
                          (org-clock-projects-name
                           (org-clock-projects--entry-file entry))
                          (org-clock-projects--clock-description clock))))
        (org-clock-projects--clock-out clock)
        (with-current-buffer buffer
          (when (buffer-file-name) (save-buffer)))))))
  t)


;;;; Periods
;;
;; A period is a plist of `:kind', `:start' and `:end', bounded half-open at
;; midnight.  Every boundary comes from `org-clock-special-range', so months,
;; quarters, ISO weeks and years are Org's own rather than a second calendar
;; arithmetic that drifts from it.  The exporter, the check and the clock
;; matrix all resolve through here, which is what makes a cell and a CSV
;; describe the same stretch of time.

(defconst org-clock-projects--period-keys
  '((lastmonth . month) (thismonth . month)
    (lastq . quarter) (thisq . quarter)
    (lastweek . week) (thisweek . week)
    (lastyear . year) (thisyear . year)
    (today . day) (yesterday . day))
  "The kind of period each of Org's relative range keys names.
The range resolver answers with bounds and a display string but with no
kind of its own, and the display string is locale-dependent, so the kind
has to be stated rather than read back.  Without this table the default
selection would fail to classify and be named as though it were an
arbitrary range.")

(defconst org-clock-projects--period-forms
  '(("\\`[0-9]+\\'" . year)
    ("\\`[0-9]+-[0-9]\\{1,2\\}\\'" . month)
    ("\\`[0-9]+-[wW][0-9]\\{1,2\\}\\'" . week)
    ("\\`[0-9]+-[qQ][1-4]\\'" . quarter)
    ("\\`[0-9]+-[0-9]\\{1,2\\}-[0-9]\\{1,2\\}\\'" . day))
  "The kind of period each typed range form names.
The patterns are the range resolver's own, so a form that resolves is a
form that classifies and the two cannot drift apart.")

(defconst org-clock-projects--period-candidates
  '("lastmonth" "thismonth" "lastq" "thisq"
    "lastweek" "thisweek" "lastyear" "thisyear" "range")
  "The periods the prompt offers, previous month first.
Anything else may still be typed; `range' asks for two dates instead.")

(defconst org-clock-projects--period-forms-help
  "a month, a quarter, a year, a week (YYYY-MM, YYYY-QN, YYYY, YYYY-Www) or a range"
  "The periods an export accepts, worded as a refusal names them.")

(defun org-clock-projects--period-kind (key)
  "Return the kind of period KEY names, or `interval' when it names none.
A key that resolves without classifying — the rolling `last<unit>s-N'
keywords among them — is an interval, which is a period like any other
and is named by its two dates rather than refused."
  (or (alist-get key org-clock-projects--period-keys)
      (let ((written (symbol-name key)))
        (cdr (seq-find (lambda (form) (string-match-p (car form) written))
                       org-clock-projects--period-forms)))
      'interval))

(defun org-clock-projects--period (spec &optional wstart mstart time)
  "Return the period SPEC names, as a plist of `:kind', `:start' and `:end'.
SPEC is a range key, as a symbol or a string, or a cons of the start and
end times of an interval.  WSTART and MSTART are the week and month start
days, and TIME the instant a relative key is read against, all with the
meanings `org-clock-special-range' gives them.

SPEC is interned before it is resolved.  The relative keys are dispatched
on symbols, so `lastmonth' resolves where the string does not, while the
typed forms reach a branch that writes the key back out as a string and
so accept either.  Interning unconditionally is what makes the two behave
alike.

TIME is passed on explicitly rather than left to default.  The resolver
decodes a nil time in C, reading the system clock directly, where a
caller holding an instant of its own — the matrix stepping through a
range, a test with the clock stopped — means that one.

`org-extend-today-until' is held at zero throughout.  A billing period is
a stretch of calendar days, and a boundary sitting three hours off
midnight while the file is still named for a calendar month is a
disagreement nothing downstream can see.

Nothing is refused here.  `untilnow' resolves without a start and is
passed on with it missing, because whether an open-ended period is usable
is a question for whoever asked for one."
  (if (consp spec)
      (list :kind 'interval :start (car spec) :end (cdr spec))
    (let* ((key (intern (format "%s" spec)))
           (org-extend-today-until 0)
           (range (org-clock-special-range key (or time (current-time))
                                           nil wstart mstart)))
      (list :kind (org-clock-projects--period-kind key)
            :start (nth 0 range)
            :end (nth 1 range)))))

(defun org-clock-projects--midnight (time)
  "Return the midnight beginning TIME's calendar day."
  (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_) (decode-time time)))
    (org-encode-time 0 0 0 d m y)))

(defun org-clock-projects--next-midnight (time)
  "Return the midnight beginning the day after TIME's."
  (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_) (decode-time time)))
    (org-encode-time 0 0 0 (1+ d) m y)))

(defun org-clock-projects--previous-day (time)
  "Return noon on the calendar day before TIME's.
Noon rather than midnight so that a day an hour shorter for daylight
saving cannot carry the result back into the day before that."
  (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_) (decode-time time)))
    (org-encode-time 0 0 12 (1- d) m y)))

(defun org-clock-projects--period-label (period)
  "Return the name PERIOD goes by in a file name.
A month, a quarter, a year and a week are named as they are written at
the prompt; anything else is named by its two dates.  A week takes the
ISO week-year rather than the calendar year of the day it opens on,
because the first ISO week of a year can begin in the year before it and
`2025-W01' names a week nobody has.

The end date of an interval is the last day it covers rather than the
exclusive bound, so a range picked as the 2nd to the 16th is named for
the 2nd and the 16th."
  (let ((start (plist-get period :start))
        (end (plist-get period :end)))
    (pcase (plist-get period :kind)
      (`month (format-time-string "%Y-%m" start))
      (`year (format-time-string "%Y" start))
      (`week (format-time-string "%G-W%V" start))
      (`quarter (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time start)))
                  (format "%d-Q%d" y (1+ (/ (1- m) 3)))))
      (_ (format "%s--%s"
                 (format-time-string "%Y-%m-%d" start)
                 (format-time-string
                  "%Y-%m-%d" (org-clock-projects--previous-day end)))))))

(defun org-clock-projects--read-range ()
  "Prompt for two dates and return the period lying between them.
Both picks are truncated to midnight and the end is carried forward a
day, so the day picked as the end is billed whole.  Org's own interactive
block takes the picker's answer as it comes; the picker answers with the
picked day at the current wall-clock time, so a range taken that way
begins after the morning of its first day and stops before the evening of
its last."
  (let ((start (org-read-date nil t nil "Range start (inclusive): "))
        (end (org-read-date nil t nil "Range end (inclusive): ")))
    (org-clock-projects--period
     (cons (org-clock-projects--midnight start)
           (org-clock-projects--next-midnight end)))))

(defun org-clock-projects--read-period ()
  "Prompt for a period and return its plist.
The relative keys are offered for completion and anything else may be
typed, which is why the prompt names the forms that are accepted: a
completion list announces nothing about free text.

Validation lives here rather than in the period core, which resolves
whatever it is handed.  A single day is refused because a bare date is
far more likely a slip for a month than a deliberate one-day invoice, and
refusing the kind refuses `today' and `yesterday' along with it.  A
period with no beginning — `untilnow' resolves to one — is refused
because no membership decision can be made against it."
  (let* ((answer (completing-read
                  "Period (or YYYY-MM, YYYY-QN, YYYY, YYYY-Www): "
                  org-clock-projects--period-candidates
                  nil nil nil nil "lastmonth"))
         (period (if (equal answer "range")
                     (org-clock-projects--read-range)
                   (org-clock-projects--period answer))))
    (when (eq (plist-get period :kind) 'day)
      (user-error "%s names a single day; export %s"
                  answer org-clock-projects--period-forms-help))
    (unless (plist-get period :start)
      (user-error "%s has no beginning; export %s"
                  answer org-clock-projects--period-forms-help))
    period))


;;;; Selecting entries
;;
;; Membership is decided on `YYYY-MM-DD HH:MM' strings rather than on time
;; values.  A clock element carries its start and its end as separate fields
;; already, and zero-padded they sort chronologically, so the question is two
;; string comparisons on data the parse produced anyway.
;;
;; That is what makes the period the primary gate, ahead of any parsing.  Only
;; the entries a period holds are handed to the CSV package's parser, which is
;; where the cost is: it builds each entry's whole inherited-property plist.
;; Gating on the tag first would decide nothing, because a project's tag sits
;; at the top of its file and every clock in the file inherits it.

(defun org-clock-projects--stamp (year month day hour minute)
  "Return YEAR MONTH DAY HOUR MINUTE as a `YYYY-MM-DD HH:MM' string.
Every field is padded to its full width, which is what lets two of these
be compared with `string<'.  A timestamp carrying no time of day is read
as midnight, the way Org reads one."
  (format "%04d-%02d-%02d %02d:%02d" year month day (or hour 0) (or minute 0)))

(defun org-clock-projects--time-stamp (time)
  "Return TIME in the form `org-clock-projects--stamp' writes.
A period's bounds and a clock's are written alike so that membership is
settled by comparing them directly."
  (format-time-string "%Y-%m-%d %H:%M" time))

(defun org-clock-projects--clock-start (element)
  "Return the stamp clock ELEMENT begins at."
  (let ((timestamp (org-element-property :value element)))
    (org-clock-projects--stamp
     (org-element-property :year-start timestamp)
     (org-element-property :month-start timestamp)
     (org-element-property :day-start timestamp)
     (org-element-property :hour-start timestamp)
     (org-element-property :minute-start timestamp))))

(defun org-clock-projects--clock-end (element)
  "Return the stamp clock ELEMENT ends at.
A clock still running has no end of its own, and Org reports its start
again, which places it at a single instant and outside every period whose
edge it sits on."
  (let ((timestamp (org-element-property :value element)))
    (org-clock-projects--stamp
     (org-element-property :year-end timestamp)
     (org-element-property :month-end timestamp)
     (org-element-property :day-end timestamp)
     (org-element-property :hour-end timestamp)
     (org-element-property :minute-end timestamp))))

(defun org-clock-projects--clock-closed-p (element)
  "Return non-nil when clock ELEMENT records a finished stretch of time.
This is exactly what `org-clock-csv' will take: a closed status and an
inactive range.  The two are asked together because a clock line without
its `=>  H:MM' tail parses as running however its timestamp reads, and
such a line is dropped from the CSV without a word."
  (and (eq (org-element-property :status element) 'closed)
       (eq (org-element-property :type (org-element-property :value element))
           'inactive-range)))

(defun org-clock-projects--clock-task (element)
  "Return the heading clock ELEMENT was clocked on, or nil above every heading."
  (let ((headline (org-element-lineage element '(headline))))
    (and headline (org-element-property :raw-value headline))))

(defun org-clock-projects--in-period-p (start end from until)
  "Return non-nil when a clock from START to END overlaps FROM until UNTIL.
All four are stamps, and the comparison is strict at both ends: an entry
ending exactly where a period begins belongs to the period before it
alone, one starting exactly there to the period after it alone, and an
entry of no length sitting on the boundary to neither."
  (and (string< start until)
       (string< from end)))

(defun org-clock-projects--matches-p (element matcher)
  "Return non-nil when MATCHER selects the entry clock ELEMENT sits in.
A nil MATCHER selects everything.  The tags are read at the element's own
position so that inheritance applies and a `#+FILETAGS:' tag written once
at the top of a project file reaches every clock beneath it.  The parsed
element carries the clocked headline's own tags alone, which for such a
file is nothing at all."
  (or (null matcher)
      (save-excursion
        (goto-char (org-element-property :begin element))
        (and (funcall matcher nil (org-get-tags) nil) t))))

(defun org-clock-projects--map-clocks (files function)
  "Return every non-nil value FUNCTION returns over the clock elements of FILES.
FUNCTION is called with a clock element, the file holding it, and that
file's `#+TITLE' and `#+CATEGORY' — the two file-level defaults the CSV
package's parser needs, read once per file rather than once per clock.
The file's buffer is current and widened throughout, so a caller may ask
Org anything that depends on a clock's position, tag inheritance
included.

Clocks are read out of a parse of the buffer rather than by searching for
clock lines, because whether a clock is closed and whether its timestamp
is a range are what decide if it records finished time, and only the
parser answers those.

`org-clock-csv--get-org-data' is read directly, as is the parser in
`org-clock-projects--entries'.  That package is unmaintained and offers
no filtering interface of any kind — its only knobs are the header string
and the row formatter — so depending on exactly these two internals is a
deliberate and recorded decision rather than an oversight."
  (let (collected)
    (dolist (file files)
      (with-current-buffer (org-clock-projects--org-buffer file)
        (org-with-wide-buffer
         (let* ((ast (org-element-parse-buffer))
                (title (org-clock-csv--get-org-data 'TITLE ast file))
                (category (org-clock-csv--get-org-data 'CATEGORY ast "")))
           (setq collected
                 (nconc collected
                        (org-element-map ast 'clock
                          (lambda (element)
                            (funcall function element file title category)))))))))
    collected))

(defun org-clock-projects--export-files (record wide)
  "Return the existing files a scan of RECORD reads.
By default that is the project's own file together with the archives Org
resolves for it, which follow `org-archive-location', honour a subtree's
own `:ARCHIVE:' property, and are dropped when they do not exist — a
project that has never been archived is not a special case.  With WIDE
non-nil the scan reaches every agenda file and its archives instead,
which is the way to find work carrying the project's tag but filed
somewhere else.

Files that do not exist are dropped, because the CSV package asserts the
existence of every file it is handed."
  (seq-filter
   #'file-exists-p
   (org-add-archive-files
    (if wide
        (org-agenda-files t)
      (delq nil (list (org-clock-projects--truename
                       (org-clock-projects--record-file record))))))))

(defun org-clock-projects--entries (files match period)
  "Return the clock entries FILES hold for PERIOD, oldest first.
MATCH is a tag match as `org-make-tags-matcher' reads one, or nil to take
every entry the period holds.

The entries are the CSV package's own plists, carrying the file and the
position of their clock line besides, so that a finding can name the line
it is about and take a reader there.  The rows are sorted by start time
because a CSV read by eye, and diffed against the one exported last
month, has to be in the order the work happened in."
  (let ((from (org-clock-projects--time-stamp (plist-get period :start)))
        (until (org-clock-projects--time-stamp (plist-get period :end)))
        (matcher (and match (cdr (org-make-tags-matcher match)))))
    (sort (org-clock-projects--map-clocks
           files
           (lambda (element file title category)
             (and (org-clock-projects--clock-closed-p element)
                  (org-clock-projects--in-period-p
                   (org-clock-projects--clock-start element)
                   (org-clock-projects--clock-end element)
                   from until)
                  (org-clock-projects--matches-p element matcher)
                  (let ((entry (org-clock-csv--parse-element
                                element title category)))
                    (and entry
                         (append (list :file file
                                       :position (org-element-property
                                                  :begin element))
                                 entry))))))
          (lambda (a b) (string< (plist-get a :start) (plist-get b :start))))))

(defun org-clock-projects--entry-minutes (entry)
  "Return the minutes ENTRY runs for.
Measured between the two timestamps the row carries rather than read off
the clock line's own total, so that a check judges exactly what the
export reports."
  (/ (- (org-time-string-to-seconds (plist-get entry :end))
        (org-time-string-to-seconds (plist-get entry :start)))
     60))


;;;; Exporting
;;
;; Entries are written whole and the exporter never clips.  The CSV is a
;; transcript of the CLOCK lines, which is what lets an invoice be checked
;; against the Org file line by line; the price is that an entry straddling a
;; period's edge appears in both adjacent exports, and trimming it to a
;; billing period belongs downstream with whoever renders that invoice.

(defcustom org-clock-projects-export-directory "~/"
  "Directory the exported CSV files are written to.
It is created if it does not exist."
  :type 'directory)

(defcustom org-clock-projects-export-file-function
  #'org-clock-projects-export-default-file-name
  "Function naming the file one export lands in.
It is called with the project's record and the period plist, and returns
a bare file name, which is expanded against
`org-clock-projects-export-directory'.  The period is handed over whole
because its kind is what tells a quarter from an arbitrary range that
happens to fall on the same two days, which its bounds alone cannot."
  :type 'function)

(defun org-clock-projects-export-default-file-name (record period)
  "Return the bare file name RECORD's PERIOD export takes.
`<period>-org-clock-<file-base>.csv'.  The tail is not decoration: the
downstream invoice build derives the same name from its own period, and a
month's export has to match it exactly for the two to meet."
  (format "%s-org-clock-%s.csv"
          (org-clock-projects--period-label period)
          (file-name-base (org-clock-projects--record-file record))))

(defun org-clock-projects--export-path (record period)
  "Return the path RECORD's PERIOD export is written to."
  (expand-file-name (funcall org-clock-projects-export-file-function
                             record period)
                    org-clock-projects-export-directory))

(defun org-clock-projects--write (path entries)
  "Write ENTRIES to PATH as CSV, and return how many rows it holds.
The header and each row come from `org-clock-csv-header' and
`org-clock-csv-row-fmt', so anyone who has arranged their columns keeps
them and this stays one shape of file rather than becoming a second one.
An empty period still leaves a header behind: downstream tooling looks
the file up by name, and a quiet month should read as zero rows rather
than as a broken build."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert org-clock-csv-header "\n")
    (dolist (entry entries)
      (insert (funcall org-clock-csv-row-fmt entry) "\n")))
  (length entries))

(defun org-clock-projects--read-record ()
  "Read one of the registered projects and return its record.
The candidates are the registry's, not the projects clocked into this
session: a period is invoiced for whether or not it was worked on today.
Each is offered under the name it is registered by, used exactly as it
was given.  The qualification `org-clock-projects-name' performs is for
names *derived* from a path, which can collide by accident; a name
somebody chose is theirs."
  (let ((candidates (mapcar (lambda (record)
                              (cons (org-clock-projects--record-name record)
                                    record))
                            (org-clock-projects--records))))
    (unless candidates
      (user-error "No projects are registered; see `org-clock-projects-projects-function'"))
    (cdr (assoc (completing-read "Project: " candidates nil t) candidates))))


;;;; Checks
;;
;; A finding is a plist of `:kind', `:file', `:position' and `:summary'.  The
;; export raises on the two kinds that corrupt an invoice by themselves — time
;; the CSV cannot carry, and time it would carry twice — and the check renders
;; all four.  One judgement, made once, that the two commands differ only in
;; what they do with.

(defcustom org-clock-projects-duration-threshold 10
  "Hours above which a single clock entry is reported as questionable.
The guard is on the length of an entry rather than on the period's edges.
A boundary check catches a forgotten clock-out only when it happens to
straddle one, where a stretch no working day reaches is visible wherever
in the period it falls."
  :type 'number)

(defun org-clock-projects--finding (kind file position summary)
  "Return a finding of KIND about POSITION in FILE, described by SUMMARY."
  (list :kind kind :file file :position position :summary summary))

(defun org-clock-projects--open-clock-findings (files period)
  "Return a finding for every clock in FILES still running when PERIOD ended.
This one cannot be left to anything downstream: `org-clock-csv' takes
closed clocks and says nothing about the rest, so an open clock's time
goes missing from the invoice with no trace of it in the output.  It
cannot be clipped either, having no end, which is why a clock opened
before the period is reported as readily as one opened inside it.  A
clock opened after the period closed can hold none of its time and is
left alone — invoicing an old month while clocked on today's work is the
ordinary case."
  (let ((until (org-clock-projects--time-stamp (plist-get period :end))))
    (org-clock-projects--map-clocks
     files
     (lambda (element file _title _category)
       (let ((start (org-clock-projects--clock-start element)))
         (and (not (org-clock-projects--clock-closed-p element))
              (string< start until)
              (org-clock-projects--finding
               'open-clock file (org-element-property :begin element)
               (format "%s: %s has been running since %s"
                       (file-name-nondirectory file)
                       (or (org-clock-projects--clock-task element) "a clock")
                       start))))))))

(defun org-clock-projects--overlap-findings (entries)
  "Return a finding for each of ENTRIES that covers minutes another already does.
Two projects clocked at once is what holding several clocks is for and is
never reported; two entries of one project covering the same minutes bill
them twice.  Only entries one project selected are ever compared, so the
question never arises between projects.

ENTRIES arrive oldest first, so an entry overlaps an earlier one exactly
when it begins before the furthest end reached so far, and one pass finds
every such pair.  The later entry is the one reported: it is the one
whose start landed inside a stretch already accounted for."
  (let (findings furthest)
    (dolist (entry entries)
      (when (and furthest
                 (string< (plist-get entry :start) (plist-get furthest :end)))
        (push (org-clock-projects--finding
               'overlap (plist-get entry :file) (plist-get entry :position)
               (format "%s: %s starting %s runs alongside %s"
                       (file-name-nondirectory (plist-get entry :file))
                       (plist-get entry :task)
                       (plist-get entry :start)
                       (plist-get furthest :task)))
              findings))
      (when (or (null furthest)
                (string< (plist-get furthest :end) (plist-get entry :end)))
        (setq furthest entry)))
    (nreverse findings)))

(defun org-clock-projects--long-entry-findings (entries)
  "Return a finding for each of ENTRIES longer than the duration threshold.
`org-clock-projects-duration-threshold' is in hours, and an entry of
exactly that length passes."
  (let ((limit (* 60 org-clock-projects-duration-threshold)))
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((minutes (org-clock-projects--entry-minutes entry)))
               (when (> minutes limit)
                 (org-clock-projects--finding
                  'long-entry (plist-get entry :file) (plist-get entry :position)
                  (format "%s: %s ran %s from %s"
                          (file-name-nondirectory (plist-get entry :file))
                          (plist-get entry :task)
                          (org-duration-from-minutes minutes 'h:mm)
                          (plist-get entry :start))))))
           entries))))

(defun org-clock-projects--file-tagged-p (file tag)
  "Return non-nil when FILE declares TAG for every entry it holds.
The export finds a project's time by inherited tag, so a file of the
project's own that does not declare the tag exports nothing and gives no
reason why.

Only a file-level tag makes every entry inherit one, so a tag worn by
individual headlines does not count.  Accepting one would call a file
clean while every entry outside those subtrees stayed unreachable, which
is the silent under-export this check exists to catch."
  (with-current-buffer (org-clock-projects--org-buffer file)
    (org-with-wide-buffer
     (and (member tag org-file-tags) t))))

(defun org-clock-projects--missing-tag-findings (record)
  "Return a finding for each of RECORD's own files that does not carry its tag.
Always the project's own file and its existing archives, whatever scope
the scan was given.  Widening the scan is how work tagged for this
project but filed elsewhere is found, so the agenda files it reaches are
not expected to carry the tag, and asking the question of them would bury
the one file that should under every file that never claimed to.  A
record with no tag has nothing that could be missing."
  (let ((tag (org-clock-projects--record-tag record)))
    (when tag
      (delq nil
            (mapcar
             (lambda (file)
               (unless (org-clock-projects--file-tagged-p file tag)
                 (org-clock-projects--finding
                  'missing-tag file 1
                  (format "%s carries no \"%s\" anywhere, so the export reads past it"
                          (file-name-nondirectory file) tag))))
             (org-clock-projects--export-files record nil))))))

(defun org-clock-projects--findings (record files period entries)
  "Return everything questionable about RECORD's PERIOD in FILES and ENTRIES.
FILES is the set that was scanned and ENTRIES what the period and the tag
selected out of it.  Four things are looked for: a clock still running, a
pair of selected entries covering the same minutes, an entry longer than
`org-clock-projects-duration-threshold' hours, and one of RECORD's own
files that does not carry RECORD's tag."
  (append (org-clock-projects--open-clock-findings files period)
          (org-clock-projects--overlap-findings entries)
          (org-clock-projects--long-entry-findings entries)
          (org-clock-projects--missing-tag-findings record)))

(defun org-clock-projects--raise (findings)
  "Stop, counting FINDINGS by kind, when there is anything in them.
Nothing has been written by the time this runs, because a half-written
export is exactly the thing that would be believed."
  (when findings
    (user-error
     "Nothing exported: %s.  `org-clock-projects-check' shows them; a double prefix exports anyway"
     (mapconcat
      (lambda (kind)
        (let ((n (seq-count (lambda (finding) (eq kind (plist-get finding :kind)))
                            findings)))
          (format "%d %s finding%s" n kind (if (= n 1) "" "s"))))
      (seq-uniq (mapcar (lambda (finding) (plist-get finding :kind)) findings))
      " and "))))

(defconst org-clock-projects--check-buffer "*org-clock-projects-check*"
  "Buffer the check renders its report into.")

(defun org-clock-projects--visit-finding (button)
  "Go to the place BUTTON's finding is about.
The target buffer is made current rather than merely shown: the next
thing a reader does is repair the clock line, and a report that only
displayed it would leave them typing into the report."
  (pop-to-buffer (org-clock-projects--org-buffer
                  (button-get button 'org-clock-projects-file)))
  (widen)
  (goto-char (button-get button 'org-clock-projects-position))
  (org-fold-show-context))

(defun org-clock-projects--insert-finding (finding)
  "Insert FINDING as one line that goes to the place it is about.
The kind leads the line because it is the one thing every finding has to
say, and the line is a button whole, so a reader can act on it from
anywhere along it."
  (insert-text-button
   (format "%s  %s" (plist-get finding :kind) (plist-get finding :summary))
   'action #'org-clock-projects--visit-finding
   'org-clock-projects-file (plist-get finding :file)
   'org-clock-projects-position (plist-get finding :position)
   'follow-link t
   'help-echo "Go to this clock line")
  (insert "\n"))

(defun org-clock-projects--render-findings (record period findings)
  "Render FINDINGS about RECORD's PERIOD into the report buffer and show it.
Every finding is one line and one button and the prose around them is
neither, so a reader can take each finding to the line it names.  A run
that turned nothing up says as much, because an empty buffer and a
command that quietly did nothing read alike."
  (let ((buffer (get-buffer-create org-clock-projects--check-buffer)))
    (with-current-buffer buffer
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s over %s: %s\n\n"
                        (org-clock-projects--record-name record)
                        (org-clock-projects--period-label period)
                        (if findings
                            (format "%d to look at" (length findings))
                          "nothing to look at")))
        (dolist (finding findings)
          (org-clock-projects--insert-finding finding))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))


;;;; The export and check commands

(defun org-clock-projects--wide-p (arg)
  "Return non-nil when prefix ARG asks for the scan to reach the agenda files."
  (memq (prefix-numeric-value arg) '(4 64)))

;;;###autoload
(defun org-clock-projects-export (&optional arg)
  "Export a project's clock entries for a period to CSV.
The rows are the CLOCK lines themselves, written whole and oldest first,
so an invoice can be checked against the Org file line by line.  An entry
straddling the period's edge therefore appears in both adjacent exports;
trimming it to a billing period is the job of whoever renders the
invoice.

ARG both widens the scan and waives the checks:

  none             the project's own files, both checks
  \\[universal-argument]              every agenda file, both checks
  \\[universal-argument] \\[universal-argument]          the project's own files, neither check
  \\[universal-argument] \\[universal-argument] \\[universal-argument]      every agenda file, neither check

Waiving is unsafe by construction, so it is deliberately the harder
gesture, and it waives both checks rather than one: reproducing an
invoice already issued over data known to be bad is the reason to have
the override at all."
  (interactive "P")
  (let* ((wide (org-clock-projects--wide-p arg))
         (waived (memq (prefix-numeric-value arg) '(16 64)))
         (record (org-clock-projects--read-record))
         (period (org-clock-projects--read-period))
         (files (org-clock-projects--export-files record wide))
         (entries (org-clock-projects--entries
                   files (org-clock-projects--record-tag record) period)))
    (unless waived
      (org-clock-projects--raise
       (seq-filter (lambda (finding)
                     (memq (plist-get finding :kind) '(open-clock overlap)))
                   (org-clock-projects--findings record files period entries))))
    (let* ((path (org-clock-projects--export-path record period))
           (rows (org-clock-projects--write path entries)))
      (message "Exported %d row%s to %s"
               rows (if (= rows 1) "" "s") (abbreviate-file-name path)))))

;;;###autoload
(defun org-clock-projects-check (&optional arg)
  "Report questionable clock data for a project and period.
The same project and the same period as the export, and the same ARG
widening the scan, but nothing is written and nothing refused: every
finding is rendered into `*org-clock-projects-check*', one line each,
each of them going to what it is about.  This is the command to reach for
once an export has stopped, and the one to run before invoicing so that
everything questionable arrives at once rather than one error at a time."
  (interactive "P")
  (let* ((record (org-clock-projects--read-record))
         (period (org-clock-projects--read-period))
         (files (org-clock-projects--export-files
                 record (org-clock-projects--wide-p arg)))
         (entries (org-clock-projects--entries
                   files (org-clock-projects--record-tag record) period)))
    (org-clock-projects--render-findings
     record period
     (org-clock-projects--findings record files period entries))))


;;;; Rolling clock ranges
;;
;; These extend Org's `:block' vocabulary rather than this package's, so they
;; install when the file loads rather than with `org-clock-projects-mode'.  A
;; clocktable written months ago must keep resolving whether or not the mode is
;; on today.

(defconst org-clock-projects--last-periods-regexp
  "\\`last\\(day\\|week\\|semimonth\\|month\\|quarter\\|year\\)s-\\(.+\\)\\'"
  "Regexp matching a `last<unit>s-N' range keyword.
Group 1 is the unit; group 2 is the count exactly as it was written.
The units are the six `:step' accepts, so the two vocabularies match.")

(defconst org-clock-projects--last-periods-keys
  '((day . today)
    (week . thisweek)
    (month . thismonth)
    (quarter . thisq)
    (year . thisyear))
  "Org's own range keyword for the period of each unit currently in progress.
`semimonth' is absent because Org has no keyword for it.")

(defun org-clock-projects--last-periods-parse (key)
  "Return (UNIT . WRITTEN) when KEY is a `last<unit>s-N' keyword, else nil.
WRITTEN is the count as it was typed, which need not be a number: the
shift guard needs to recognise a malformed count as one of these
keywords, so parsing stays separate from validating it.  Org's singular
`lastweek' is a shift to the previous week and does not match here; only
the plural span form does."
  (let ((skey (format "%s" key)))
    (when (string-match org-clock-projects--last-periods-regexp skey)
      (cons (intern (match-string 1 skey)) (match-string 2 skey)))))

(defun org-clock-projects--last-periods-parse-count (written key)
  "Return WRITTEN, the count typed in range keyword KEY, as a number.
N counts periods rather than offsets, so the smallest window is one."
  (unless (string-match-p "\\`[0-9]+\\'" written)
    (user-error "Time block %s counts periods, so N must be a whole number" key))
  (let ((n (string-to-number written)))
    (when (= n 0)
      (user-error "Time block %s counts periods, so N must be at least 1" key))
    n))

(defun org-clock-projects--semimonth-index (time)
  "Return the number of the semimonth holding TIME.
Numbering the halves of every month consecutively is what lets a window
step back over month and year boundaries without a special case at each."
  (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_) (decode-time time)))
    (+ (* 24 y) (* 2 (1- m)) (if (< d 16) 0 1))))

(defun org-clock-projects--semimonth-start (index)
  "Return the time at which the semimonth numbered INDEX begins."
  (let ((half (mod index 24)))
    (org-encode-time 0 0 org-extend-today-until
                     (if (cl-evenp half) 1 16)
                     (1+ (/ half 2))
                     (/ index 24))))

(defun org-clock-projects--last-periods-range (unit count time wstart mstart)
  "Return the last COUNT whole UNIT periods ending with the one holding TIME.
The value is a list of the window's start and end times.  WSTART and
MSTART are the week and month start days.

Every unit but `semimonth' resolves through Org itself, so the window
lands on exactly the boundaries of the reports it is read alongside.
Org has no semimonth keyword to borrow, so that unit counts halves of
months directly, and it ignores MSTART: `:step semimonth' always splits
a month on the 1st and the 16th, so a window honouring MSTART would open
somewhere its own steps never land."
  (if (eq unit 'semimonth)
      (let ((index (org-clock-projects--semimonth-index time)))
        (list (org-clock-projects--semimonth-start (- index (1- count)))
              (org-clock-projects--semimonth-start (1+ index))))
    (let ((key (alist-get unit org-clock-projects--last-periods-keys)))
      ;; The window opens where the shift COUNT-1 periods back opens, and closes
      ;; where the period in progress closes.  Org reads a shift off the end of
      ;; a string but dispatches the plain keyword on a symbol.
      (list (car (org-clock-special-range
                  (format "%s-%d" key (1- count)) time nil wstart mstart))
            (nth 1 (org-clock-special-range key time nil wstart mstart))))))

(defun org-clock-projects--clock-special-range (orig key &optional time as-strings
                                                     wstart mstart)
  "Resolve a `last<unit>s-N' KEY, delegating every other KEY to ORIG.

KEY names the last N whole periods of one of the six units `:step'
accepts, ending with and including the period currently in progress.
So `lastweeks-1' is the current week alone, and equals `thisweek', while
`lastweeks-8' is that week together with the seven before it.  Measuring
the window in the same unit as its boundaries is what keeps every period
in it whole.

Every clock report funnels through this function, which is what makes
the keywords work in a plain clocktable, a stepped clocktable and a
clockmatrix alike.  TIME, AS-STRINGS, WSTART and MSTART keep the
meanings Org gives them."
  (pcase (org-clock-projects--last-periods-parse key)
    (`nil (funcall orig key time as-strings wstart mstart))
    (`(,unit . ,written)
     (let* ((count (org-clock-projects--last-periods-parse-count written key))
            (range (org-clock-projects--last-periods-range
                    unit count time wstart mstart))
            (text (format "the last %d %s%s" count unit (if (= count 1) "" "s"))))
       (if (not as-strings)
           (append range (list text))
         (let ((fmt (org-time-stamp-format 'with-time)))
           (list (format-time-string fmt (car range))
                 (format-time-string fmt (nth 1 range))
                 text)))))))

(defun org-clock-projects--clocktable-shift-guard (&rest _)
  "Refuse to shift a clocktable whose `:block' spans a count of periods.
Org's shift command matches the digits of `lastweeks-8' as though they
were a bare year and rewrites the block to `9', destroying the parameter
with nothing left in the buffer to recover it from.  Its own fallback
already turns away the blocks it cannot shift, `untilnow' among them;
these join that set, under the same message, so the refusal is
indistinguishable from any other."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (and (looking-at
                "^[ \t]*#\\+BEGIN:[ \t]+clocktable\\>.*?:block[ \t]+\\(\\S-+\\)")
               (org-clock-projects--last-periods-parse (match-string 1)))
      (user-error "Cannot shift clocktable block"))))

(advice-add 'org-clock-special-range :around
            #'org-clock-projects--clock-special-range)
(advice-add 'org-clocktable-shift :before
            #'org-clock-projects--clocktable-shift-guard)


;;;; Clock matrix dynamic block
;;
;; Every cell is a sum Org itself computes, through `org-clock-get-table-data',
;; which clips an entry at the period boundaries it is handed.  The exporter
;; writes entries whole, so for any period holding an entry that straddles one
;; of its edges the two disagree by exactly the clipped share.  Both are right,
;; and the difference is deliberate: the matrix is a report and has to stay
;; comparable with the clocktable a reader would write beside it, while the CSV
;; is the evidence behind an invoice, whose clipping belongs downstream with
;; whoever renders that invoice.
;;
;; Like the range keywords, the block installs at load rather than with
;; `org-clock-projects-mode', so a document holding one keeps refreshing.

(defconst org-clock-projects--clockmatrix-step-headers
  '((day . "Day")
    (week . "Week")
    (semimonth . "Semimonth")
    (month . "Month")
    (quarter . "Quarter")
    (year . "Year"))
  "Label-column header for each `:step' a `clockmatrix' block accepts.")

(defun org-clock-projects--clockmatrix-time (value)
  "Resolve VALUE from a clockmatrix range specification into a time.
VALUE is an absolute day number as used by the agenda, or an Org
timestamp string.  Anything else, nil included, is an endpoint that never
resolved, and reports the same missing range an unset one does."
  (pcase value
    ((and (pred numberp) n)
     (pcase-let ((`(,m ,d ,y) (calendar-gregorian-from-absolute n)))
       ;; Midnight, as everywhere else in the matrix: a period is a stretch of
       ;; calendar days.
       (org-encode-time 0 0 0 d m y)))
    ((and (pred stringp) timestamp)
     (seconds-to-time (org-matcher-time timestamp)))
    (_ (user-error
        "Clockmatrix needs a range: set `:block', or both `:tstart' and `:tend'"))))

(defun org-clock-projects--clockmatrix-range (params)
  "Return the range PARAMS report on, as a cons of start and end times.
`:block' takes precedence over `:tstart' and `:tend', as in clocktable.
It resolves through the shared period core, so a matrix cell and an
exported CSV are cut on the same boundaries."
  (pcase (plist-get params :block)
    (`nil (cons (org-clock-projects--clockmatrix-time (plist-get params :tstart))
                (org-clock-projects--clockmatrix-time (plist-get params :tend))))
    (block
      (let ((period (org-clock-projects--period
                     block
                     (or (plist-get params :wstart) 1)
                     (or (plist-get params :mstart) 1))))
        ;; `untilnow' resolves without a start.  A matrix tiles its range into
        ;; periods, so an open-ended one is no range at all, and says so in the
        ;; words an absent range gets.
        (cons (or (plist-get period :start)
                  (org-clock-projects--clockmatrix-time nil))
              (plist-get period :end))))))

(defun org-clock-projects--clockmatrix-step-end (start step wstart mstart)
  "Return the end of the STEP period beginning at START.
WSTART and MSTART are the week and month start days.  The calendar
arithmetic follows Org's own stepped clocktables, and every step ends at
midnight: a report whose weeks opened at `org-extend-today-until' while
its months opened at midnight would disagree with itself about where a
period starts, and so would the CSV cut from the same range."
  (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y ,dow . ,_) (decode-time start)))
    (pcase step
      (`day (org-encode-time 0 0 0 (1+ d) m y))
      (`week
       (let ((offset (if (= dow wstart) 7 (mod (- wstart dow) 7))))
         (org-encode-time 0 0 0 (+ d offset) m y)))
      (`semimonth (org-encode-time 0 0 0
                                   (if (< d 16) 16 1)
                                   (if (< d 16) m (1+ m)) y))
      (`month (org-encode-time 0 0 0 mstart (1+ m) y))
      (`quarter (org-encode-time 0 0 0 mstart (+ 3 m) y))
      (`year (org-encode-time 0 0 0 1 1 (1+ y)))
      (_ (user-error "Unknown `:step' specification: %S" step)))))

(defun org-clock-projects--clockmatrix-periods (start end step wstart mstart)
  "Return the STEP periods tiling START to END, as (FROM . TO) conses.
The first and last period are clipped to the range, so the periods
partition it exactly.  WSTART and MSTART are the week and month start
days."
  (let (periods)
    (while (time-less-p start end)
      (let ((next (org-clock-projects--clockmatrix-step-end
                   start step wstart mstart)))
        ;; A `:wstart' outside 0-6 can land the next period on the current one,
        ;; which would loop forever.  Refuse rather than hang.
        (unless (time-less-p start next)
          (user-error "Clockmatrix `:step' %s does not advance past %s: \
check `:wstart' (0-6) and `:mstart' (1-31)"
                      step (format-time-string "%Y-%m-%d" start)))
        (push (cons start (if (time-less-p end next) end next)) periods)
        (setq start next)))
    (nreverse periods)))

(defun org-clock-projects--clockmatrix-label (start step)
  "Return the row label for the STEP period beginning at START.
A week is labelled by the date it starts on."
  (pcase step
    (`month (format-time-string "%Y-%m" start))
    (`year (format-time-string "%Y" start))
    (`quarter (pcase-let ((`(,_ ,_ ,_ ,_ ,m ,y . ,_) (decode-time start)))
                (format "%d-Q%d" y (1+ (/ (1- m) 3)))))
    (_ (format-time-string "%Y-%m-%d" start))))

(defun org-clock-projects--clockmatrix-own-files (record label)
  "Return the existing files holding time clocked for RECORD.
That is the project's own file together with whatever archives Org
resolves for it, from `org-archive-location' and from any `:ARCHIVE:'
property the file sets.  Asking Org is what keeps the report reading the
files the archive command actually writes to.

LABEL names the project in the warning a missing main file raises: that
is a misconfiguration rather than an absence of clocked time.  A missing
archive is silent, because most projects never accumulate one."
  (let ((main (expand-file-name (org-clock-projects--record-file record)
                                org-directory)))
    (if (file-exists-p main)
        (org-add-archive-files (list main))
      (warn "Clockmatrix: project %S has no file at %s, so it reports no time.  \
Set :file on its entry in the registry `org-clock-projects-projects-function' \
returns, or use `:scope agenda-with-archives'"
            label main)
      nil)))

(defun org-clock-projects--clockmatrix-record (tag)
  "Return the registered record carrying TAG, or one derived from TAG alone.
A `:tags' column names a project exactly as the registry does, so one
registered under a file name of its own is still read from that file; a
tag registered nowhere falls back to `<tag>.org'."
  (or (cl-find tag (org-clock-projects--records)
               :key #'org-clock-projects--record-tag :test #'equal)
      (org-clock-projects--record (list :tag tag))))

(defun org-clock-projects--clockmatrix-column (record scope)
  "Return the column reporting RECORD's clocked time under SCOPE.
A column is a plist of the `:label' heading it, the `:files' to scan and
the `:match' picking RECORD's entries out of them.  SCOPE is nil for the
project's own files, or `agenda-with-archives'.

The match travels with the files rather than being derived once from
them: under `agenda-with-archives' every column scans the same agenda-wide
file set, and a column without a match of its own would sum every project
into itself.  A record with no tag has nothing to match on, so it is
headed by its file's base name and takes that file whole."
  (let* ((tag (org-clock-projects--record-tag record))
         (label (or tag (file-name-base
                         (org-clock-projects--record-file record)))))
    (list :label label
          :match tag
          :files (pcase scope
                   (`nil (org-clock-projects--clockmatrix-own-files record label))
                   (`agenda-with-archives
                    (org-add-archive-files (org-agenda-files t)))
                   (_ (user-error "Unknown `:scope' for clockmatrix: %S" scope))))))

(defun org-clock-projects--clockmatrix-columns (params)
  "Return the columns a clockmatrix block with PARAMS reports on.
They are the registered projects in registration order, or the projects
`:tags' names outright."
  (let ((tags (plist-get params :tags))
        (scope (plist-get params :scope)))
    (mapcar (lambda (record)
              (org-clock-projects--clockmatrix-column record scope))
            (if tags
                (mapcar #'org-clock-projects--clockmatrix-record tags)
              (org-clock-projects--records)))))

(defun org-clock-projects--clockmatrix-minutes (files match start end)
  "Return the minutes MATCH selects in FILES between START and END.
A nil MATCH takes every entry in FILES.  Delegating the sum to Org is
what makes a cell agree with the equivalent clocktable, and clips clocks
that cross START or END rather than double-counting or dropping them."
  (let ((params (list :maxlevel 0
                      :match match
                      :tstart (format-time-string (org-time-stamp-format t t) start)
                      :tend (format-time-string (org-time-stamp-format t t) end))))
    (cl-loop for file in files
             sum (with-current-buffer (or (find-buffer-visiting file)
                                          (find-file-noselect file))
                   (save-excursion
                     (save-restriction
                       (widen)
                       (or (nth 1 (org-clock-get-table-data file params)) 0)))))))

(defun org-clock-projects--clockmatrix-rows (columns periods step)
  "Return one row per period in PERIODS.
Each row is a cons of the period's label and the minutes each of COLUMNS
reports in it."
  (org-agenda-prepare-buffers
   (cl-remove-duplicates
    (apply #'append (mapcar (lambda (column) (plist-get column :files)) columns))
    :test #'equal))
  (mapcar (lambda (period)
            (cons (org-clock-projects--clockmatrix-label (car period) step)
                  (mapcar (lambda (column)
                            (org-clock-projects--clockmatrix-minutes
                             (plist-get column :files)
                             (plist-get column :match)
                             (car period) (cdr period)))
                          columns)))
          periods))

(defun org-clock-projects--clockmatrix-prune-columns (columns rows)
  "Drop the entries of COLUMNS with no clocked time anywhere in ROWS.
Return a cons of the surviving columns and the correspondingly narrowed
ROWS."
  (let ((kept (cl-loop for i from 0 below (length columns)
                       when (> (cl-loop for row in rows sum (nth i (cdr row))) 0)
                       collect i)))
    (cons (mapcar (lambda (i) (nth i columns)) kept)
          (mapcar (lambda (row)
                    (cons (car row) (mapcar (lambda (i) (nth i (cdr row))) kept)))
                  rows))))

(defun org-clock-projects--clockmatrix-duration-format ()
  "Return `org-duration-format' with every unit above hours dropped.
Monthly totals read as 108:30 rather than 4d 12:30, while the rest of
the configured format is left alone."
  (if (not (consp org-duration-format))
      org-duration-format
    (or (cl-remove-if (lambda (entry)
                        (let ((unit (car entry)))
                          (and (stringp unit)
                               (> (or (cdr (assoc unit org-duration-units)) 0) 60))))
                      org-duration-format)
        'h:mm)))

(defun org-clock-projects--clockmatrix-cell (minutes)
  "Format MINUTES as a table cell, left blank when there is no time."
  (if (> minutes 0)
      (org-duration-from-minutes
       minutes (org-clock-projects--clockmatrix-duration-format))
    ""))

(defun org-clock-projects--clockmatrix-row-total (row)
  "Return the total minutes in ROW, a label followed by per-project minutes."
  (apply #'+ (cdr row)))

(defun org-clock-projects--clockmatrix-insert-row (cells)
  "Insert CELLS as one Org table row."
  (insert "| " (mapconcat #'identity cells " | ") " |\n"))

(defun org-dblock-write:clockmatrix (params)
  "Write a `clockmatrix' dynamic block according to PARAMS.
Render clocked time as a matrix with calendar periods down the rows and
projects across the columns, totalled on both axes.  Columns default to
the registered projects; one with no time anywhere in the range is
dropped.  See the README for the full parameter list."
  (let* ((step (or (plist-get params :step) 'month))
         (wstart (or (plist-get params :wstart) 1))
         (mstart (or (plist-get params :mstart) 1))
         (skip0 (plist-get params :stepskip0))
         (show-total (if (plist-member params :total)
                         (plist-get params :total)
                       t))
         (header (or (alist-get step org-clock-projects--clockmatrix-step-headers)
                     (user-error "Unknown `:step' specification: %S" step)))
         (range (org-clock-projects--clockmatrix-range params))
         (periods (org-clock-projects--clockmatrix-periods
                   (car range) (cdr range) step wstart mstart))
         (columns (org-clock-projects--clockmatrix-columns params))
         (pruned (org-clock-projects--clockmatrix-prune-columns
                  columns
                  (org-clock-projects--clockmatrix-rows columns periods step)))
         (shown (car pruned))
         (rows (if skip0
                   (cl-remove-if (lambda (row)
                                   (= 0 (org-clock-projects--clockmatrix-row-total
                                         row)))
                                 (cdr pruned))
                 (cdr pruned)))
         (table-start (point)))
    (org-clock-projects--clockmatrix-insert-row
     (append (list header)
             (mapcar (lambda (column) (plist-get column :label)) shown)
             (and show-total (list "Total"))))
    (insert "|-\n")
    (dolist (row rows)
      (org-clock-projects--clockmatrix-insert-row
       (append (list (car row))
               (mapcar #'org-clock-projects--clockmatrix-cell (cdr row))
               (and show-total
                    (list (org-clock-projects--clockmatrix-cell
                           (org-clock-projects--clockmatrix-row-total row)))))))
    (when show-total
      (insert "|-\n")
      (org-clock-projects--clockmatrix-insert-row
       (append (list "Total")
               (cl-loop for i from 0 below (length shown)
                        collect (org-clock-projects--clockmatrix-cell
                                 (cl-loop for row in rows sum (nth i (cdr row)))))
               (list (org-clock-projects--clockmatrix-cell
                      (cl-loop for row in rows
                               sum (org-clock-projects--clockmatrix-row-total
                                    row)))))))
    (goto-char table-start)
    (org-table-align)))

;;;###autoload
(defun org-clock-projects-clockmatrix ()
  "Insert a `clockmatrix' dynamic block at point and render it."
  (interactive)
  (org-create-dblock (list :name "clockmatrix" :block 'thisyear :step 'month))
  (org-update-dblock))

(org-dynamic-block-define "clockmatrix" #'org-clock-projects-clockmatrix)


;;;; Installation

;;;###autoload
(define-minor-mode org-clock-projects-mode
  "Give Org one clock per project, with several open at once.

Until a project is created this changes nothing: clock lines are written
by Org's own primitives, no prompt is raised, and the modeline segment
stays empty."
  :global t
  :group 'org-clock-projects
  (if org-clock-projects-mode
      (progn
        (advice-add 'org-clock-in :around #'org-clock-projects--clock-in-advice)
        (advice-add 'org-clock-out :around #'org-clock-projects--clock-out-advice)
        ;; Whether to be asked at all stays the user's, through Org's own
        ;; option; this only changes which question gets asked.
        (when org-clock-ask-before-exiting
          (remove-hook 'kill-emacs-query-functions #'org-clock-kill-emacs-query)
          (add-hook 'kill-emacs-query-functions
                    #'org-clock-projects-kill-emacs-query))
        (unless (memq 'org-clock-projects-mode-line-string global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '(""))
                        '(org-clock-projects-mode-line-string))))
        (org-clock-projects--update-mode-line))
    (advice-remove 'org-clock-in #'org-clock-projects--clock-in-advice)
    (advice-remove 'org-clock-out #'org-clock-projects--clock-out-advice)
    (remove-hook 'kill-emacs-query-functions #'org-clock-projects-kill-emacs-query)
    ;; Hand the question back rather than leaving the session with none.
    (when org-clock-ask-before-exiting
      (add-hook 'kill-emacs-query-functions #'org-clock-kill-emacs-query))
    (setq global-mode-string
          (delq 'org-clock-projects-mode-line-string global-mode-string))))

(provide 'org-clock-projects)
;;; org-clock-projects.el ends here
