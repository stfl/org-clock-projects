;;; org-clock-projects.el --- Per-project parallel Org clocks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Stefan Lendl

;; Author: Stefan Lendl <s@stfl.dev>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: calendar, convenience
;; URL: https://github.com/stfl/org-clock-projects

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
;; The set of files that may become projects comes from
;; `org-clock-projects-files-function', which defaults to returning
;; `org-clock-projects-files'.  Point it at your own project registry to avoid
;; maintaining the same list twice.
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

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)
(require 'org-clock)

(defgroup org-clock-projects nil
  "Per-project parallel Org clocks."
  :group 'org-clock
  :prefix "org-clock-projects-")


;;;; Configuration

(defcustom org-clock-projects-files nil
  "Org files that may become clock projects.
Relative names are expanded against `org-directory'.  This is what the
default `org-clock-projects-files-function' returns."
  :type '(repeat file))

(defcustom org-clock-projects-files-function #'org-clock-projects-default-files
  "Function returning the files that may become clock projects.
It takes no arguments and returns a list of file names, absolute or
relative to `org-directory'.  Set it to your own project registry's
accessor to keep one list rather than two."
  :type 'function)

(defcustom org-clock-projects-continue-threshold 30
  "Minutes below which a project's clock-out time is resumed without asking.
Above it, clocking in on a project asks whether to start from that
project's stored clock-out time."
  :type 'integer)

(defun org-clock-projects-default-files ()
  "Return `org-clock-projects-files'."
  org-clock-projects-files)


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
  (when (and file (stringp file))
    (file-truename (expand-file-name file (or org-directory default-directory)))))

(defun org-clock-projects--candidate-files ()
  "Return the truenames of every file that may become a project."
  (delete-dups
   (delq nil (mapcar #'org-clock-projects--truename
                     (funcall org-clock-projects-files-function)))))

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

(defun org-clock-projects-name (project)
  "Return the display name of PROJECT."
  (file-name-base project))

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
  (cdr (cdr entry)))

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
        (dolist (clock (org-find-open-clocks file))
          (push (cons file clock) entries))))
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
        ;; dangling-clock prompt; Org binds this the same way in
        ;; `org-clock-resolve-clock'.
        (org-clock-auto-clock-resolution nil)
        (org-clock-projects--inhibit t))
    (org-clock-clock-in clock t)))

(defun org-clock-projects--clock-out (clock &optional at-time)
  "Clock out of CLOCK at AT-TIME, without prompting for a note.
With no AT-TIME, Org applies the configured rounding itself.
Clock-outs this package performs are bookkeeping rather than the end of a
task, and several of them are followed by more work in the same command,
so they use the binding Org itself uses in `org-clock-auto-clockout'.
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
            (format "⏱ %s%s"
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
     ;; A task selection resolves to a marker in another file, so the target
     ;; cannot be read off the current buffer.  Resolve it here and clock in
     ;; at that task, which re-enters with the right file current.
     ((equal select '(4))
      (let ((task (org-clock-select-task "Clock-in on task: ")))
        (unless task (error "Abort"))
        (org-with-point-at task (org-clock-in nil start-time))))
     ((not (org-clock-projects--live-p))
      ;; Dormant: stock Org, plus the session-wide continuation prompt.
      (let ((org-clock-continuously
             (if start-time
                 org-clock-continuously
               (or (org-clocking-p)
                   (org-clock-projects--continue-p org-clock-out-time)))))
        (apply orig-fn args)))
     (t (org-clock-projects--clock-in-scoped orig-fn args)))))

(defun org-clock-projects--clock-in-scoped (orig-fn args)
  "Run ORIG-FN, `org-clock-in', with ARGS, scoped to a project.
Crossing a project boundary asks two independent questions: whether to
stop the current project's clock, and what to do about the target file.
A start time supplied in ARGS is the caller's, and only the scope and the
one-clock-per-project invariant are decided here."
  (let* ((file (org-clock-projects--buffer-file))
         (scope (org-clock-projects--scope file))
         (parallel org-clock-projects--parallel)
         ;; Nothing here is committed until the clock-in itself succeeds.
         (previous-current org-clock-projects--current)
         (previous-projects org-clock-projects--projects)
         (committed nil)
         (target (cond
                  ;; `org-clock-projects-fork' has already settled the scope.
                  (parallel scope)
                  ((equal scope org-clock-projects--current)
                   org-clock-projects--current)
                  (t
                   ;; Question one: the clock that is running now.
                   (when (org-clocking-p)
                     (setq parallel
                           (not (y-or-n-p
                                 (format "Stop the clock running in %s? "
                                         (org-clock-projects--active-label))))))
                   ;; Question two: the file being clocked into.  Creating a
                   ;; project is only on offer when the file may become one.
                   (if (and (null scope) (org-clock-projects--candidate-p file))
                       (and (y-or-n-p (format "Create clock project %s? "
                                              (org-clock-projects-name file)))
                            (org-clock-projects--create file))
                     scope))))
         ;; One clock per project.
         (existing (org-clock-projects--scope-clocks target)))
    (when existing
      ;; More than one is damage rather than a state this package creates, so
      ;; the surplus is closed rather than assumed away.
      (if (cadr args)
          ;; The caller fixed when the new clock begins, so the clocks it
          ;; replaces have to end at that same moment.  Handing one to Org to
          ;; interrupt would end it at now instead, and the difference would
          ;; be billed to both.
          (dolist (clock existing)
            (org-clock-projects--clock-out-at clock (cadr args)))
        ;; The newest is handed to Org active, and Org's own interrupt
        ;; handling closes it, exactly as it would for any ordinary clock-in
        ;; within one file.
        (dolist (clock (butlast existing))
          (org-clock-projects--clock-out clock))
        (when (org-clock-projects--activate (car (last existing)) parallel)
          ;; Org must now stop the clock it has just been handed.
          (setq parallel nil))))
    (setq org-clock-projects--current target)
    (unwind-protect
        (prog1
            (cond
             ;; The caller fixed the start time, so it wins.  Leaving
             ;; `org-clock-continuously' ambient would let the clock-out Org
             ;; performs on the way in overwrite `org-clock-out-time' with now
             ;; and beat the caller's time to `org-clock-in's `or'.
             ((cadr args)
              ;; Same reasoning for a clock still running elsewhere: Org would
              ;; close it at now while this one starts earlier.
              (when (and (org-clocking-p) (not parallel))
                (org-clock-projects--clock-out-at
                 (cons org-clock-marker org-clock-start-time) (cadr args)))
              (let ((org-clock-continuously nil)
                    (org-clock-leftover-time nil)
                    (org-clock-resolving-clocks-due-to-idleness parallel))
                (apply orig-fn args)))
             ;; A clock is being stopped by this very clock-in, so the two are
             ;; contiguous and the new one starts exactly where that one ends.
             ;; There is no gap to misattribute — and letting Org compute a
             ;; fresh start instead would round it *down*, below the end just
             ;; rounded *up*, and bill one rounding interval to both.
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
              ;; target is, and only the target's own clock-out time can close
              ;; it.  `org-clock-continuously' cannot be used: `org-clock-out'
              ;; overwrites `org-clock-out-time' with now before `org-clock-in'
              ;; reads it, which is how time bridges between engagements.
              (let* ((out (org-clock-projects--out-time target))
                     (start (and (org-clock-projects--continue-p out) out))
                     (org-clock-continuously nil)
                     (org-clock-leftover-time nil)
                     (org-clock-resolving-clocks-due-to-idleness parallel))
                (funcall orig-fn (car args) start))))
          (setq committed t))
      (unless committed
        ;; A project created for a clock-in that never happened would suppress
        ;; the boundary prompt for that file ever after.
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
which only happens through damage — names each clock by its heading, so
the surplus can be picked out and closed."
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
      (if (y-or-n-p (format "Clock out the open clock in %s? " name))
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
  "Ask about each open clock when Emacs is killed.
This replaces `org-clock-kill-emacs-query', which only ever inspects the
active clock and so sees nothing when none is active.  Clocks left open
are still there after a restart, because open clocks are read back from
the files.  Always returns t, as `kill-emacs-query-functions' requires."
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
          (when (buffer-file-name) (save-buffer))))))
  t)


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
        (remove-hook 'kill-emacs-query-functions #'org-clock-kill-emacs-query)
        (add-hook 'kill-emacs-query-functions #'org-clock-projects-kill-emacs-query)
        (unless (memq 'org-clock-projects-mode-line-string global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '(""))
                        '(org-clock-projects-mode-line-string))))
        (org-clock-projects--update-mode-line))
    (advice-remove 'org-clock-in #'org-clock-projects--clock-in-advice)
    (advice-remove 'org-clock-out #'org-clock-projects--clock-out-advice)
    (remove-hook 'kill-emacs-query-functions #'org-clock-projects-kill-emacs-query)
    (setq global-mode-string
          (delq 'org-clock-projects-mode-line-string global-mode-string))))

(provide 'org-clock-projects)
;;; org-clock-projects.el ends here
