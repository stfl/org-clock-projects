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
  (delq nil (mapcar #'org-clock-projects--truename
                    (funcall org-clock-projects-files-function))))

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
The nil scope carries Org's own session-wide `org-clock-out-time', which
is what unscoped work continues from."
  (if project
      (cdr (assoc project org-clock-projects--projects))
    org-clock-out-time))

(defun org-clock-projects--set-out-time (file time)
  "Store TIME as the clock-out time of the project owning FILE."
  (let ((cell (assoc file org-clock-projects--projects)))
    (when cell (setcdr cell time))))


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
marker and a start time.  At most one clock is reported per file, the
active one when there is a choice.  The active clock is included even
when its file is not a project candidate, so an unscoped clock is never
invisible."
  (let (entries)
    (dolist (file (org-clock-projects--candidate-files))
      (when (file-exists-p file)
        (let* ((clocks (org-find-open-clocks file))
               ;; `org-find-open-clocks' pushes, so the list runs backwards
               ;; through the file; the last element is the first clock in it.
               (clock (or (seq-find #'org-clock-projects--active-clock-p clocks)
                          (car (last clocks)))))
          (when clock (push (cons file clock) entries)))))
    (when (org-clock-is-active)
      (let ((file (org-clock-projects--buffer-file (org-clocking-buffer))))
        (when (and file (not (assoc file entries)))
          (push (cons file (cons (copy-marker org-clock-marker t)
                                 org-clock-start-time))
                entries))))
    (sort entries (lambda (a b)
                    (time-less-p (org-clock-projects--entry-start a)
                                 (org-clock-projects--entry-start b))))))

(defun org-clock-projects--scope-clock (scope)
  "Return the open clock whose file belongs to SCOPE, or nil.
SCOPE is a project or nil, nil meaning unscoped work."
  (org-clock-projects--entry-clock
   (seq-find (lambda (entry)
               (equal scope (org-clock-projects--scope
                             (org-clock-projects--entry-file entry))))
             (org-clock-projects-open-clocks))))

(defun org-clock-projects--file-clock (file)
  "Return the open clock in FILE, or nil."
  (org-clock-projects--entry-clock
   (assoc file (org-clock-projects-open-clocks))))

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

(defun org-clock-projects--activate (clock &optional parallel)
  "Make CLOCK Org's active clock.
This is Org's own clock-in with resume enabled: it reads the start time
back out of the existing clock line, restores effort and accumulated
total, and writes no new clock line.  With PARALLEL non-nil the clock
that was active is left open rather than stopped."
  (unless (org-clock-projects--active-clock-p clock)
    (let ((org-clock-resolving-clocks-due-to-idleness parallel)
          ;; Resuming a clock this package located must never raise Org's
          ;; dangling-clock prompt; Org binds this the same way in
          ;; `org-clock-resolve-clock'.
          (org-clock-auto-clock-resolution nil)
          (org-clock-projects--inhibit t))
      (org-clock-clock-in clock t))))

(defun org-clock-projects--clock-out (clock)
  "Clock out of CLOCK without prompting for a note.
Clock-outs this package performs are bookkeeping rather than the end of a
task, and several of them are followed by more work in the same command,
so they use the binding Org itself uses in `org-clock-auto-clockout'.
Passing no end time leaves the configured rounding to Org.

This is `org-clock-clock-out' with `org-clock-projects--active-clock-p'
in place of `org-is-active-clock': the two differ exactly when another
project's clock sits at the same character offset, and there the stock
predicate sends the clock-out to the wrong file."
  (let ((org-log-note-clock-out nil)
        (org-clock-out-switch-to-state nil))
    (if (org-clock-projects--active-clock-p clock)
        (org-clock-out nil t)
      (let ((temp (copy-marker (car clock) (marker-insertion-type (car clock)))))
        (org-with-clock clock (org-clock-out nil t))
        (setcar clock temp)))))


;;;; Continuation
;;
;; Continuation is scoped to a project rather than to the session, so clocking
;; out of one engagement and into another never backdates the new clock to the
;; other engagement's clock-out.

(defun org-clock-projects--minutes-ago (time)
  "Return the whole minutes elapsed since TIME."
  (/ (org-time-convert-to-integer (time-subtract (org-current-time) time)) 60))

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
     ;; Org turns this into a second `org-clock-in' carrying an explicit start
     ;; time, and that inner call is the one to scope.
     ((equal select '(64)) (apply orig-fn args))
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
         ;; One clock per project: make the target's own open clock active so
         ;; that Org's interrupt handling closes it, exactly as it would for any
         ;; ordinary clock-in within one file.
         (existing (org-clock-projects--scope-clock target)))
    (when (and existing (not (org-clock-projects--active-clock-p existing)))
      (org-clock-projects--activate existing parallel)
      (setq parallel nil))
    (setq org-clock-projects--current target)
    (prog1
        (cond
         ;; The caller fixed the start time; only the scope was ours to decide.
         ((cadr args)
          (let ((org-clock-leftover-time nil)
                (org-clock-resolving-clocks-due-to-idleness parallel))
            (apply orig-fn args)))
         ;; A clock about to be stopped by ORIG-FN that belongs to the target
         ;; hands its time straight over.  One belonging to another project
         ;; must not: `org-clock-out' overwrites `org-clock-out-time' with now
         ;; before ORIG-FN reads it, which is how time bridges between
         ;; engagements.  Passing the target's own stored time explicitly
         ;; cannot bridge.
         ((and (org-clocking-p)
               (not parallel)
               (equal target (org-clock-projects--scope
                              (org-clock-projects--buffer-file
                               (org-clocking-buffer)))))
          (let ((org-clock-continuously t)
                (org-clock-leftover-time nil)
                (org-clock-resolving-clocks-due-to-idleness nil))
            (apply orig-fn args)))
         (t
          (let* ((out (org-clock-projects--out-time target))
                 (start (and (org-clock-projects--continue-p out) out))
                 (org-clock-continuously nil)
                 (org-clock-leftover-time nil)
                 (org-clock-resolving-clocks-due-to-idleness parallel))
            (funcall orig-fn (car args) start))))
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
    (let ((existing (org-clock-projects--file-clock file)))
      (when (and existing
                 (not (y-or-n-p
                       (format "%s already has an open clock; clock it out and start here? "
                               (org-clock-projects-name file)))))
        (user-error "Aborted"))
      (org-clock-projects--create file)
      (when existing (org-clock-projects--clock-out existing)))
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
            (org-duration-from-minutes
             (/ (org-time-convert-to-integer (time-subtract (current-time) start))
                60)
             'h:mm)
            (org-clock-projects--clock-heading
             (org-clock-projects--entry-clock entry)))))

(defun org-clock-projects--read-clock (entries)
  "Read one of the open-clock ENTRIES and return it.
Candidates are project names, so typing filters on those alone and cannot
accidentally match a duration or a timestamp.  The completion category is
private on purpose: annotation packages override annotations for
categories they recognise and defer only for ones they do not, so a
well-known name would silently lose these annotations."
  (let* ((candidates (mapcar (lambda (entry)
                               (cons (org-clock-projects-name
                                      (org-clock-projects--entry-file entry))
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
  (let ((unscoped (org-clock-projects--scope-clock nil)))
    (when unscoped (org-clock-projects--activate unscoped t)))
  (org-clock-projects--update-mode-line))

(defun org-clock-projects--clear-1 (project)
  "Forget PROJECT, deciding what becomes of its open clock."
  (let ((clock (org-clock-projects--file-clock project))
        (name (org-clock-projects-name project)))
    (when clock
      (if (y-or-n-p (format "Clock out the open clock in %s? " name))
          (org-clock-projects--clock-out clock)
        (when (org-clock-projects--scope-clock nil)
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
                  (format "Clock out %s (%s)? "
                          (org-clock-projects-name
                           (org-clock-projects--entry-file entry))
                          (org-clock-projects--clock-heading clock))))
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
