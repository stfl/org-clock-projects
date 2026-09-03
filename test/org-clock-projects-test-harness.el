;;; org-clock-projects-test-harness.el --- Shared harness for the suites -*- lexical-binding: t; -*-

;; The frozen clock, the scripted answer queues, the sandbox and the fixture
;; writers are shared by every suite, so they live apart from the tests that
;; use them.  The runner loads each file under test/ by its path, which
;; bypasses `features' and loads a suite a second time the moment a sibling
;; requires it; ERT refuses a test it has already seen, so no file that
;; defines one may be required by another.  A file of helpers is safe to load
;; twice, and this is that file.

;;; Code:

;; `display-warning' is an autoload until something pulls in warnings.el, and
;; the warning capture below replaces its function cell.  Called while it is
;; still an autoload it loads warnings.el, which defines the real
;; `display-warning' over the replacement and takes the call, so the first
;; warning of the session escapes capture and only the first test to assert a
;; warning fails.  Which Emacs builds preload warnings.el varies, which makes
;; that a failure seen on one Emacs and not another.
(require 'warnings)
(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-clock-projects)

;;;; Clock

(defvar org-clock-projects-test--now nil
  "The frozen value of `current-time' inside `org-clock-projects-test-at'.")

(defmacro org-clock-projects-test-at (time &rest body)
  "Run BODY with `current-time' frozen at TIME.
Clock arithmetic is the whole subject here, so the clock has to hold
still: rounding, thresholds and durations are all read off it."
  (declare (indent 1) (debug t))
  `(let ((org-clock-projects-test--now ,time))
     (cl-letf (((symbol-function 'current-time)
                (lambda () org-clock-projects-test--now)))
       ,@body)))

(defun org-clock-projects-test-advance (minutes)
  "Move the frozen clock forward by MINUTES."
  (setq org-clock-projects-test--now
        (time-add org-clock-projects-test--now (seconds-to-time (* 60 minutes)))))

(defun org-clock-projects-test-time (&optional minutes-ago)
  "Return the frozen time, MINUTES-AGO minutes earlier if given."
  (if minutes-ago
      (time-subtract (current-time) (seconds-to-time (* 60 minutes-ago)))
    (current-time)))

(defun org-clock-projects-test-stamp (&optional minutes-ago)
  "Return the inactive Org timestamp for MINUTES-AGO minutes ago."
  (format-time-string (org-time-stamp-format 'with-time t)
                      (org-clock-projects-test-time minutes-ago)))

(defun org-clock-projects-test-hour (hour &optional minute)
  "Return today's HOUR:MINUTE as a time value."
  (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_) (decode-time)))
    (org-encode-time 0 (or minute 0) hour d m y)))

;;;; Scripted answers
;;
;; Several flows ask two or three questions in sequence, so a yes/no stub
;; returning a constant would answer them all the same way and hide a
;; regression.  The queue makes "how many questions does this flow ask, and in
;; what order" a tested property: a question the queue did not anticipate is an
;; error, and so is an answer that was never asked for.

(defvar org-clock-projects-test--answers nil)
(defvar org-clock-projects-test--asked nil)

(defun org-clock-projects-test--answer (prompt)
  "Answer PROMPT from the scripted queue, recording that it was asked."
  (push prompt org-clock-projects-test--asked)
  (unless org-clock-projects-test--answers
    (error "Unscripted question: %s" prompt))
  (pop org-clock-projects-test--answers))

(defmacro org-clock-projects-test-with-answers (answers &rest body)
  "Run BODY answering each yes/no question from ANSWERS, in order.
An empty ANSWERS asserts that BODY raises no question at all."
  (declare (indent 1) (debug t))
  `(let ((org-clock-projects-test--answers (copy-sequence ,answers))
         (org-clock-projects-test--asked nil))
     (cl-letf (((symbol-function 'y-or-n-p) #'org-clock-projects-test--answer)
               ((symbol-function 'yes-or-no-p) #'org-clock-projects-test--answer))
       (prog1 (progn ,@body)
         (when org-clock-projects-test--answers
           (error "Questions the flow never asked: %S"
                  org-clock-projects-test--answers))))))

(defun org-clock-projects-test-questions ()
  "Return the prompts raised so far, in the order they were asked."
  (reverse org-clock-projects-test--asked))

;;;; Completion

(defvar org-clock-projects-test--collection nil
  "The collection the last stubbed `completing-read' was handed.")

(defmacro org-clock-projects-test-with-choice (choice &rest body)
  "Run BODY with `completing-read' answering CHOICE.
CHOICE is evaluated when the prompt arrives.  A nil CHOICE asserts that
BODY never prompts for a completion."
  (declare (indent 1) (debug t))
  `(let ((org-clock-projects-test--collection nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &rest _)
                  (setq org-clock-projects-test--collection collection)
                  (or ,choice (error "Unexpected completion: %s" prompt)))))
       ,@body)))

(defun org-clock-projects-test-candidates ()
  "Return the candidates offered by the last stubbed completion, in order."
  (all-completions "" org-clock-projects-test--collection))

(defun org-clock-projects-test-metadata (key)
  "Return KEY from the completion metadata of the last stubbed completion."
  (alist-get key (cdr (funcall org-clock-projects-test--collection
                               "" nil 'metadata))))

(defun org-clock-projects-test-annotation (candidate)
  "Return the annotation the last stubbed completion gives CANDIDATE."
  (funcall (org-clock-projects-test-metadata 'annotation-function) candidate))

;;;; Captured output
;;
;; The sandbox binds `inhibit-message', because a run that printed every
;; message would bury the failures among them — which also puts the echo area
;; out of reach of a test whose subject is what the user was told.  A warning
;; is worse: displayed for real it opens a window over the test.  Both are
;; captured into a list and read back in the order they were emitted.

(defvar org-clock-projects-test--messages nil
  "The messages captured inside `org-clock-projects-test-with-messages'.")

(defmacro org-clock-projects-test-with-messages (&rest body)
  "Run BODY capturing every message it emits.
Read them back with `org-clock-projects-test-messages'."
  (declare (indent 0) (debug t))
  `(let ((org-clock-projects-test--messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args)
                  ;; `(message nil)' clears the echo area and says nothing, so
                  ;; recording it would put an entry in the list for text no
                  ;; reader ever saw.
                  (when format
                    (let ((text (apply #'format-message format args)))
                      (push text org-clock-projects-test--messages)
                      text)))))
       ,@body)))

(defun org-clock-projects-test-messages ()
  "Return the captured messages, in the order they were emitted.
The text is the one `format-message' produces, quote translation
included, so an assertion reads what the echo area would have shown."
  (reverse org-clock-projects-test--messages))

(defvar org-clock-projects-test--warnings nil
  "The warnings captured inside `org-clock-projects-test-with-warnings'.")

(defmacro org-clock-projects-test-with-warnings (&rest body)
  "Run BODY capturing the text of every warning it displays.
Read them back with `org-clock-projects-test-warnings'."
  (declare (indent 0) (debug t))
  `(let ((org-clock-projects-test--warnings nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (_type message &rest _)
                  (push message org-clock-projects-test--warnings))))
       ,@body)))

(defun org-clock-projects-test-warnings ()
  "Return the captured warnings, in the order they were displayed."
  (reverse org-clock-projects-test--warnings))

;;;; Fixtures

(defconst org-clock-projects-test-projects
  '((:tag "atag" :name "Alpha Project" :file "alpha.org")
    (:tag "btag" :name "Beta Project"  :file "beta.org")
    (:tag "gtag" :name "Gamma Project" :file "gamma.org"))
  "The projects most tests register.
A project's tag, the name it is displayed under and the base of its file
are three different strings, so a lookup keyed on the wrong one of the
three cannot pass by coincidence.  `todo.org' belongs to none of them, so
it stays a file no project owns however many of these are enrolled.")

(defun org-clock-projects-test-entry (heading &optional minutes-ago tag)
  "Return an Org entry for HEADING.
With MINUTES-AGO, the entry carries an open clock started then.  With
TAG, the headline carries that tag, which is how an entry is owned inside
a file that declares no tag of its own."
  (concat "* TODO " heading (if tag (format " :%s:" tag) "") "\n"
          (if minutes-ago
              (format ":LOGBOOK:\nCLOCK: %s\n:END:\n"
                      (org-clock-projects-test-stamp minutes-ago))
            "")))

(defun org-clock-projects-test-clock-at (heading start &optional tag)
  "Return an Org entry for HEADING carrying an open clock started at START.
With TAG, the headline carries that tag."
  (format "* TODO %s%s\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
          heading
          (if tag (format " :%s:" tag) "")
          (format-time-string (org-time-stamp-format 'with-time t) start)))

(defun org-clock-projects-test--kill-buffers (dir)
  "Kill every buffer visiting a file under DIR."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (string-prefix-p (file-name-as-directory dir) file))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))))
  (dolist (name '("*Org Note*"))
    (when (get-buffer name) (kill-buffer name))))

(defun org-clock-projects-test--reset-clock ()
  "Return Org's clock globals to their unclocked state."
  (setq org-clock-current-task nil
        org-clock-start-time nil
        org-clock-out-time nil
        org-clock-heading ""
        org-clock-history nil)
  (move-marker org-clock-marker nil)
  (move-marker org-clock-hd-marker nil)
  (setq global-mode-string (delq 'org-mode-line-string global-mode-string)))

(defmacro org-clock-projects-test-with-sandbox (&rest body)
  "Run BODY with isolated Org, clock and project state in a temp directory."
  (declare (indent 0) (debug t))
  `(let* ((tmpdir (make-temp-file "org-clock-projects-test-" t))
          (org-directory tmpdir)
          (org-clock-projects-projects nil)
          (org-clock-projects-projects-function
           #'org-clock-projects-default-projects)
          (org-clock-projects-continue-threshold 30)
          (org-clock-projects--enrolled nil)
          (org-clock-projects--current nil)
          (org-clock-projects--unscoped-out-time nil)
          ;; The remembered unscoped clock and the skip-once warning
          ;; bookkeeping both outlive the command that set them, so a test
          ;; leaving either behind answers the next test's question for it.
          (org-clock-projects--unscoped-clock nil)
          (org-clock-projects--last-registry nil)
          (org-clock-projects--warned-entries nil)
          ;; Ownership is defined under the reader's inheritance settings, so
          ;; the sandbox states all three rather than letting the machine the
          ;; suite runs on decide which entries a project owns.
          (org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil)
          (org-agenda-use-tag-inheritance t)
          ;; Rounding is a property under test, so it is off unless a test
          ;; turns it on.
          (org-clock-rounding-minutes 0)
          (org-time-stamp-rounding-minutes '(0 0))
          ;; With parallel clocks, dangling clock lines are the normal state.
          (org-clock-auto-clock-resolution nil)
          (org-clock-out-remove-zero-time-clocks nil)
          (org-clock-continuously nil)
          (org-clock-in-switch-to-state nil)
          (org-clock-out-switch-to-state nil)
          (org-clock-in-hook nil)
          (org-clock-out-hook nil)
          (org-clock-in-prepare-hook nil)
          (org-log-note-clock-out nil)
          (org-clock-persist nil)
          (org-clock-idle-time nil)
          (org-todo-keywords '((sequence "TODO" "DONE")))
          (global-mode-string nil)
          (inhibit-message t))
     (unwind-protect
         (org-clock-projects-test-at (org-clock-projects-test-hour 12)
           (org-clock-projects-test--reset-clock)
           (org-clock-projects-mode 1)
           ,@body)
       (org-clock-projects-mode -1)
       (org-clock-projects-test--reset-clock)
       (org-clock-projects-test--kill-buffers tmpdir)
       (delete-directory tmpdir t))))

(defun org-clock-projects-test--names-a-tag-p (text)
  "Return non-nil when Org fixture TEXT names a tag of its own.
A `#+FILETAGS:' line counts even when it lists nothing, and so does a
tagged headline: writing either is how a fixture says it settles who owns
its entries itself."
  (string-match-p
   "^\\(?:#\\+FILETAGS:\\|\\*+ .*[ \t]:[[:alnum:]_@#%:]+:[ \t]*$\\)" text))

(defun org-clock-projects-test--file-tag (name)
  "Return the tag of the first registered project keeping its time in NAME.
NAME is relative to `org-directory', the way the fixture alist writes it.
The registry is read through the package's own accessor, so a project
that declares no `:file' is looked up under the file the package would
give it rather than under a second copy of that default."
  (let ((path (expand-file-name name org-directory)))
    (seq-some (lambda (record)
                (and (equal path (expand-file-name (plist-get record :file)
                                                   org-directory))
                     (plist-get record :tag)))
              (org-clock-projects-project-records))))

(defun org-clock-projects-test--fixture-text (name text)
  "Return fixture TEXT as it is written to NAME.
A file that some registered project declares as its `:file' is written
with `#+FILETAGS: :TAG:' ahead of TEXT, TAG being that project's tag.  A
TEXT that names a tag of its own anywhere — a `#+FILETAGS:' line, whether
it lists tags or nothing, or a tagged headline — is written exactly as it
stands.

Ownership is the subject of most of these tests, and spelling a tag into
every entry of every fixture alist would bury it, so a project's own file
carries its project's tag for free.  Naming a tag is how a fixture takes
the question back, and that is how a file two projects share, a project
file that deliberately mentions no tag at all, and an entry inside a
project's file that belongs to no project, are each written."
  (let ((tag (and (not (org-clock-projects-test--names-a-tag-p text))
                  (org-clock-projects-test--file-tag name))))
    (if tag (format "#+FILETAGS: :%s:\n%s" tag text) text)))

(defmacro org-clock-projects-test-with-data (projects contents &rest body)
  "Run BODY with PROJECTS registered and CONTENTS written out.
PROJECTS is a registry in the layout `org-clock-projects-projects'
documents: one plist per project, each declaring at least a `:tag'.
CONTENTS is an alist of (NAME . TEXT); intermediate directories are
created, and a project's own file is tagged as
`org-clock-projects-test--fixture-text' describes."
  (declare (indent 2) (debug t))
  `(org-clock-projects-test-with-sandbox
     (setq org-clock-projects-projects ,projects)
     (pcase-dolist (`(,name . ,text) ,contents)
       (let ((path (expand-file-name name org-directory)))
         (make-directory (file-name-directory path) t)
         (with-temp-file path
           (insert (org-clock-projects-test--fixture-text name text)))))
     ;; Reading the registry to tag the fixtures also spends the skip-once
     ;; warnings on it, so the bookkeeping goes back to where the sandbox
     ;; opened it: a test about a bad entry wants its own first read to be
     ;; the read that warns.
     (setq org-clock-projects--last-registry nil
           org-clock-projects--warned-entries nil)
     ,@body))

;;;; A parity oracle for the continuation prompt
;;
;; The hand-written continuation advice this package's prompt stands in for,
;; kept here verbatim as an oracle.  The tests below run both over the same
;; inputs and compare, so the prompt's behaviour is demonstrated against a
;; second implementation rather than restated by an assertion that agrees with
;; whatever the package happens to do.

(defun org-clock-projects-test--legacy-minutes-ago (time)
  "The time helper the oracle advice reads elapsed minutes with."
  (/ (org-time-convert-to-integer (time-subtract (org-current-time) time)) 60))

(defun org-clock-projects-test--legacy-advice (orig-fn &rest args)
  "The oracle: continuation decided from the session-wide clock-out time."
  (let* ((start-time (cadr args))
         (org-clock-continuously
          (if start-time
              org-clock-continuously
            (or (org-clocking-p)
                (and org-clock-out-time
                     (or (< (org-clock-projects-test--legacy-minutes-ago
                             org-clock-out-time)
                            30)
                         (y-or-n-p "You stopped another clock; start from then? ")))))))
    (apply orig-fn args)))

;;;; Reading the world back

(defun org-clock-projects-test-buffer (file)
  "Return the buffer visiting FILE in the sandbox."
  (find-file-noselect (expand-file-name file org-directory)))

(defun org-clock-projects-test-goto (file heading)
  "Make FILE current with point on HEADING.
HEADING is the headline text alone: the todo keyword before it and the
tags after it are matched but not named, so a test names an entry the
same way whether its ownership is declared on the headline or on the
file."
  (set-buffer (org-clock-projects-test-buffer file))
  (goto-char (point-min))
  (re-search-forward (concat "^\\*+ +\\(?:[A-Z]+ +\\)?" (regexp-quote heading)
                             "\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
  (forward-line 0))

(defun org-clock-projects-test-clock-in (file heading)
  "Clock in on HEADING in FILE."
  (save-current-buffer
    (org-clock-projects-test-goto file heading)
    (org-clock-in)))

(defun org-clock-projects-test-fork (file heading)
  "Fork a clock on HEADING in FILE."
  (save-current-buffer
    (org-clock-projects-test-goto file heading)
    (org-clock-projects-fork)))

(defun org-clock-projects-test-lines (file)
  "Return the CLOCK lines of FILE, trimmed, in document order."
  (with-current-buffer (org-clock-projects-test-buffer file)
    (save-excursion
      (goto-char (point-min))
      (let (lines)
        (while (re-search-forward "^[ \t]*CLOCK:.*$" nil t)
          (push (string-trim (match-string-no-properties 0)) lines))
        (nreverse lines)))))

(defun org-clock-projects-test-open-lines (file)
  "Return the open CLOCK lines of FILE."
  (seq-remove (lambda (line) (string-match-p "--" line))
              (org-clock-projects-test-lines file)))

(defun org-clock-projects-test-closed-lines (file)
  "Return the closed CLOCK lines of FILE."
  (seq-filter (lambda (line) (string-match-p "--" line))
              (org-clock-projects-test-lines file)))

(defun org-clock-projects-test-start-of (line)
  "Return the start timestamp of CLOCK LINE, brackets included."
  (and (string-match "\\(\\[[^]]+\\]\\)" line) (match-string 1 line)))

(defun org-clock-projects-test-end-of (line)
  "Return the end timestamp of closed CLOCK LINE, brackets included."
  (and (string-match "--\\(\\[[^]]+\\]\\)" line) (match-string 1 line)))

(defun org-clock-projects-test-active-file ()
  "Return the name of the file holding the active clock, or nil."
  (and (org-clock-is-active)
       (file-name-nondirectory (buffer-file-name (org-clocking-buffer)))))

(defun org-clock-projects-test-active-heading ()
  "Return the heading the active clock is on, or nil."
  (when (org-clock-is-active)
    (org-with-point-at org-clock-marker
      (org-back-to-heading t)
      (org-get-heading t t t t))))

(defun org-clock-projects-test-current ()
  "Return the display name of the project this session is scoped to, or nil.
Unscoped work answers nil rather than
`org-clock-projects-unscoped-name': the tests ask whether there is a
current project at all as often as they ask which one it is."
  (and org-clock-projects--current
       (org-clock-projects-name org-clock-projects--current)))

;;;; Scripted prompts
;;
;; The export and check flows ask for a project, then a period, and sometimes
;; two dates after that.  A stub returning one constant would answer all of
;; them alike and hide a flow that asked the wrong question in the wrong
;; order, so the answers are a queue: an unanticipated prompt is itself a
;; failure, and so is an answer nothing asked for.

(defvar org-clock-projects-test--prompts nil)
(defvar org-clock-projects-test--prompted nil)

(defun org-clock-projects-test--prompt (prompt)
  "Answer PROMPT from the scripted prompt queue, recording that it was asked."
  (push prompt org-clock-projects-test--prompted)
  (unless org-clock-projects-test--prompts
    (error "Unscripted prompt: %s" prompt))
  (pop org-clock-projects-test--prompts))

(defun org-clock-projects-test--read-date (&optional _with-time to-time
                                                     _from-string prompt
                                                     &rest _)
  "Answer a date prompt from the queue the way `org-read-date' would.
The real picker returns the picked day at the *current* wall-clock time,
which is why a range has to be normalised to midnight before it is used.
Returning a clean midnight here would hide that, so the stub reproduces
the picker rather than the intent."
  (let ((answer (org-clock-projects-test--prompt (or prompt "Date: "))))
    (if (not to-time)
        answer
      (pcase-let ((`(,_ ,now-m ,now-h . ,_) (decode-time (current-time)))
                  (`(,_ ,_ ,_ ,d ,m ,y . ,_) (org-parse-time-string answer)))
        (org-encode-time 0 now-m now-h d m y)))))

(defmacro org-clock-projects-test-with-prompts (answers &rest body)
  "Run BODY answering each minibuffer prompt from ANSWERS, in order.
Covers `completing-read' and `org-read-date', which is every prompt the
export and check flows raise.  An empty ANSWERS asserts that BODY prompts
for nothing at all."
  (declare (indent 1) (debug t))
  `(let ((org-clock-projects-test--prompts (copy-sequence ,answers))
         (org-clock-projects-test--prompted nil)
         (org-clock-projects-test--collection nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt collection &optional _predicate _require-match
                                _initial-input _hist def &rest _)
                  (setq org-clock-projects-test--collection collection)
                  (let ((answer (org-clock-projects-test--prompt prompt)))
                    ;; Accepting the minibuffer empty is how a default is
                    ;; taken, so a scripted empty answer is what makes the
                    ;; default a prompt offers observable at all.
                    (if (equal answer "")
                        (if (consp def) (car def) def)
                      answer))))
               ((symbol-function 'org-read-date)
                #'org-clock-projects-test--read-date))
       (prog1 (progn ,@body)
         (when org-clock-projects-test--prompts
           (error "Prompts the flow never raised: %S"
                  org-clock-projects-test--prompts))))))

(defun org-clock-projects-test-prompts ()
  "Return the minibuffer prompts raised so far, in the order they were asked."
  (reverse org-clock-projects-test--prompted))

;;;; Clock fixtures

(defun org-clock-projects-test-time-at (year month day &optional hour minute)
  "Return the time at YEAR-MONTH-DAY HOUR:MINUTE, midnight by default."
  (org-encode-time 0 (or minute 0) (or hour 0) day month year))

(defun org-clock-projects-test-clock-line (start end)
  "Return a closed CLOCK line running from START to END.
The trailing duration is what makes the clock closed: `org-element'
reads the status off the presence of that tail alone, so a line without
one parses as still running, is dropped by `org-clock-csv', and is
reported as an open clock."
  (let* ((fmt (org-time-stamp-format 'with-time t))
         (minutes (floor (/ (float-time (time-subtract end start)) 60))))
    (format "CLOCK: %s--%s =>  %d:%02d\n"
            (format-time-string fmt start)
            (format-time-string fmt end)
            (/ minutes 60)
            (% minutes 60))))

(defun org-clock-projects-test-closed-entry (heading &rest clocks)
  "Return an Org entry for HEADING carrying one closed clock per CLOCKS.
Each element of CLOCKS is a (START . END) cons of times."
  (concat "* TODO " heading "\n:LOGBOOK:\n"
          (mapconcat (lambda (clock)
                       (org-clock-projects-test-clock-line
                        (car clock) (cdr clock)))
                     clocks "")
          ":END:\n"))

(defun org-clock-projects-test-file-contents (path)
  "Return the contents of PATH as a string, or nil when it does not exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun org-clock-projects-test-csv-rows (path)
  "Return the non-header lines of the CSV at PATH, in file order."
  (let ((text (org-clock-projects-test-file-contents path)))
    (and text (cdr (split-string (string-trim-right text "\n+") "\n")))))

(provide 'org-clock-projects-test-harness)
;;; org-clock-projects-test-harness.el ends here
