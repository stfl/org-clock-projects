;;; org-clock-projects-test-harness.el --- Shared harness for the suites -*- lexical-binding: t; -*-

;; The frozen clock, the scripted answer queues, the sandbox and the fixture
;; writers are shared by every suite, so they live apart from the tests that
;; use them.  The runner loads each file under test/ by its path, which
;; bypasses `features' and loads a suite a second time the moment a sibling
;; requires it; ERT refuses a test it has already seen, so no file that
;; defines one may be required by another.  A file of helpers is safe to load
;; twice, and this is that file.

;;; Code:

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

;;;; Fixtures

(defconst org-clock-projects-test-files '("alpha.org" "beta.org" "gamma.org")
  "The files that may become projects in most tests.
`todo.org' is deliberately absent, so it is a file belonging to no
project even once every candidate has been created.")

(defun org-clock-projects-test-entry (heading &optional minutes-ago)
  "Return an Org entry for HEADING.
With MINUTES-AGO, the entry carries an open clock started then."
  (concat "* TODO " heading "\n"
          (if minutes-ago
              (format ":LOGBOOK:\nCLOCK: %s\n:END:\n"
                      (org-clock-projects-test-stamp minutes-ago))
            "")))

(defun org-clock-projects-test-clock-at (heading start)
  "Return an Org entry for HEADING carrying an open clock started at START."
  (format "* TODO %s\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
          heading
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
          (org-clock-projects-files nil)
          (org-clock-projects-files-function #'org-clock-projects-default-files)
          (org-clock-projects-continue-threshold 30)
          (org-clock-projects--projects nil)
          (org-clock-projects--current nil)
          (org-clock-projects--unscoped-out-time nil)
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

(defmacro org-clock-projects-test-with-data (files contents &rest body)
  "Run BODY with FILES as project candidates and CONTENTS written out.
FILES is a list of names for `org-clock-projects-files'.  CONTENTS is an
alist of (NAME . TEXT); intermediate directories are created."
  (declare (indent 2) (debug t))
  `(org-clock-projects-test-with-sandbox
     (setq org-clock-projects-files ,files)
     (pcase-dolist (`(,name . ,text) ,contents)
       (let ((path (expand-file-name name org-directory)))
         (make-directory (file-name-directory path) t)
         (with-temp-file path (insert text))))
     ,@body))

;;;; The configuration this package absorbed
;;
;; The continuation advice moved out of the editor configuration into this
;; package.  It is reproduced here verbatim so the move can be shown to have
;; preserved the behaviour, rather than merely asserted to have.

(defun org-clock-projects-test--legacy-minutes-ago (time)
  "The time helper the absorbed advice used."
  (/ (org-time-convert-to-integer (time-subtract (org-current-time) time)) 60))

(defun org-clock-projects-test--legacy-advice (orig-fn &rest args)
  "The continuation advice this package absorbed."
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
  "Make FILE current with point on HEADING."
  (set-buffer (org-clock-projects-test-buffer file))
  (goto-char (point-min))
  (re-search-forward (concat "^\\*+ +\\(?:[A-Z]+ +\\)?" (regexp-quote heading) "$"))
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
  "Return the name of the current project, or nil."
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
                (lambda (prompt collection &rest _)
                  (setq org-clock-projects-test--collection collection)
                  (org-clock-projects-test--prompt prompt)))
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
