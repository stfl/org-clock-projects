;;; org-clock-projects-test.el --- Tests for org-clock-projects -*- lexical-binding: t; -*-

;; Every assertion here is on externally observable state: the text of the
;; CLOCK lines in the fixture files, which clock Org considers active, the
;; current project, the modeline string, and which questions a flow asked.  No
;; private helper is called, and no internal data structure is inspected beyond
;; the two session variables the modeline and the boundary check are defined in
;; terms of.  A test that would still pass after the internals are rewritten,
;; and fail if the user-visible behaviour changes, is the target.

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


;;;; The vanilla guarantee
;;
;; With no project created, this package must be invisible.

(ert-deftest org-clock-projects-test-dormant-clock-in-asks-nothing ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "alpha.org" "A one")
      (should (equal (org-clock-projects-test-questions) '()))
      (should (equal (org-clock-projects-test-open-lines "alpha.org")
                     (list (format "CLOCK: %s" (org-clock-projects-test-stamp)))))
      (should (null (org-clock-projects-test-current)))
      (should (equal org-clock-projects-mode-line-string "")))))

(ert-deftest org-clock-projects-test-dormant-clock-lines-match-stock ()
  "With nothing to continue from, the clock lines are stock Org's own."
  (let (with-package without-package)
    (dolist (enabled '(t nil))
      (org-clock-projects-test-with-data
          org-clock-projects-test-files
          `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
        (unless enabled (org-clock-projects-mode -1))
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-test-clock-in "alpha.org" "A one")
          (org-clock-projects-test-advance 45)
          (org-clock-out))
        (if enabled
            (setq with-package (org-clock-projects-test-lines "alpha.org"))
          (setq without-package (org-clock-projects-test-lines "alpha.org")))))
    (should (= (length with-package) 1))
    (should (equal with-package without-package))))

(ert-deftest org-clock-projects-test-dormant-matches-the-absorbed-advice ()
  "Dormant, the package reproduces the configuration it was moved out of."
  (let (with-package with-legacy)
    (dolist (enabled '(t nil))
      (org-clock-projects-test-with-data
          org-clock-projects-test-files
          `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                    (org-clock-projects-test-entry "A two"))))
        (unless enabled
          (org-clock-projects-mode -1)
          (advice-add 'org-clock-in :around
                      #'org-clock-projects-test--legacy-advice))
        (unwind-protect
            (org-clock-projects-test-with-answers '()
              (org-clock-projects-test-clock-in "alpha.org" "A one")
              (org-clock-projects-test-advance 45)
              (org-clock-out)
              ;; Ten minutes is inside the threshold, so both continue silently.
              (org-clock-projects-test-advance 10)
              (org-clock-projects-test-clock-in "alpha.org" "A two")
              (org-clock-projects-test-advance 20)
              (org-clock-out)
              (should (equal (org-clock-projects-test-questions) '())))
          (advice-remove 'org-clock-in #'org-clock-projects-test--legacy-advice))
        (if enabled
            (setq with-package (org-clock-projects-test-lines "alpha.org"))
          (setq with-legacy (org-clock-projects-test-lines "alpha.org")))))
    (should (= (length with-package) 2))
    (should (equal with-package with-legacy))))

(ert-deftest org-clock-projects-test-dormant-continues-silently-below-threshold ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "alpha.org" "A one")
      (org-clock-projects-test-advance 45)
      (org-clock-out)
      (org-clock-projects-test-advance 10)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (equal (org-clock-projects-test-questions) '())))
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-end-of
                    (car (org-clock-projects-test-closed-lines "alpha.org")))))))

(ert-deftest org-clock-projects-test-dormant-asks-above-threshold ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    (org-clock-projects-test-with-answers '(nil)
      (org-clock-projects-test-clock-in "alpha.org" "A one")
      (org-clock-projects-test-advance 45)
      (org-clock-out)
      (org-clock-projects-test-advance 45)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (= (length (org-clock-projects-test-questions)) 1))
      (should (string-match-p "start this one from then"
                              (car (org-clock-projects-test-questions)))))
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-stamp)))))


;;;; Scope resolution

(ert-deftest org-clock-projects-test-fork-scopes-the-session-to-the-file ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (should (equal (org-clock-projects-test-current) "alpha"))
    (should (equal (org-clock-projects-test-active-file) "alpha.org"))))

(ert-deftest org-clock-projects-test-declining-to-create-leaves-work-unscoped ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    ;; Stop alpha's clock, and decline to enrol beta.
    (org-clock-projects-test-with-answers '(t nil)
      (org-clock-projects-test-clock-in "beta.org" "B one")
      (should (= (length (org-clock-projects-test-questions)) 2)))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))))

(ert-deftest org-clock-projects-test-a-file-with-no-project-drops-the-pointer ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    ;; Only the running clock is in question: `todo.org' can never be a project,
    ;; so there is nothing to offer about the target.
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-clock-in "todo.org" "Something else")
      (should (= (length (org-clock-projects-test-questions)) 1)))
    (should (null (org-clock-projects-test-current)))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))))


;;;; Forking

(ert-deftest org-clock-projects-test-fork-leaves-the-previous-clock-open ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one")
      (should (equal (org-clock-projects-test-questions) '())))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-file) "beta.org"))
    (should (equal (org-clock-projects-test-current) "beta"))))

(ert-deftest org-clock-projects-test-fork-does-not-bridge-from-the-other-project ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    ;; Beta has no clock-out time of its own, so it starts now rather than
    ;; inheriting alpha's.
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "beta.org")))
                   (org-clock-projects-test-stamp)))))

(ert-deftest org-clock-projects-test-fork-into-a-clocked-project-stops-that-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-fork "alpha.org" "A two")
      (should (= (length (org-clock-projects-test-questions)) 1))
      (should (string-match-p "already has an open clock"
                              (car (org-clock-projects-test-questions)))))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A two"))))

(ert-deftest org-clock-projects-test-fork-declining-the-collision-aborts ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(nil)
      (should-error (org-clock-projects-test-fork "alpha.org" "A two")
                    :type 'user-error))
    (should (= (length (org-clock-projects-test-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A one"))))

(ert-deftest org-clock-projects-test-fork-outside-a-project-file-errors ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (should-error (org-clock-projects-test-fork "todo.org" "Something else")
                    :type 'user-error))
    (should (null (org-clock-projects-test-current)))
    (should (equal (org-clock-projects-test-lines "todo.org") '()))))


;;;; The boundary prompt: both questions, all four answers

(defmacro org-clock-projects-test-with-boundary (answers &rest body)
  "Fork a clock in alpha, then plain-clock-in on beta answering ANSWERS."
  (declare (indent 1) (debug t))
  `(org-clock-projects-test-with-data
       org-clock-projects-test-files
       `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
         ("beta.org" . ,(org-clock-projects-test-entry "B one")))
     (org-clock-projects-test-with-answers '()
       (org-clock-projects-test-fork "alpha.org" "A one"))
     (org-clock-projects-test-advance 30)
     (org-clock-projects-test-with-answers ,answers
       (org-clock-projects-test-clock-in "beta.org" "B one")
       ,@body)))

(ert-deftest org-clock-projects-test-boundary-stop-yes-create-yes ()
  (org-clock-projects-test-with-boundary '(t t)
    (should (= (length (org-clock-projects-test-questions)) 2))
    (should (string-match-p "Stop the clock running in alpha"
                            (nth 0 (org-clock-projects-test-questions))))
    (should (string-match-p "Create clock project beta"
                            (nth 1 (org-clock-projects-test-questions))))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 0))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-current) "beta"))
    (should (equal org-clock-projects-mode-line-string "⏱ beta"))))

(ert-deftest org-clock-projects-test-boundary-stop-yes-create-no ()
  (org-clock-projects-test-with-boundary '(t nil)
    (should (= (length (org-clock-projects-test-questions)) 2))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))))

(ert-deftest org-clock-projects-test-boundary-stop-no-create-yes ()
  (org-clock-projects-test-with-boundary '(nil t)
    (should (= (length (org-clock-projects-test-questions)) 2))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 0))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-file) "beta.org"))
    (should (equal (org-clock-projects-test-current) "beta"))
    (should (equal org-clock-projects-mode-line-string "⏱ beta [+1]"))))

(ert-deftest org-clock-projects-test-boundary-stop-no-create-no ()
  (org-clock-projects-test-with-boundary '(nil nil)
    (should (= (length (org-clock-projects-test-questions)) 2))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-file) "beta.org"))
    (should (null (org-clock-projects-test-current)))))


;;;; Per-project clock-out times
;;
;; Continuation must be scoped to the engagement.  Bridging it across projects
;; moves billable minutes between clients without a prompt.

(ert-deftest org-clock-projects-test-a-project-resumes-its-own-clock-out-time ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out)
      (org-clock-projects-test-advance 10)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (equal (org-clock-projects-test-questions) '())))
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-end-of
                    (car (org-clock-projects-test-closed-lines "alpha.org")))))))

(ert-deftest org-clock-projects-test-a-project-never-continues-from-another ()
  "The pain this feature exists for: minutes must not move between clients."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (let ((alpha-end (org-clock-projects-test-end-of
                      (car (org-clock-projects-test-closed-lines "alpha.org")))))
      ;; Five minutes later — well inside the continuation threshold, which is
      ;; exactly when the session-wide clock-out time would have been used.
      (org-clock-projects-test-advance 5)
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-test-clock-in "beta.org" "B one")
        ;; Only the create question: beta has no clock-out time to resume from.
        (should (= (length (org-clock-projects-test-questions)) 1))
        (should (string-match-p "Create clock project beta"
                               (car (org-clock-projects-test-questions)))))
      (let ((beta-start (org-clock-projects-test-start-of
                         (car (org-clock-projects-test-open-lines "beta.org")))))
        (should (equal beta-start (org-clock-projects-test-stamp)))
        (should-not (equal beta-start alpha-end))))))

(ert-deftest org-clock-projects-test-returning-to-a-project-offers-its-own-time ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (let ((alpha-end (org-clock-projects-test-end-of
                      (car (org-clock-projects-test-closed-lines "alpha.org")))))
      (org-clock-projects-test-advance 5)
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-test-clock-in "beta.org" "B one"))
      (org-clock-projects-test-advance 60)
      (org-clock-out)
      ;; Back to alpha.  Its own clock-out time is 65 minutes old, so it is
      ;; offered rather than taken; beta's is 0 minutes old and must not be.
      (org-clock-projects-test-advance 5)
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-test-clock-in "alpha.org" "A two")
        (should (= (length (org-clock-projects-test-questions)) 1))
        (should (string-match-p "start this one from then"
                                (car (org-clock-projects-test-questions)))))
      (should (equal (org-clock-projects-test-start-of
                      (car (org-clock-projects-test-open-lines "alpha.org")))
                     alpha-end)))))

(ert-deftest org-clock-projects-test-declining-the-resume-starts-now ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (org-clock-projects-test-advance 5)
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-clock-in "beta.org" "B one"))
    (org-clock-projects-test-advance 60)
    (org-clock-out)
    (org-clock-projects-test-advance 5)
    (org-clock-projects-test-with-answers '(nil)
      (org-clock-projects-test-clock-in "alpha.org" "A two"))
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-stamp)))))


;;;; One clock per project

(ert-deftest org-clock-projects-test-clocking-in-again-inside-a-project ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (equal (org-clock-projects-test-questions) '())))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A two"))))

(ert-deftest org-clock-projects-test-clocking-into-a-projects-inactive-clock ()
  "Org will not stop a clock that is not active, so this path must."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    ;; Keep beta running; alpha's own clock is closed by clocking into alpha.
    (org-clock-projects-test-with-answers '(nil)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (= (length (org-clock-projects-test-questions)) 1)))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A two"))))


(ert-deftest org-clock-projects-test-clocks-at-equal-offsets-stay-distinct ()
  "Two clocks can sit at the same character offset in different files.
`org-is-active-clock' compares positions alone and calls them the same
clock, which sends a clock-out to the wrong project.  The files here are
byte-identical so the offsets are guaranteed to collide."
  (let ((content "* TODO Task\n"))
    (org-clock-projects-test-with-data
        org-clock-projects-test-files
        `(("alpha.org" . ,content) ("beta.org" . ,content))
      (org-clock-projects-test-with-answers '()
        (org-clock-projects-test-fork "alpha.org" "Task")
        (org-clock-projects-test-advance 30)
        (org-clock-projects-test-fork "beta.org" "Task"))
      ;; The collision is real: the two markers are at the same offset in
      ;; different buffers, and beta's is the active one.
      (let ((clocks (org-clock-projects-open-clocks)))
        (should (= (length clocks) 2))
        (should (apply #'= (mapcar (lambda (entry)
                                     (marker-position (cadr entry)))
                                   clocks))))
      (org-clock-projects-test-advance 30)
      ;; Close alpha's and leave beta's; a position-only comparison closes
      ;; beta here instead.
      (org-clock-projects-test-with-answers '(t nil)
        (org-clock-projects-kill-emacs-query))
      (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
      (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
      (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
      (should (equal (org-clock-projects-test-closed-lines "beta.org") '())))))


(ert-deftest org-clock-projects-test-a-task-selection-scopes-its-own-file ()
  "`org-clock-in' with a selection clocks in another file than the current one."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (let ((task (with-current-buffer (org-clock-projects-test-buffer "beta.org")
                  (goto-char (point-min))
                  (re-search-forward "^\\*+ +TODO B one$")
                  (point-marker))))
      (cl-letf (((symbol-function 'org-clock-select-task) (lambda (&rest _) task)))
        ;; Point is in alpha; the selected task is in beta, and beta is what
        ;; the questions and the scope must be about.
        (org-clock-projects-test-with-answers '(t t)
          (save-current-buffer
            (org-clock-projects-test-goto "alpha.org" "A one")
            (org-clock-in '(4)))
          (should (= (length (org-clock-projects-test-questions)) 2))
          (should (string-match-p "Create clock project beta"
                                  (nth 1 (org-clock-projects-test-questions)))))))
    (should (equal (org-clock-projects-test-current) "beta"))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))))

(ert-deftest org-clock-projects-test-an-explicit-start-time-keeps-the-invariant ()
  "Clocking in at a chosen time still leaves one clock in the project."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (let ((chosen (org-clock-projects-test-time 10)))
      ;; Keep beta running; alpha still must not end up with two open clocks.
      (org-clock-projects-test-with-answers '(nil)
        (save-current-buffer
          (org-clock-projects-test-goto "alpha.org" "A two")
          (org-clock-in nil chosen))
        (should (= (length (org-clock-projects-test-questions)) 1)))
      (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
      (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
      (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
      ;; The chosen time is the caller's and is not overridden.
      (should (equal (org-clock-projects-test-start-of
                      (car (org-clock-projects-test-open-lines "alpha.org")))
                     (org-clock-projects-test-stamp 10))))))


;;;; The switcher

(ert-deftest org-clock-projects-test-switch-without-clocks-falls-back ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (let ((called nil))
      (cl-letf (((symbol-function 'org-clock-goto)
                 (lambda (&rest _) (interactive) (setq called t))))
        (org-clock-projects-test-with-choice nil
          (org-clock-projects-switch)))
      (should called))))

(ert-deftest org-clock-projects-test-switch-with-one-clock-does-not-prompt ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    ;; A nil choice makes any completion prompt an error.
    (org-clock-projects-test-with-choice nil
      (org-clock-projects-switch))
    (should (equal (org-clock-projects-test-active-heading) "A one"))))

(ert-deftest org-clock-projects-test-switch-activates-a-dangling-clock ()
  "A clock left open across a restart can be resumed and then clocked out."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 9))))
    (should-not (org-clock-is-active))
    (org-clock-projects-test-with-choice nil
      (org-clock-projects-switch))
    (should (org-clock-is-active))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    (org-clock-out)
    (let ((line (car (org-clock-projects-test-closed-lines "alpha.org"))))
      (should (string-match-p "=> +3:00" line)))))

(ert-deftest org-clock-projects-test-switch-makes-the-chosen-clock-active ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "gamma.org" "G one"))
    (should (equal (org-clock-projects-test-active-file) "gamma.org"))
    (org-clock-projects-test-with-choice "alpha"
      (org-clock-projects-switch))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    (should (equal (org-clock-projects-test-current) "alpha"))
    ;; Nothing was ended by switching.
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "gamma.org")) 1))))

(ert-deftest org-clock-projects-test-switch-offers-annotated-project-names ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 90)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-with-choice "alpha"
      (org-clock-projects-switch)
      ;; Candidates are project names, in start-time order, and the order is
      ;; pinned so a completion UI cannot re-sort them.
      (should (equal (org-clock-projects-test-candidates) '("alpha" "beta")))
      (should (eq (org-clock-projects-test-metadata 'display-sort-function)
                  #'identity))
      ;; The category must not be one an annotation package recognises, or its
      ;; own annotations would replace these.
      (should (eq (org-clock-projects-test-metadata 'category)
                  'org-clock-projects--clock))
      (let ((annotation (org-clock-projects-test-annotation "alpha")))
        (should (string-match-p "A one" annotation))
        (should (string-match-p "1:30" annotation))
        (should (string-match-p (format-time-string
                                 "%H:%M" (org-clock-projects-test-time 90))
                                annotation))))))


;;;; Clocking out everything

(ert-deftest org-clock-projects-test-clock-out-all-closes-every-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-fork "beta.org" "B one")
      (org-clock-projects-test-fork "gamma.org" "G one")
      (org-clock-projects-test-advance 60)
      (org-clock-projects-clock-out-all)
      (should (equal (org-clock-projects-test-questions) '())))
    (dolist (file '("alpha.org" "beta.org" "gamma.org"))
      (should (equal (org-clock-projects-test-open-lines file) '()))
      (should (= (length (org-clock-projects-test-closed-lines file)) 1)))
    (should-not (org-clock-is-active))))

(ert-deftest org-clock-projects-test-clock-out-all-asks-for-no-note ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (setq org-log-note-clock-out t)
    (let ((notes 0))
      (cl-letf (((symbol-function 'org-add-log-setup)
                 (lambda (&rest _) (cl-incf notes))))
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-test-fork "alpha.org" "A one")
          (org-clock-projects-test-fork "beta.org" "B one")
          (org-clock-projects-test-advance 60)
          (org-clock-projects-clock-out-all))
        (should (= notes 0))
        ;; Control: an ordinary clock-out with the same setting does ask.
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-test-fork "alpha.org" "A one")
          (org-clock-projects-test-advance 60)
          (org-clock-out))
        (should (= notes 1))))))

(ert-deftest org-clock-projects-test-clock-out-all-applies-rounding ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 9))))
    (setq org-clock-rounding-minutes 15)
    (org-clock-projects-test-at (org-clock-projects-test-hour 12 7)
      (org-clock-projects-test-with-answers '()
        (org-clock-projects-clock-out-all)))
    (let ((line (car (org-clock-projects-test-closed-lines "alpha.org"))))
      ;; 12:07 rounds to the nearest quarter hour, as a hand clock-out would.
      (should (string-match-p "12:00\\]" (org-clock-projects-test-end-of line)))
      (should (string-match-p "=> +3:00" line)))))

(ert-deftest org-clock-projects-test-clock-out-all-skips-a-negative-duration ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 18)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 9))))
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-clock-out-all)))
      (should (= (length warnings) 1))
      (should (string-match-p "alpha" (car warnings))))
    ;; The bad clock is left alone rather than written as a negative duration.
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "beta.org")) 1))))


(ert-deftest org-clock-projects-test-clock-out-all-skips-a-rounded-back-end ()
  "The realistic negative duration: rounding pulls the end before the start.
With quarter-hour rounding a clock started a few minutes ago rounds its
end back past its own start, which is how a bad duration actually arises."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 12 5)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 11))))
    (setq org-clock-rounding-minutes 15)
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        ;; 12:07 rounds to 12:00, which precedes alpha's 12:05 start.
        (org-clock-projects-test-at (org-clock-projects-test-hour 12 7)
          (org-clock-projects-test-with-answers '()
            (org-clock-projects-clock-out-all))))
      (should (= (length warnings) 1))
      (should (string-match-p "alpha" (car warnings))))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (let ((line (car (org-clock-projects-test-closed-lines "beta.org"))))
      (should (string-match-p "12:00\\]" (org-clock-projects-test-end-of line)))
      (should (string-match-p "=> +1:00" line)))))


;;;; Clearing

(ert-deftest org-clock-projects-test-clear-clocks-out-when-asked ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 60)
    (org-clock-projects-test-with-choice "alpha"
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-clear)))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))))

(ert-deftest org-clock-projects-test-clear-keeps-a-declined-clock-unscoped ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-choice "alpha"
      (org-clock-projects-test-with-answers '(nil)
        (org-clock-projects-clear)))
    ;; The time is preserved even though the project is gone, and the clock
    ;; left without a home becomes the active one.
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))))

(ert-deftest org-clock-projects-test-clear-warns-when-a-clock-is-already-unscoped ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    ;; Leave alpha running and start an unscoped clock alongside it.
    (org-clock-projects-test-with-answers '(nil)
      (org-clock-projects-test-clock-in "todo.org" "Something else"))
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (org-clock-projects-test-with-choice "alpha"
          (org-clock-projects-test-with-answers '(nil)
            (org-clock-projects-clear))))
      (should (= (length warnings) 1))
      (should (string-match-p "alpha" (car warnings))))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))))

(ert-deftest org-clock-projects-test-clear-forgets-the-stored-clock-out-time ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (let ((alpha-end (org-clock-projects-test-end-of
                      (car (org-clock-projects-test-closed-lines "alpha.org")))))
      (org-clock-projects-test-advance 5)
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-test-clock-in "beta.org" "B one"))
      (org-clock-projects-test-advance 60)
      (org-clock-out)
      ;; Alpha is cleared, so its own stored time stops applying.
      (org-clock-projects-test-advance 5)
      (org-clock-projects-test-with-choice "alpha"
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-clear)))
      (org-clock-projects-test-with-answers '(nil)
        (org-clock-projects-test-clock-in "alpha.org" "A two"))
      (should-not (equal (org-clock-projects-test-start-of
                          (car (org-clock-projects-test-open-lines "alpha.org")))
                         alpha-end)))))

(ert-deftest org-clock-projects-test-clear-all-returns-to-the-vanilla-state ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-clear t)
      (should (= (length (org-clock-projects-test-questions)) 2)))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (equal (org-clock-projects-test-open-lines "beta.org") '()))
    ;; Dormant again: clocking in raises nothing.
    (org-clock-projects-test-advance 5)
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (equal (org-clock-projects-test-questions) '())))))


;;;; Exit
;;
;; Exit cannot be exercised by exiting, so the query function is its own seam.

(ert-deftest org-clock-projects-test-exit-asks-once-per-open-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-fork "beta.org" "B one")
      (org-clock-projects-test-fork "gamma.org" "G one"))
    (org-clock-projects-test-advance 60)
    (org-clock-projects-test-with-answers '(t t t)
      (should (eq (org-clock-projects-kill-emacs-query) t))
      (should (= (length (org-clock-projects-test-questions)) 3)))
    (dolist (file '("alpha.org" "beta.org" "gamma.org"))
      (should (equal (org-clock-projects-test-open-lines file) '())))))

(ert-deftest org-clock-projects-test-exit-asks-with-no-clock-active ()
  "Org's own exit query inspects the active clock, so it sees none of these."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 9)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 10))))
    (should-not (org-clock-is-active))
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-kill-emacs-query)
      (should (= (length (org-clock-projects-test-questions)) 2)))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (equal (org-clock-projects-test-open-lines "beta.org") '()))))

(ert-deftest org-clock-projects-test-exit-leaves-declined-clocks-open ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 9)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 10))))
    (org-clock-projects-test-with-answers '(t nil)
      (org-clock-projects-kill-emacs-query))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))))


;;;; Modeline

(ert-deftest org-clock-projects-test-mode-line-is-empty-while-dormant ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (should (equal org-clock-projects-mode-line-string ""))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "alpha.org" "A one"))
    (should (equal org-clock-projects-mode-line-string ""))))

(ert-deftest org-clock-projects-test-mode-line-counts-other-clocks ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (should (equal org-clock-projects-mode-line-string "⏱ alpha"))
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one")
      (should (equal org-clock-projects-mode-line-string "⏱ beta [+1]"))
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "gamma.org" "G one")
      (should (equal org-clock-projects-mode-line-string "⏱ gamma [+2]")))))

(ert-deftest org-clock-projects-test-mode-line-survives-clocking-out ()
  "A project outlives its clocks."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (should (equal org-clock-projects-mode-line-string "⏱ alpha"))
    (should (equal (org-clock-projects-test-current) "alpha"))))


;;;; The resolver

(ert-deftest org-clock-projects-test-default-resolver-reads-the-variable ()
  (org-clock-projects-test-with-data
      '("alpha.org")
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (should-error (org-clock-projects-test-fork "beta.org" "B one")
                    :type 'user-error))))

(ert-deftest org-clock-projects-test-a-custom-resolver-is-used ()
  (org-clock-projects-test-with-data
      '("alpha.org")
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (setq org-clock-projects-files-function (lambda () '("beta.org")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one")
      (should-error (org-clock-projects-test-fork "alpha.org" "A one")
                    :type 'user-error))))

;;;; Damage repair
;;
;; A project holding more than one open clock is not a state this package
;; creates: it comes from a crash, or an exit query that was declined.  Every
;; path that can see a surplus clock has to be able to close it, or it is
;; permanent.

(defun org-clock-projects-test-two-dangling ()
  "Return a file holding two entries whose clocks were both left open."
  (concat (org-clock-projects-test-clock-at
           "A one" (org-clock-projects-test-hour 9))
          (org-clock-projects-test-clock-at
           "A two" (org-clock-projects-test-hour 10))))

(ert-deftest org-clock-projects-test-clock-out-all-closes-a-surplus-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-two-dangling)))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-clock-out-all))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 2))))

(ert-deftest org-clock-projects-test-exit-query-offers-a-surplus-clock ()
  "The exit query is the last line of defence: a clock it never offers is kept."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-two-dangling))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 60)
    (org-clock-projects-test-with-answers '(t t t)
      (org-clock-projects-kill-emacs-query)
      (should (= (length (org-clock-projects-test-questions)) 3)))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (equal (org-clock-projects-test-open-lines "beta.org") '()))))

(ert-deftest org-clock-projects-test-clocking-in-repairs-a-surplus-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-two-dangling)
                                (org-clock-projects-test-entry "A three")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 60)
    ;; Stop beta, enrol alpha.
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "alpha.org" "A three"))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 2))
    (should (equal (org-clock-projects-test-active-heading) "A three"))))

(ert-deftest org-clock-projects-test-fork-closes-every-clock-the-project-holds ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-two-dangling)))
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-fork "alpha.org" "A two")
      (should (string-match-p "2 open clocks"
                              (car (org-clock-projects-test-questions)))))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 2))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    ;; Started, not resumed: leaving the clock at point open would make Org
    ;; read this as a continuation and write nothing at all.
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-stamp)))))

(ert-deftest org-clock-projects-test-a-file-listed-twice-counts-once ()
  (org-clock-projects-test-with-data
      '("alpha.org" "alpha.org" "beta.org")
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (should (equal org-clock-projects-mode-line-string "⏱ beta [+1]"))))


;;;; Clocks running in the background
;;
;; `org-with-clock' redirects the marker, but `org-clock-out' tears down the
;; display, the timers and the remembered task of *the* clock.

(ert-deftest org-clock-projects-test-a-background-clock-out-spares-the-running-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (should (memq 'org-mode-line-string global-mode-string))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-choice "alpha"
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-clear)))
    (should (equal (org-clock-projects-test-active-heading) "B one"))
    (should (equal org-clock-current-task "B one"))
    (should (memq 'org-mode-line-string global-mode-string))))

(ert-deftest org-clock-projects-test-the-running-task-is-still-recognised ()
  "Forgetting the running task makes Org split its clock line instead of
recognising a clock-in on it as the same clock."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-choice "alpha"
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-clear)))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "beta.org" "B one"))
    (should (equal (org-clock-projects-test-closed-lines "beta.org") '()))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))))


;;;; Arithmetic across a boundary

(ert-deftest org-clock-projects-test-crossing-a-boundary-leaves-no-overlap ()
  "Rounding must not bill one interval to two clients.
The end is rounded to nearest and a fresh start is rounded down, so a
clock-in that computes its own start opens a gap backwards over the end
just written."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (setq org-clock-rounding-minutes 15)
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 38)
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "beta.org" "B one"))
    (should (equal (org-clock-projects-test-end-of
                    (car (org-clock-projects-test-closed-lines "alpha.org")))
                   (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "beta.org")))))))

(ert-deftest org-clock-projects-test-unscoped-work-keeps-its-own-time ()
  "A project's clock-out must not flow into a file left unenrolled."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (let ((alpha-end (org-clock-projects-test-end-of
                      (car (org-clock-projects-test-closed-lines "alpha.org")))))
      (org-clock-projects-test-advance 20)
      ;; gamma is another client's file; decline to enrol it.
      (org-clock-projects-test-with-answers '(nil)
        (org-clock-projects-test-clock-in "gamma.org" "G one")
        (should (= (length (org-clock-projects-test-questions)) 1)))
      (let ((gamma-start (org-clock-projects-test-start-of
                          (car (org-clock-projects-test-open-lines "gamma.org")))))
        (should-not (equal gamma-start alpha-end))
        (should (equal gamma-start (org-clock-projects-test-stamp)))))))

(ert-deftest org-clock-projects-test-the-last-clock-out-prefix-stays-in-project ()
  "`C-u C-u C-u' starts from the last clock-out — this project's own."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (let ((alpha-end (org-clock-projects-test-end-of
                      (car (org-clock-projects-test-closed-lines "alpha.org")))))
      (org-clock-projects-test-advance 90)
      (org-clock-projects-test-with-answers '(t)
        (save-current-buffer
          (org-clock-projects-test-goto "beta.org" "B one")
          (org-clock-in '(64))))
      (let ((beta-start (org-clock-projects-test-start-of
                         (car (org-clock-projects-test-open-lines "beta.org")))))
        (should-not (equal beta-start alpha-end))
        (should (equal beta-start (org-clock-projects-test-stamp)))))))

(ert-deftest org-clock-projects-test-the-threshold-measures-elapsed-time ()
  "`org-time-stamp-rounding-minutes' governs `C-c .', not the clock."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    (setq org-time-stamp-rounding-minutes '(30 5))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 31)
      (org-clock-out))
    ;; 43 minutes is well above the threshold, so it is asked about.
    (org-clock-projects-test-advance 43)
    (org-clock-projects-test-with-answers '(nil)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (= (length (org-clock-projects-test-questions)) 1))
      (should (string-match-p "(-43m)" (car (org-clock-projects-test-questions)))))
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-stamp)))))

(ert-deftest org-clock-projects-test-a-chosen-start-time-is-not-overridden ()
  "The shape `stfl/org-clock-in-at' uses: clock out at a time, then clock in at it."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(concat (org-clock-projects-test-clock-at
                                 "A one" (org-clock-projects-test-hour 9))
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    ;; 14:00 now; the user types 13:00.
    (org-clock-projects-test-at (org-clock-projects-test-hour 14)
      (let ((chosen (org-clock-projects-test-hour 13)))
        (org-clock-projects-test-with-answers '(t)
          ;; Faithful to the caller: it binds `org-clock-continuously' from
          ;; `org-clocking-p', clocks out at the time, then clocks in at it.
          (let ((org-clock-continuously (org-clocking-p)))
            (org-clock-out nil nil chosen)
            (save-current-buffer
              (org-clock-projects-test-goto "alpha.org" "A two")
              (org-clock-in nil chosen))))
        ;; The typed time is what was written, and the clock it replaced ends
        ;; there too rather than at now.
        (should (equal (org-clock-projects-test-start-of
                        (car (org-clock-projects-test-open-lines "alpha.org")))
                       (format-time-string (org-time-stamp-format 'with-time t)
                                           chosen)))
        (should (equal (org-clock-projects-test-end-of
                        (car (org-clock-projects-test-closed-lines "alpha.org")))
                       (format-time-string (org-time-stamp-format 'with-time t)
                                           chosen)))
        (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))))))

(ert-deftest org-clock-projects-test-a-failed-clock-in-creates-no-project ()
  "A project created for a clock-in that never happened would suppress the
boundary prompt for that file ever after."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (with-current-buffer (org-clock-projects-test-buffer "gamma.org")
      (setq buffer-read-only t))
    (org-clock-projects-test-with-answers '(t t)
      (should-error
       (save-current-buffer
         (org-clock-projects-test-goto "gamma.org" "G one")
         (org-clock-in))))
    (should (equal (org-clock-projects-test-current) "alpha"))
    (should (equal (org-clock-projects-test-lines "gamma.org") '()))))


(ert-deftest org-clock-projects-test-an-unresumable-clock-line-is-not-doubled ()
  "Org reads a clock line more loosely than it resumes one.
A line without a start time is found by the reader, and the element
parser calls it a clock, but Org's resume branch rejects it and answers
with a *new* clock line instead — leaving the project holding two."
  (org-clock-projects-test-with-data
      org-clock-projects-test-files
      `(("alpha.org" . ,(format "* TODO A one\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                                (format-time-string
                                 "[%Y-%m-%d %a]"
                                 (org-clock-projects-test-time)))))
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        (org-clock-projects-test-with-choice nil
          (org-clock-projects-switch)))
      (should (= (length warnings) 1))
      (should (string-match-p "A one" (car warnings))))
    ;; The one line it started with, and no second one written beside it.
    (should (= (length (org-clock-projects-test-lines "alpha.org")) 1))
    (should-not (org-clock-is-active))))

(provide 'org-clock-projects-test)
;;; org-clock-projects-test.el ends here
