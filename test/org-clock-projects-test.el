;;; org-clock-projects-test.el --- Tests for org-clock-projects -*- lexical-binding: t; -*-

;; Every assertion here is on externally observable state: the text of the
;; CLOCK lines in the fixture files, which clock Org considers active, the
;; name of the current project, the modeline string, the candidates a
;; completion offered, the questions a flow asked in order, and the messages
;; and warnings it emitted.  No private helper is called and no internal data
;; structure is inspected; the one session variable a test reads is the
;; current-scope pointer, and it reads it through the harness, by the name the
;; modeline shows it under.  A test that would still pass after the internals
;; are rewritten, and fail if the user-visible behaviour changes, is the
;; target.
;;
;; A flow that cancels signals a `user-error', and that error escapes
;; `org-clock-projects-test-with-answers' before its check that every scripted
;; answer was used.  So a cancelling flow is wrapped in `should-error' inside
;; the answers macro, and the state it left behind is asserted after it.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'org-clock-projects-test-harness)

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-clock)
(require 'org-clock-projects)


;;;; The vanilla guarantee
;;
;; With no project enrolled, this package must be invisible.

(ert-deftest org-clock-projects-test-dormant-clock-in-asks-nothing ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
          org-clock-projects-test-projects
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
          org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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

(ert-deftest org-clock-projects-test-fork-scopes-the-session-to-the-project ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (should (equal (org-clock-projects-test-active-file) "alpha.org"))))

(ert-deftest org-clock-projects-test-declining-the-enrolment-cancels-the-clock-in ()
  "No clock ever opens under a scope the answer refused."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (let (questions)
      ;; Stop alpha's clock, and decline to enrol beta.
      (org-clock-projects-test-with-answers '(t nil)
        (should-error (org-clock-projects-test-clock-in "beta.org" "B one")
                      :type 'user-error)
        (setq questions (org-clock-projects-test-questions)))
      (should (equal questions '("Stop the clock running in Alpha Project? "
                                 "Enrol clock project Beta Project? "))))
    ;; Nothing happened at all: no clock line at the target, alpha's clock
    ;; still running, and the session still alpha's.
    (should (equal (org-clock-projects-test-lines "beta.org") '()))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (should (equal org-clock-projects-mode-line-string " ⏱ Alpha Project "))))

(ert-deftest org-clock-projects-test-crossing-into-unscoped-work-drops-the-pointer ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    ;; No project owns `todo.org's entry, so the second question is consent to
    ;; leave alpha for the nil scope rather than an offer to enrol anything.
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "todo.org" "Something else")
      (should (equal (org-clock-projects-test-questions)
                     '("Stop the clock running in Alpha Project? "
                       "Leave Alpha Project for unscoped work? "))))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))))

(ert-deftest org-clock-projects-test-declining-the-crossing-cancels-the-clock-in ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t nil)
      (should-error (org-clock-projects-test-clock-in "todo.org" "Something else")
                    :type 'user-error))
    (should (equal (org-clock-projects-test-lines "todo.org") '()))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))))

(ert-deftest org-clock-projects-test-an-entry-with-two-tags-belongs-to-the-first ()
  "Ownership is decided in registry order, so it is decided the same way
everywhere.  An entry carrying both tags is one project's, and the other
project's invariant leaves its clock alone."
  (org-clock-projects-test-with-data
      '((:tag "atag" :name "Alpha Project" :file "work.org")
        (:tag "btag" :name "Beta Project" :file "work.org"))
      `(("work.org" . ,(concat (org-clock-projects-test-entry "Both" nil "atag:btag")
                               (org-clock-projects-test-entry "B one" nil "btag"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "work.org" "Both"))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "work.org" "B one"))
    (should (equal (org-clock-projects-test-current) "Beta Project"))
    ;; Beta closed only its own: the doubly tagged clock is alpha's.
    (should (= (length (org-clock-projects-test-open-lines "work.org")) 2))
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project [+1] "))))


;;;; Forking

(ert-deftest org-clock-projects-test-fork-leaves-the-previous-clock-open ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
    (should (equal (org-clock-projects-test-current) "Beta Project"))))

(ert-deftest org-clock-projects-test-fork-does-not-bridge-from-the-other-project ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      ;; Only alpha's own clock is in question; beta's keeps running.
      (should (equal (org-clock-projects-test-questions)
                     '("Alpha Project already has A one (12:00, 1:00); \
clock out and start here? "))))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A two"))))

(ert-deftest org-clock-projects-test-fork-declining-the-collision-aborts ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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

(ert-deftest org-clock-projects-test-fork-at-an-unowned-entry-crosses-into-unscoped-work ()
  "The gesture still means something off-project: it opens the nil scope's
clock and leaves every project's clock running."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-fork "todo.org" "Something else")
      (should (equal (org-clock-projects-test-questions)
                     '("Leave Alpha Project for unscoped work? "))))
    (should (null (org-clock-projects-test-current)))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "Something else"))))

(ert-deftest org-clock-projects-test-fork-declining-the-crossing-aborts ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(nil)
      (should-error (org-clock-projects-test-fork "todo.org" "Something else")
                    :type 'user-error))
    (should (equal (org-clock-projects-test-lines "todo.org") '()))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (should (equal (org-clock-projects-test-active-heading) "A one"))))

(ert-deftest org-clock-projects-test-fork-while-unscoped-crosses-no-boundary ()
  "Already unscoped, nothing is crossed: no question, and only the nil
scope's own clock is closed."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(concat (org-clock-projects-test-entry "Something else")
                               (org-clock-projects-test-entry "Another thing"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-fork "todo.org" "Something else"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "todo.org" "Another thing")
      (should (equal (org-clock-projects-test-questions) '())))
    ;; The nil scope holds one clock like any other, and alpha's is untouched.
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "todo.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "Another thing"))))


;;;; The boundary prompt: both questions, all four answers

(defmacro org-clock-projects-test-with-boundary (&rest body)
  "Fork a clock in alpha, let half an hour pass, then run BODY.
BODY crosses into beta itself: two of the four answers proceed and two
cancel, and cancelling signals, so the clock-in cannot sit inside a
wrapper shared by all four."
  (declare (indent 0) (debug t))
  `(org-clock-projects-test-with-data
       org-clock-projects-test-projects
       `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
         ("beta.org" . ,(org-clock-projects-test-entry "B one")))
     (org-clock-projects-test-with-answers '()
       (org-clock-projects-test-fork "alpha.org" "A one"))
     (org-clock-projects-test-advance 30)
     ,@body))

(defconst org-clock-projects-test-boundary-questions
  '("Stop the clock running in Alpha Project? "
    "Enrol clock project Beta Project? ")
  "The two questions crossing from alpha into unenrolled beta asks.
Question one is a choice between stopping the running clock and leaving
it running in parallel, so either answer proceeds.  Question two is
consent to the crossing itself, and declining it cancels the clock-in.")

(ert-deftest org-clock-projects-test-boundary-stop-yes-enrol-yes ()
  (org-clock-projects-test-with-boundary
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "beta.org" "B one")
      (should (equal (org-clock-projects-test-questions)
                     org-clock-projects-test-boundary-questions)))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 0))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-current) "Beta Project"))
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project "))))

(ert-deftest org-clock-projects-test-boundary-stop-yes-enrol-no ()
  "Declining cancels, so the clock question one was about is still running."
  (org-clock-projects-test-with-boundary
    (let (questions)
      (org-clock-projects-test-with-answers '(t nil)
        (should-error (org-clock-projects-test-clock-in "beta.org" "B one")
                      :type 'user-error)
        (setq questions (org-clock-projects-test-questions)))
      (should (equal questions org-clock-projects-test-boundary-questions)))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 0))
    (should (equal (org-clock-projects-test-lines "beta.org") '()))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (should (equal org-clock-projects-mode-line-string " ⏱ Alpha Project "))))

(ert-deftest org-clock-projects-test-boundary-stop-no-enrol-yes ()
  (org-clock-projects-test-with-boundary
    (org-clock-projects-test-with-answers '(nil t)
      (org-clock-projects-test-clock-in "beta.org" "B one")
      (should (equal (org-clock-projects-test-questions)
                     org-clock-projects-test-boundary-questions)))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 0))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-file) "beta.org"))
    (should (equal (org-clock-projects-test-current) "Beta Project"))
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project [+1] "))))

(ert-deftest org-clock-projects-test-boundary-stop-no-enrol-no ()
  (org-clock-projects-test-with-boundary
    (let (questions)
      (org-clock-projects-test-with-answers '(nil nil)
        (should-error (org-clock-projects-test-clock-in "beta.org" "B one")
                      :type 'user-error)
        (setq questions (org-clock-projects-test-questions)))
      (should (equal questions org-clock-projects-test-boundary-questions)))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-lines "beta.org") '()))
    (should (equal (org-clock-projects-test-active-file) "alpha.org"))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))))


;;;; Per-project clock-out times
;;
;; Continuation must be scoped to the engagement.  Bridging it across projects
;; moves billable minutes between clients without a prompt.

(ert-deftest org-clock-projects-test-a-project-resumes-its-own-clock-out-time ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
        ;; Only the enrolment question: no clock is running to be asked about,
        ;; and beta has no clock-out time to resume from.
        (should (equal (org-clock-projects-test-questions)
                       '("Enrol clock project Beta Project? "))))
      (let ((beta-start (org-clock-projects-test-start-of
                         (car (org-clock-projects-test-open-lines "beta.org")))))
        (should (equal beta-start (org-clock-projects-test-stamp)))
        (should-not (equal beta-start alpha-end))))))

(ert-deftest org-clock-projects-test-returning-to-a-project-offers-its-own-time ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    ;; Keep beta running; alpha's own clock is closed by clocking into alpha.
    ;; Alpha is enrolled, so the crossing needs no consent beyond this.
    (org-clock-projects-test-with-answers '(nil)
      (org-clock-projects-test-clock-in "alpha.org" "A two")
      (should (equal (org-clock-projects-test-questions)
                     '("Stop the clock running in Beta Project? "))))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A two"))))


(ert-deftest org-clock-projects-test-clocks-at-equal-offsets-stay-distinct ()
  "Two clocks can sit at the same character offset in different files.
`org-is-active-clock' compares positions alone and calls them the same
clock, which sends a clock-out to the wrong project.  The two files here
carry each project's own tag on an otherwise identical headline, so they
are the same length and the offsets are guaranteed to collide."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "Task" nil "atag"))
        ("beta.org" . ,(org-clock-projects-test-entry "Task" nil "btag")))
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
    (should (equal (org-clock-projects-test-closed-lines "beta.org") '()))))


(ert-deftest org-clock-projects-test-a-task-selection-scopes-its-own-file ()
  "`org-clock-in' with a selection clocks in another file than the current one."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
          (should (equal (org-clock-projects-test-questions)
                         org-clock-projects-test-boundary-questions)))))
    (should (equal (org-clock-projects-test-current) "Beta Project"))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))))

(ert-deftest org-clock-projects-test-an-explicit-start-time-keeps-the-invariant ()
  "Clocking in at a chosen time still leaves one clock in the project."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
        (should (equal (org-clock-projects-test-questions)
                       '("Stop the clock running in Beta Project? "))))
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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (let ((called nil))
      (cl-letf (((symbol-function 'org-clock-goto)
                 (lambda (&rest _) (interactive) (setq called t))))
        (org-clock-projects-test-with-choice nil
          (org-clock-projects-switch)))
      (should called))))

(ert-deftest org-clock-projects-test-switch-with-one-clock-does-not-prompt ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-switch))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    ;; Nothing was ended by switching.
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "gamma.org")) 1))))

(ert-deftest org-clock-projects-test-switch-offers-annotated-project-names ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 90)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-switch)
      ;; Candidates are project names, in start-time order, and the order is
      ;; pinned so a completion UI cannot re-sort them.
      (should (equal (org-clock-projects-test-candidates) '("Alpha Project" "Beta Project")))
      (should (eq (org-clock-projects-test-metadata 'display-sort-function)
                  #'identity))
      ;; The category must not be one an annotation package recognises, or its
      ;; own annotations would replace these.
      (should (eq (org-clock-projects-test-metadata 'category)
                  'org-clock-projects--clock))
      (let ((annotation (org-clock-projects-test-annotation "Alpha Project")))
        (should (string-match-p "A one" annotation))
        (should (string-match-p "1:30" annotation))
        (should (string-match-p (format-time-string
                                 "%H:%M" (org-clock-projects-test-time 90))
                                annotation))))))


;;;; Clocking out everything

(ert-deftest org-clock-projects-test-clock-out-all-closes-every-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 18)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 9))))
    (org-clock-projects-test-with-warnings
      (org-clock-projects-test-with-answers '()
        (org-clock-projects-clock-out-all))
      ;; Both clocks are dangling in files of projects this session never
      ;; enrolled, so both belong to the nil scope and it is the line
      ;; assertions below that say which one was left alone.
      (should (equal (org-clock-projects-test-warnings)
                     '("Left open, the end time would precede the start: no project"))))
    ;; The bad clock is left alone rather than written as a negative duration.
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-closed-lines "beta.org")) 1))))


(ert-deftest org-clock-projects-test-clock-out-all-skips-a-rounded-back-end ()
  "The realistic negative duration: rounding pulls the end before the start.
With quarter-hour rounding a clock started a few minutes ago rounds its
end back past its own start, which is how a bad duration actually arises."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 12 5)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 11))))
    (setq org-clock-rounding-minutes 15)
    (org-clock-projects-test-with-warnings
      ;; 12:07 rounds to 12:00, which precedes alpha's 12:05 start.
      (org-clock-projects-test-at (org-clock-projects-test-hour 12 7)
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-clock-out-all)))
      (should (equal (org-clock-projects-test-warnings)
                     '("Left open, the end time would precede the start: no project"))))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (let ((line (car (org-clock-projects-test-closed-lines "beta.org"))))
      (should (string-match-p "12:00\\]" (org-clock-projects-test-end-of line)))
      (should (string-match-p "=> +1:00" line)))))


;;;; Clearing

(ert-deftest org-clock-projects-test-clear-clocks-out-when-asked ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 60)
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-clear)))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))))

(ert-deftest org-clock-projects-test-clear-keeps-a-declined-clock-unscoped ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-test-with-answers '(nil)
        (org-clock-projects-clear)))
    ;; The time is preserved even though the project is gone, and the clock
    ;; left without a home becomes the active one.
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    (should (= (length (org-clock-projects-test-open-lines "beta.org")) 1))))

(ert-deftest org-clock-projects-test-clear-warns-when-a-clock-is-already-unscoped ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    ;; Leave alpha running, and cross into unscoped work alongside it.
    (org-clock-projects-test-with-answers '(nil t)
      (org-clock-projects-test-clock-in "todo.org" "Something else"))
    (org-clock-projects-test-with-warnings
      ;; Only alpha's own clock is offered: the unscoped one is no project's.
      (org-clock-projects-test-with-choice "Alpha Project"
        (org-clock-projects-test-with-answers '(nil)
          (org-clock-projects-clear)))
      (should (= (length (org-clock-projects-test-warnings)) 1))
      (should (string-match-p "Alpha Project"
                              (car (org-clock-projects-test-warnings)))))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))))

(ert-deftest org-clock-projects-test-clear-forgets-the-stored-clock-out-time ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      (org-clock-projects-test-with-choice "Alpha Project"
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-clear)))
      ;; Enrolling alpha again enrols a project with no stored time, so the
      ;; clock starts now rather than bridging back to the cleared one.
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-test-clock-in "alpha.org" "A two")
        (should (equal (org-clock-projects-test-questions)
                       '("Enrol clock project Alpha Project? "))))
      (should-not (equal (org-clock-projects-test-start-of
                          (car (org-clock-projects-test-open-lines "alpha.org")))
                         alpha-end)))))

(ert-deftest org-clock-projects-test-clear-all-returns-to-the-vanilla-state ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      (should (equal (org-clock-projects-test-questions)
                     '("Clock out B one (12:30, 0:30) in Beta Project? "
                       "Clock out A one (12:00, 1:00) in Alpha Project? "))))
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 9)))
        ("beta.org" . ,(org-clock-projects-test-clock-at
                        "B one" (org-clock-projects-test-hour 10))))
    (should-not (org-clock-is-active))
    ;; Neither project is enrolled, so both clocks are the nil scope's and
    ;; each is told apart by its heading rather than by a project name.
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-kill-emacs-query)
      (should (equal (org-clock-projects-test-questions)
                     '("Clock out no project: A one (09:00, 3:00)? "
                       "Clock out no project: B one (10:00, 2:00)? "))))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (equal (org-clock-projects-test-open-lines "beta.org") '()))))

(ert-deftest org-clock-projects-test-exit-leaves-declined-clocks-open ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (should (equal org-clock-projects-mode-line-string ""))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "alpha.org" "A one"))
    (should (equal org-clock-projects-mode-line-string ""))))

(ert-deftest org-clock-projects-test-mode-line-counts-other-clocks ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (should (equal org-clock-projects-mode-line-string " ⏱ Alpha Project "))
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one")
      (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project [+1] "))
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "gamma.org" "G one")
      (should (equal org-clock-projects-mode-line-string " ⏱ Gamma Project [+2] ")))))

(ert-deftest org-clock-projects-test-mode-line-survives-clocking-out ()
  "A project outlives its clocks."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (should (equal org-clock-projects-mode-line-string " ⏱ Alpha Project "))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))))


;;;; The resolver
;;
;; Which registry the package reads is visible in ownership: an entry whose
;; tag the registry declares is offered for enrolment, and one whose tag it
;; does not is a crossing into unscoped work.

(ert-deftest org-clock-projects-test-default-resolver-reads-the-variable ()
  "The registry the variable holds is the registry ownership is read from."
  (org-clock-projects-test-with-data
      (list (car org-clock-projects-test-projects))
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (org-clock-projects-test-advance 30)
    ;; Beta is declared nowhere, so its entry belongs to no project and the
    ;; crossing is into unscoped work rather than an offer to enrol it.
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "beta.org" "B one")
      (should (equal (org-clock-projects-test-questions)
                     '("Stop the clock running in Alpha Project? "
                       "Leave Alpha Project for unscoped work? "))))
    (should (null (org-clock-projects-test-current)))))

(ert-deftest org-clock-projects-test-a-custom-resolver-is-used ()
  "One registry can serve two packages.
The resolver here returns records in the layout `agile-gtd' keeps, `:key'
included: the key is another package's extension, tolerated on the way in
and of no consequence here.  The entries carry their tags on their own
headlines, so ownership is decided by the resolver's registry alone."
  (org-clock-projects-test-with-data
      nil
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one" nil "atag"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one" nil "btag")))
    (setq org-clock-projects-projects-function
          (lambda () '((:tag "btag" :name "Beta Project" :file "beta.org"
                             :key ?b))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    ;; The resolver's `:name' is what every surface calls the project.
    (should (equal (org-clock-projects-test-current) "Beta Project"))
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project "))
    (org-clock-projects-test-advance 30)
    ;; Alpha's tag is in no registry the resolver returns, so its entry is
    ;; owned by nobody.
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "alpha.org" "A one")
      (should (equal (org-clock-projects-test-questions)
                     '("Stop the clock running in Beta Project? "
                       "Leave Beta Project for unscoped work? "))))
    (should (null (org-clock-projects-test-current)))))

(ert-deftest org-clock-projects-test-a-bad-registry-entry-warns-once ()
  "One bad line costs its own project, never the session, and says so once.
Warning on every read would fire on every clock-in and every modeline
refresh; a registry that has changed is worth complaining about again."
  (org-clock-projects-test-with-data
      '("solo.org"
        (:name "Nameless")
        (:tag "atag" :name "Alpha Project" :file "alpha.org"))
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one")))
    (org-clock-projects-test-with-warnings
      (org-clock-projects-test-with-answers '()
        (org-clock-projects-test-fork "alpha.org" "A one"))
      (let ((warnings (org-clock-projects-test-warnings)))
        (should (= (length warnings) 2))
        ;; The entry is shown as it was written, beside the form that would
        ;; have been accepted, so a registry still written as a list of file
        ;; names explains its own migration.
        (should (string-match-p "\"solo\\.org\"" (nth 0 warnings)))
        (should (string-match-p ":tag \"acme\"" (nth 0 warnings)))
        (should (string-match-p "Nameless" (nth 1 warnings)))))
    ;; The good entry survived both bad ones.
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    ;; Reading the same registry again says nothing more.
    (org-clock-projects-test-with-warnings
      (org-clock-projects-project-records)
      (org-clock-projects-project-records)
      (should (equal (org-clock-projects-test-warnings) '())))
    ;; A registry that has moved starts the warnings over.
    (setq org-clock-projects-projects
          (cons '(:tag "" :name "Empty") org-clock-projects-projects))
    (org-clock-projects-test-with-warnings
      (org-clock-projects-project-records)
      (should (= (length (org-clock-projects-test-warnings)) 3)))))

(ert-deftest org-clock-projects-test-a-tag-declared-twice-warns-and-counts-once ()
  "The first entry claiming a tag wins, as it does everywhere ownership is
decided in registry order, and the second explains itself."
  (org-clock-projects-test-with-data
      '((:tag "atag" :name "Alpha Project" :file "alpha.org")
        (:tag "atag" :name "Alpha Again" :file "other.org")
        (:tag "btag" :name "Beta Project" :file "beta.org"))
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-warnings
      (org-clock-projects-test-with-answers '()
        (org-clock-projects-test-fork "alpha.org" "A one")
        (org-clock-projects-test-advance 30)
        (org-clock-projects-test-fork "beta.org" "B one"))
      (should (= (length (org-clock-projects-test-warnings)) 1))
      (should (string-match-p "Alpha Again"
                              (car (org-clock-projects-test-warnings)))))
    ;; Two projects, two clocks — the repeated tag added neither.
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project [+1] "))
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-switch)
      (should (equal (org-clock-projects-test-candidates)
                     '("Alpha Project" "Beta Project"))))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))))

;;;; Tag identity
;;
;; The tag is what a project is; the file set only bounds the scans.  So one
;; file can hold two projects, and moving a tag moves the clock on it.

(ert-deftest org-clock-projects-test-two-projects-share-one-file ()
  "Each tag holds its own open clock, however the files are laid out."
  (org-clock-projects-test-with-data
      '((:tag "atag" :name "Alpha Project" :file "work.org")
        (:tag "btag" :name "Beta Project" :file "work.org"))
      `(("work.org" . ,(concat (org-clock-projects-test-entry "A one" nil "atag")
                               (org-clock-projects-test-entry "B one" nil "btag"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "work.org" "A one")
      (org-clock-projects-test-advance 30)
      ;; Beta holds no clock of its own, so there is nothing to ask about.
      (org-clock-projects-test-fork "work.org" "B one")
      (should (equal (org-clock-projects-test-questions) '())))
    (should (= (length (org-clock-projects-test-open-lines "work.org")) 2))
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project [+1] "))
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-switch)
      (should (equal (org-clock-projects-test-candidates)
                     '("Alpha Project" "Beta Project"))))
    (should (equal (org-clock-projects-test-active-heading) "A one"))))

(ert-deftest org-clock-projects-test-retagging-moves-an-open-clock-between-projects ()
  "The tag is the identity, so the file layout is free to change under it.
Both projects here keep their time in one file.  Moving the tag on an
entry moves the open clock on it into the other project's namespace,
where it shows up as the second clock that project now holds."
  (org-clock-projects-test-with-data
      '((:tag "atag" :name "Alpha Project" :file "work.org")
        (:tag "btag" :name "Beta Project" :file "work.org"))
      `(("work.org" . ,(concat (org-clock-projects-test-entry "B one" nil "btag")
                               (org-clock-projects-test-entry "A one" nil "atag"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "work.org" "B one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "work.org" "A one"))
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-switch)
      (should (equal (org-clock-projects-test-candidates)
                     '("Beta Project" "Alpha Project"))))
    (save-current-buffer
      (org-clock-projects-test-goto "work.org" "A one")
      (org-set-tags '("btag")))
    ;; Both clocks answer to beta now, so each is named by its heading.
    (org-clock-projects-test-with-choice "Beta Project: A one"
      (org-clock-projects-switch)
      (should (equal (org-clock-projects-test-candidates)
                     '("Beta Project: B one" "Beta Project: A one"))))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    (should (equal (org-clock-projects-test-current) "Beta Project"))))

(ert-deftest org-clock-projects-test-enrolling-adopts-an-already-open-clock ()
  "A clock's scope is derived from its entry and stored nowhere, so a
project governs the clock that was started before the project existed."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one")
                                (org-clock-projects-test-entry "A two"))))
    ;; Dormant, so this clock belongs to nobody and nothing is displayed.
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-clock-in "alpha.org" "A one")
      (should (equal (org-clock-projects-test-questions) '())))
    (should (equal org-clock-projects-mode-line-string ""))
    (org-clock-projects-test-advance 30)
    ;; Enrolling alpha adopts it, and the collision the fork runs into is
    ;; with the clock that was running before there was a project to hold it.
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-fork "alpha.org" "A two")
      (should (equal (org-clock-projects-test-questions)
                     '("Alpha Project already has A one (12:00, 0:30); \
clock out and start here? "))))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 1))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    (should (equal org-clock-projects-mode-line-string " ⏱ Alpha Project "))))


;;;; The unscoped scope
;;
;; The nil scope is a peer: one open clock, its own clock-out time, and always
;; visible.  No project declares `todo.org', so no scan reaches the clock
;; there; it stays visible through the marker the nil scope remembers.

(defmacro org-clock-projects-test-with-unscoped-clock (&rest body)
  "Open an unscoped clock in `todo.org', fork alpha beside it, run BODY.
The unscoped clock is started while the session is still dormant, which
is the transition the memory exists for, and alpha's fork then takes the
active clock away from it.  Nothing scans `todo.org', so from there on
the remembered marker is the only way that clock is reachable at all."
  (declare (indent 0) (debug t))
  `(org-clock-projects-test-with-data
       org-clock-projects-test-projects
       `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
         ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
     (org-clock-projects-test-with-answers '()
       (org-clock-projects-test-clock-in "todo.org" "Something else"))
     (org-clock-projects-test-advance 30)
     (org-clock-projects-test-with-answers '()
       (org-clock-projects-test-fork "alpha.org" "A one"))
     ,@body))

(ert-deftest org-clock-projects-test-an-inactive-unscoped-clock-reaches-the-switcher ()
  (org-clock-projects-test-with-unscoped-clock
    (should (equal org-clock-projects-mode-line-string " ⏱ Alpha Project [+1] "))
    (org-clock-projects-test-with-choice "no project"
      (org-clock-projects-switch)
      (should (equal (org-clock-projects-test-candidates)
                     '("no project" "Alpha Project"))))
    (should (equal (org-clock-projects-test-active-heading) "Something else"))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))))

(ert-deftest org-clock-projects-test-clock-out-all-closes-the-unscoped-clock ()
  (org-clock-projects-test-with-unscoped-clock
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-clock-out-all))
    (should (equal (org-clock-projects-test-open-lines "todo.org") '()))
    (should (= (length (org-clock-projects-test-closed-lines "todo.org")) 1))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))))

(ert-deftest org-clock-projects-test-the-exit-query-offers-the-unscoped-clock ()
  "The exit query is the last line of defence, and the unscoped clock is
the one it would be easiest to leave running forever."
  (org-clock-projects-test-with-unscoped-clock
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-kill-emacs-query)
      (should (equal (org-clock-projects-test-questions)
                     '("Clock out no project: Something else (12:00, 1:00)? "
                       "Clock out Alpha Project: A one (12:30, 0:30)? "))))
    (should (equal (org-clock-projects-test-open-lines "todo.org") '()))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))))

(ert-deftest org-clock-projects-test-a-killed-buffer-drops-the-remembered-clock ()
  "The memory may never manufacture an entry.
Once the buffer is gone the marker describes nothing, and a switcher
entry pointing at it would clock out whatever text moved under it."
  (org-clock-projects-test-with-unscoped-clock
    (with-current-buffer (org-clock-projects-test-buffer "todo.org")
      (save-buffer)
      (kill-buffer))
    ;; One clock left, so the switcher goes straight there: a nil choice
    ;; makes any completion prompt an error.
    (org-clock-projects-test-with-choice nil
      (org-clock-projects-switch))
    (should (equal (org-clock-projects-test-active-heading) "A one"))
    ;; The clock line itself is untouched.  A clock in a file no project
    ;; declares is untracked until that file is visited again — the
    ;; limitation the README records, not something the memory may paper
    ;; over by inventing an entry for it.
    (should (= (length (org-clock-projects-test-open-lines "todo.org")) 1))))

(ert-deftest org-clock-projects-test-a-closed-line-drops-the-remembered-clock ()
  "A clock line closed by another hand leaves nothing to remember."
  (org-clock-projects-test-with-unscoped-clock
    (with-current-buffer (org-clock-projects-test-buffer "todo.org")
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\([ \t]*CLOCK: \\[[^]]+\\]\\)$")
        (replace-match (concat (match-string 1) "--"
                               (org-clock-projects-test-stamp) " =>  0:30")
                       t t)))
    (should (equal (org-clock-projects-test-open-lines "todo.org") '()))
    (org-clock-projects-test-with-choice nil
      (org-clock-projects-switch))
    (should (equal (org-clock-projects-test-active-heading) "A one"))))

(ert-deftest org-clock-projects-test-an-unowned-entry-in-a-project-file-says-so ()
  "The forgotten-tag case, said out loud where it is cheapest to fix.
Alpha's file carries its tag on the entries that are alpha's, so the
stray one beside them belongs to nobody, and time clocked there would be
missing from the invoice without a word."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(concat (org-clock-projects-test-entry "A one" nil "atag")
                                (org-clock-projects-test-entry "A stray"))))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-messages
      (org-clock-projects-test-with-answers '(t t)
        (org-clock-projects-test-clock-in "alpha.org" "A stray")
        (should (equal (org-clock-projects-test-questions)
                       '("Stop the clock running in Alpha Project? "
                         "Leave Alpha Project for unscoped work? "))))
      (should (seq-find (lambda (text)
                          (string-match-p "\\`Clocked in unscoped in .*alpha\\.org\\'"
                                          text))
                        (org-clock-projects-test-messages))))
    (should (null (org-clock-projects-test-current)))))

(ert-deftest org-clock-projects-test-the-nil-scope-clocks-like-stock-org ()
  "Registration alone never affects clocking.
From the nil scope an entry of a registered but unenrolled project is
clocked exactly as stock Org clocks it: no question, no message, and the
session stays unscoped.  Its time is still selected by that project's tag
at reporting, so the silence loses nothing."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else"))
        ("gamma.org" . ,(org-clock-projects-test-entry "G one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-answers '(t t)
      (org-clock-projects-test-clock-in "todo.org" "Something else"))
    (should (null (org-clock-projects-test-current)))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-messages
      (org-clock-projects-test-with-answers '()
        (org-clock-projects-test-clock-in "gamma.org" "G one")
        (should (equal (org-clock-projects-test-questions) '())))
      ;; Gamma's entry is owned, only unenrolled, so nothing is said about it.
      (should-not (seq-find (lambda (text) (string-match-p "unscoped" text))
                            (org-clock-projects-test-messages))))
    (should (null (org-clock-projects-test-current)))
    (should (equal org-clock-projects-mode-line-string ""))
    (should (= (length (org-clock-projects-test-open-lines "gamma.org")) 1))
    ;; The nil scope holds one clock, so the one it held is closed.
    (should (equal (org-clock-projects-test-open-lines "todo.org") '()))))


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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-two-dangling)))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-clock-out-all))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 2))))

(ert-deftest org-clock-projects-test-exit-query-offers-a-surplus-clock ()
  "The exit query is the last line of defence: a clock it never offers is kept."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-two-dangling))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 60)
    (org-clock-projects-test-with-answers '(t t t)
      (org-clock-projects-kill-emacs-query)
      (should (equal (org-clock-projects-test-questions)
                     '("Clock out no project: A one (09:00, 4:00)? "
                       "Clock out no project: A two (10:00, 3:00)? "
                       "Clock out Beta Project: B one (12:00, 1:00)? "))))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))
    (should (equal (org-clock-projects-test-open-lines "beta.org") '()))))

(ert-deftest org-clock-projects-test-clocking-in-repairs-a-surplus-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-two-dangling)))
    (org-clock-projects-test-with-answers '(t)
      (org-clock-projects-test-fork "alpha.org" "A two")
      ;; The prompt says what it is about to close, so a five-minute clock and
      ;; one open since yesterday can be told apart before answering.
      (should (equal (org-clock-projects-test-questions)
                     '("Alpha Project already has A one (09:00, 3:00), \
A two (10:00, 2:00); clock out and start here? "))))
    (should (= (length (org-clock-projects-test-closed-lines "alpha.org")) 2))
    (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1))
    ;; Started, not resumed: leaving the clock at point open would make Org
    ;; read this as a continuation and write nothing at all.
    (should (equal (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "alpha.org")))
                   (org-clock-projects-test-stamp)))))

;;;; Clocks running in the background
;;
;; `org-with-clock' redirects the marker, but `org-clock-out' tears down the
;; display, the timers and the remembered task of *the* clock.

(ert-deftest org-clock-projects-test-a-background-clock-out-spares-the-running-clock ()
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (should (memq 'org-mode-line-string global-mode-string))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-choice "Alpha Project"
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-clear)))
    (should (equal (org-clock-projects-test-active-heading) "B one"))
    (should (equal org-clock-current-task "B one"))
    (should (memq 'org-mode-line-string global-mode-string))))

(ert-deftest org-clock-projects-test-the-running-task-is-still-recognised ()
  "Forgetting the running task makes Org split its clock line instead of
recognising a clock-in on it as the same clock."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 30)
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (org-clock-projects-test-with-choice "Alpha Project"
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
      org-clock-projects-test-projects
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
  "A project's clock-out must not flow into the nil scope.
The nil scope is a scope like any other and continues only from its own
clock-out, so twenty minutes after a project's clock ended — well inside
the continuation threshold — unscoped work still starts now."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("todo.org" . ,(org-clock-projects-test-entry "Something else")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one")
      (org-clock-projects-test-advance 60)
      (org-clock-out))
    (let ((alpha-end (org-clock-projects-test-end-of
                      (car (org-clock-projects-test-closed-lines "alpha.org")))))
      (org-clock-projects-test-advance 20)
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-test-clock-in "todo.org" "Something else")
        (should (equal (org-clock-projects-test-questions)
                       '("Leave Alpha Project for unscoped work? "))))
      (let ((start (org-clock-projects-test-start-of
                    (car (org-clock-projects-test-open-lines "todo.org")))))
        (should-not (equal start alpha-end))
        (should (equal start (org-clock-projects-test-stamp)))))))

(ert-deftest org-clock-projects-test-the-last-clock-out-prefix-stays-in-project ()
  "`C-u C-u C-u' starts from the last clock-out — this project's own."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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
      org-clock-projects-test-projects
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

(ert-deftest org-clock-projects-test-a-failed-clock-in-leaves-the-session-alone ()
  "A clock-in that raises must leave nothing behind: not a line in the
target file, and not the scope pointer it was about to move."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
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
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    (should (equal (org-clock-projects-test-lines "gamma.org") '()))))


(ert-deftest org-clock-projects-test-an-unresumable-clock-line-is-not-doubled ()
  "Org reads a clock line more loosely than it resumes one.
A line without a start time is found by the reader, and the element
parser calls it a clock, but Org's resume branch rejects it and answers
with a *new* clock line instead — so handing one over would turn one open
clock into two.  It is reported and left alone: closing it would invent
the hours since midnight, and resuming it is what Org declines to do."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(concat
                         (format "* TODO A one\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                                 (format-time-string
                                  "[%Y-%m-%d %a]"
                                  (org-clock-projects-test-time)))
                         (org-clock-projects-test-entry "A two")))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type message &rest _) (push message warnings))))
        ;; Stop beta, enrol alpha.  The repair reaches alpha's damaged line.
        (org-clock-projects-test-with-answers '(t t)
          (org-clock-projects-test-clock-in "alpha.org" "A two")))
      (should (= (length warnings) 1))
      (should (string-match-p "A one" (car warnings))))
    ;; The damaged line, untouched, and the one new clock — not a third
    ;; written beside it.
    (should (= (length (org-clock-projects-test-lines "alpha.org")) 2))
    (should (member (format "CLOCK: %s"
                            (format-time-string "[%Y-%m-%d %a]"
                                                (org-clock-projects-test-time 30)))
                    (org-clock-projects-test-lines "alpha.org")))
    (should (equal (org-clock-projects-test-active-heading) "A two"))))

(ert-deftest org-clock-projects-test-switch-refuses-rather-than-pretends ()
  "A switch that cannot happen must not look like one that did.
Otherwise the user goes on believing they changed project while the
previous clock runs, and every minute after that is billed to it."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(format "* TODO A one\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                                (format-time-string
                                 "[%Y-%m-%d %a]" (org-clock-projects-test-time))))
        ("beta.org" . ,(org-clock-projects-test-entry "B one")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "beta.org" "B one"))
    (org-clock-projects-test-advance 30)
    ;; Alpha is registered but was never enrolled, so the clock dangling in
    ;; its file is the nil scope's and is offered under that name.
    (org-clock-projects-test-with-choice "no project"
      (should-error (org-clock-projects-switch) :type 'user-error))
    ;; Nothing moved: the project, the modeline and the running clock all stand.
    (should (equal (org-clock-projects-test-current) "Beta Project"))
    (should (equal org-clock-projects-mode-line-string " ⏱ Beta Project [+1] "))
    (should (equal (org-clock-projects-test-active-file) "beta.org"))
    ;; But point is on the line that needs repairing.
    (should (equal (file-name-nondirectory (buffer-file-name)) "alpha.org"))))

(defconst org-clock-projects-test-clock-shapes
  '("[2026-08-25 Di 09:00]"                       ; plain
    "[2026-08-25 Di. 09:00]"                      ; abbreviating dot
    "[2026-08-25 Mittwoch 09:00]"                 ; full day name
    "[2026-08-25 Di 00:00]"                       ; midnight
    "[2026-08-25 Di 23:59]"                       ; last minute of the day
    "[2026-08-25 Di 9:00]"                        ; single-digit hour
    "[2026-08-25 Di]"                             ; no time at all
    "[2026-08-25 09:00]"                          ; no day name
    "[2026-08-25 Di 09:00 +1d]"                   ; repeater
    "[2026-08-25 Di 09:00 -0d]"                   ; warning cookie
    "[2026-08-25 Di 09:00]--[2026-08-25 Di 10:00]") ; already closed
  "Clock timestamps `org-find-open-clocks' reports as open.
Org resumes some of these in place and answers the rest with a *new*
clock line.  Which is which is deliberately not recorded here.")

(defun org-clock-projects-test--org-resumes-p (stamp)
  "Return non-nil when Org resumes a clock line carrying STAMP in place.
Org is asked rather than predicted: the clock is handed to it with this
package out of the way, and the answer is whether a clock line appeared."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(format "* TODO A one\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                                stamp)))
    (org-clock-projects-mode -1)
    (let ((before (length (org-clock-projects-test-lines "alpha.org")))
          (clock (car (org-find-open-clocks
                       (expand-file-name "alpha.org" org-directory)))))
      (when clock
        (ignore-errors (org-clock-clock-in clock t))
        (= before (length (org-clock-projects-test-lines "alpha.org")))))))

(ert-deftest org-clock-projects-test-the-guard-follows-org ()
  "Whether a clock line can be resumed is Org's answer, not this package's.
The guard mirrors the regexp in `org-clock-in's resume branch, so it can
drift if Org ever changes it.  Rather than pin down which shapes it
accepts, each one is put to Org directly and the switcher is required to
agree: it goes through when Org would resume the line, and refuses when
Org would write a second one instead."
  (dolist (stamp org-clock-projects-test-clock-shapes)
    (let ((org-resumes (org-clock-projects-test--org-resumes-p stamp)))
      (org-clock-projects-test-with-data
          org-clock-projects-test-projects
          `(("alpha.org" . ,(format "* TODO A one\n:LOGBOOK:\nCLOCK: %s\n:END:\n"
                                    stamp))
            ("beta.org" . ,(org-clock-projects-test-entry "B one")))
        (org-clock-projects-test-with-answers '()
          (org-clock-projects-test-fork "beta.org" "B one"))
        (org-clock-projects-test-advance 30)
        (let ((switched (org-clock-projects-test-with-choice "no project"
                          (condition-case nil
                              (progn (org-clock-projects-switch) t)
                            (user-error nil)))))
          (should (eq switched (and org-resumes t)))
          ;; Either way, no second clock line was written beside the first.
          (should (= (length (org-clock-projects-test-lines "alpha.org")) 1))
          (should (equal (org-clock-projects-test-active-file)
                         (if switched "alpha.org" "beta.org"))))))))

(ert-deftest org-clock-projects-test-a-narrowed-buffer-hides-nothing ()
  "Narrowing is routine in Org, and every path here reads one scan.
`org-find-open-clocks' searches from `point-min' without widening, so a
narrowed buffer would hide its own clocks from the invariant, the bulk
clock-out, the exit query, the switcher and the modeline at once."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(concat (org-clock-projects-test-clock-at
                                 "A one" (org-clock-projects-test-hour 9))
                                "* TODO A two\n")))
    (should (= (length (org-clock-projects-open-clocks)) 1))
    (with-current-buffer (org-clock-projects-test-buffer "alpha.org")
      (goto-char (point-min))
      (re-search-forward "^\* TODO A two")
      (org-narrow-to-subtree))
    (should (= (length (org-clock-projects-open-clocks)) 1))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-clock-out-all))
    ;; Read the file back widened, since the buffer under test is not.
    (with-current-buffer (org-clock-projects-test-buffer "alpha.org")
      (should (equal (save-restriction
                       (widen)
                       (org-clock-projects-test-open-lines "alpha.org"))
                     '())))))

(ert-deftest org-clock-projects-test-disabling-hands-the-exit-query-back ()
  "Turning the mode off must not leave the session with no exit query at all."
  (let ((kill-emacs-query-functions kill-emacs-query-functions)
        (org-clock-ask-before-exiting t))
    (org-clock-projects-mode 1)
    (should (memq #'org-clock-projects-kill-emacs-query kill-emacs-query-functions))
    (should-not (memq #'org-clock-kill-emacs-query kill-emacs-query-functions))
    (org-clock-projects-mode -1)
    (should-not (memq #'org-clock-projects-kill-emacs-query
                      kill-emacs-query-functions))
    (should (memq #'org-clock-kill-emacs-query kill-emacs-query-functions))))

(ert-deftest org-clock-projects-test-being-asked-at-all-stays-the-users ()
  "`org-clock-ask-before-exiting' nil means no question, not a different one."
  (let ((kill-emacs-query-functions kill-emacs-query-functions)
        (org-clock-ask-before-exiting nil))
    (org-clock-projects-mode 1)
    (unwind-protect
        (should-not (memq #'org-clock-projects-kill-emacs-query
                          kill-emacs-query-functions))
      (org-clock-projects-mode -1))))

(ert-deftest org-clock-projects-test-a-failed-repair-enrols-no-project ()
  "The rollback has to cover the repair, not just the clock-in.
Closing a surplus clock can fail, and a project enrolled for a clock-in
that never happened suppresses that project's boundary question ever
after — so the question being asked a second time is what says the
rollback held."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-entry "A one"))
        ("gamma.org" . ,(concat (org-clock-projects-test-clock-at
                                 "G one" (org-clock-projects-test-hour 9))
                                (org-clock-projects-test-clock-at
                                 "G two" (org-clock-projects-test-hour 10))
                                "* TODO G three\n")))
    (org-clock-projects-test-with-answers '()
      (org-clock-projects-test-fork "alpha.org" "A one"))
    (org-clock-projects-test-advance 30)
    (with-current-buffer (org-clock-projects-test-buffer "gamma.org")
      (setq buffer-read-only t))
    (let (questions)
      (org-clock-projects-test-with-answers '(t t)
        (should-error
         (save-current-buffer
           (org-clock-projects-test-goto "gamma.org" "G three")
           (org-clock-in)))
        (setq questions (org-clock-projects-test-questions)))
      (should (equal questions '("Stop the clock running in Alpha Project? "
                                 "Enrol clock project Gamma Project? "))))
    (should (equal (org-clock-projects-test-current) "Alpha Project"))
    ;; Gamma was never enrolled, and the way to see that is that crossing
    ;; into it is still an open question: an enrolled project is not asked
    ;; about again.
    (with-current-buffer (org-clock-projects-test-buffer "gamma.org")
      (setq buffer-read-only nil))
    (let (questions)
      (org-clock-projects-test-with-answers '(t nil)
        (should-error (org-clock-projects-test-clock-in "gamma.org" "G three")
                      :type 'user-error)
        (setq questions (org-clock-projects-test-questions)))
      (should (equal questions '("Stop the clock running in Alpha Project? "
                                 "Enrol clock project Gamma Project? "))))))

(ert-deftest org-clock-projects-test-the-exit-option-is-read-when-asked ()
  "Turning the option off should take effect without cycling the mode."
  (org-clock-projects-test-with-data
      org-clock-projects-test-projects
      `(("alpha.org" . ,(org-clock-projects-test-clock-at
                         "A one" (org-clock-projects-test-hour 9))))
    (let ((org-clock-ask-before-exiting nil))
      (org-clock-projects-test-with-answers '()
        (should (eq (org-clock-projects-kill-emacs-query) t))
        (should (equal (org-clock-projects-test-questions) '())))
      ;; And the clock is left exactly as it was.
      (should (= (length (org-clock-projects-test-open-lines "alpha.org")) 1)))
    (let ((org-clock-ask-before-exiting t))
      (org-clock-projects-test-with-answers '(t)
        (org-clock-projects-kill-emacs-query)
        (should (= (length (org-clock-projects-test-questions)) 1))))
    (should (equal (org-clock-projects-test-open-lines "alpha.org") '()))))

(provide 'org-clock-projects-test)
;;; org-clock-projects-test.el ends here
