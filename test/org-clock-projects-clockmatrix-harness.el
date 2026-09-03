;;; org-clock-projects-clockmatrix-harness.el --- Clockmatrix fixtures -*- lexical-binding: t; -*-

;; The matrix sandbox is shared by the clockmatrix suite and by the range
;; keywords, which are driven through the block that accepts them.  It sits
;; apart from both because the runner loads every file under test/ by path:
;; requiring a file that defines tests would load those tests twice, and ERT
;; refuses a test it has already seen.  Helpers can be loaded twice without
;; harm.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-clock-projects)

;;; Fixtures

(defconst org-clock-projects-clockmatrix-test-projects
  '((:tag "alpha" :name "Alpha")
    (:tag "beta"  :name "Beta")
    (:tag "gamma" :name "Gamma"))
  "Projects registered for most clockmatrix tests.")

(defconst org-clock-projects-clockmatrix-test-files
  '(("alpha.org" . "\
* TODO Alpha task :alpha:
:LOGBOOK:
CLOCK: [2026-05-04 Mon 09:00]--[2026-05-04 Mon 11:00] =>  2:00
CLOCK: [2026-06-02 Tue 09:00]--[2026-06-02 Tue 12:30] =>  3:30
:END:
")
    ("archive/alpha.org" . "\
* DONE Archived alpha task :alpha:
:LOGBOOK:
CLOCK: [2026-05-11 Mon 09:00]--[2026-05-11 Mon 10:15] =>  1:15
:END:
")
    ("beta.org" . "\
* TODO Beta task :beta:
:LOGBOOK:
CLOCK: [2026-05-05 Tue 14:00]--[2026-05-05 Tue 15:15] =>  1:15
CLOCK: [2026-05-20 Wed 10:00]--[2026-05-20 Wed 10:30] =>  0:30
:END:
")
    ("gamma.org" . "\
* TODO Gamma task :gamma:
:LOGBOOK:
CLOCK: [2025-11-03 Mon 09:00]--[2025-11-03 Mon 10:45] =>  1:45
:END:
"))
  "Synthetic project files with known clocked time.

Totals for 2026: alpha 6:45 (2:00 + 3:30 in its own file, 1:15 in its
archive), beta 1:45, gamma nothing.  Gamma's only clock is in 2025.")

(defvar org-clock-projects-clockmatrix-test-registry nil
  "The projects the sandbox registers, read back by its resolver.")

(defun org-clock-projects-clockmatrix-test-projects-function ()
  "Return the projects the sandbox registered.
The registry is reached the way any other one is, through
`org-clock-projects-projects-function', so the tests exercise the seam a
real registry is attached to."
  org-clock-projects-clockmatrix-test-registry)

(defun org-clock-projects-clockmatrix-test-kill-buffers (dir)
  "Kill all buffers visiting a file under DIR."
  (dolist (buffer (buffer-list))
    (let ((file (buffer-file-name buffer)))
      (when (and file (string-prefix-p (file-name-as-directory dir) file))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(defmacro org-clock-projects-clockmatrix-test-with-data (projects files &rest body)
  "Run BODY with PROJECTS registered and FILES written into a sandbox.

PROJECTS is a list of project records.  FILES is an
alist of (RELATIVE-NAME . CONTENT); intermediate directories are created.
Durations render as `h:mm' and the day begins at midnight, so assertions
depend on neither the ambient `org-duration-format' nor
`org-extend-today-until'.  Archives resolve inside the sandbox, since
where the matrix finds them is part of what is under test."
  (declare (indent 2) (debug t))
  `(let* ((sandbox (make-temp-file "org-clock-projects-clockmatrix-test-" t))
          (org-directory sandbox)
          (org-agenda-files nil)
          (org-duration-format 'h:mm)
          (org-extend-today-until 0)
          (org-todo-keywords '((sequence "TODO" "DONE")))
          (org-use-tag-inheritance t)
          (org-tags-exclude-from-inheritance nil)
          (org-archive-location
           (expand-file-name "archive/%s::datetree" sandbox))
          (org-clock-projects-projects-function
           #'org-clock-projects-clockmatrix-test-projects-function)
          (org-clock-projects-clockmatrix-test-registry ,projects)
          ;; A skipped registry entry warns once per entry and per registry.
          ;; The bookkeeping that makes it once outlives the block that
          ;; rendered, so unbound here one test's warning would be the reason
          ;; the next test sees none.
          (org-clock-projects--last-registry nil)
          (org-clock-projects--warned-entries nil)
          ;; The nil scope's remembered clock is session state like any
          ;; other, and a matrix test has no reason to inherit one.
          (org-clock-projects--unscoped-clock nil))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,content) ,files)
             (let ((path (expand-file-name name sandbox)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert content))))
           ,@body)
       (org-clock-projects-clockmatrix-test-kill-buffers sandbox)
       (delete-directory sandbox t))))

(defun org-clock-projects-clockmatrix-test-render (params)
  "Insert a clockmatrix block with PARAMS, refresh it, return the block text.

PARAMS may be written across several lines for readability; it is folded
onto the single `#+BEGIN:' line."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: clockmatrix "
            (replace-regexp-in-string "[ \t\n]+" " " params)
            "\n#+END:\n")
    (goto-char (point-min))
    (org-update-dblock)
    (buffer-string)))

(defun org-clock-projects-clockmatrix-test-table (params)
  "Render a clockmatrix block with PARAMS and return its table as Lisp.

Rows are lists of trimmed cell strings; horizontal rules are `hline'."
  (let ((text (org-clock-projects-clockmatrix-test-render params)))
    (with-temp-buffer
      (org-mode)
      (insert text)
      (goto-char (point-min))
      (forward-line 1)
      (and (org-at-table-p) (org-table-to-lisp)))))

(defun org-clock-projects-clockmatrix-test-warnings (params)
  "Render a clockmatrix block with PARAMS, returning the warnings it emits."
  (let ((warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      (org-clock-projects-clockmatrix-test-table params))
    (nreverse warnings)))

(provide 'org-clock-projects-clockmatrix-harness)
;;; org-clock-projects-clockmatrix-harness.el ends here
