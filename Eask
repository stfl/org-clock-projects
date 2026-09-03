;; -*- mode: eask; lexical-binding: t; -*-

(package "org-clock-projects"
         "0.3.0"
         "Per-project parallel Org clocks")

(website-url "https://github.com/stfl/org-clock-projects")
(keywords "calendar" "convenience")
(package-file "org-clock-projects.el")

(files "org-clock-projects.el")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "29.1")
(depends-on "org" "9.6")
(depends-on "org-clock-csv")

;; Stale bytecode is loaded in preference to newer source, so a test run can
;; silently exercise the last compile rather than the working tree.  The sweep
;; leaves .eask alone: that is the dependencies' own bytecode, not ours.
(script "test" "find . -maxdepth 2 -name '*.elc' -not -path './.eask/*' -delete && eask test ert test/*.el")
