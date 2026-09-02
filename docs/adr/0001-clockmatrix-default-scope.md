# 1. Clockmatrix default scope: per-project files, not a full agenda scan

## Status

Accepted

## Context

The `clockmatrix` dynamic block renders a table of clocked time with calendar periods
down the rows and projects across the columns. Each cell is one clock-sum over a
(period, project) pair, so the number of file scans the block performs is periods ×
projects × files scanned per cell. That multiplier makes file scope the dominant cost
of a refresh.

A 12-period × 4-project grid (48 cells) scanning the full agenda file set — around 27
files — measures roughly 14 seconds to render. Scoping each project's cells to only
that project's own two files (its main file and its archive) brings the same grid
under a second. `clockmatrix` refreshes with `C-c C-c` like any Org dynamic block, and
a multi-second wait on every refresh defeats the point of an interactive report.

## Decision

The default scope, per project, is that project's own file plus its archives,
resolved with `org-add-archive-files`. That reads `org-archive-location`, honours a
subtree's own `:ARCHIVE:` property, and drops archives that do not exist, so the scope
follows whatever archive layout the Org configuration actually declares rather than a
path this package guesses. The project's own file comes from the record the project
resolver returns.

`:scope agenda-with-archives` selects `(org-add-archive-files (org-agenda-files t))`
as an explicit, opt-in audit path that scans the full agenda file set instead.

A related decision governs missing files: a project whose *main* file is absent is
a misconfiguration, not an absence of clocked time, and the block warns, naming the
project, the path it tried, and both remedies — set `:file` on its entry in the
project registry, or use `:scope agenda-with-archives`. A missing *archive* file produces no warning,
because archives are routinely absent by design; most projects never accumulate one.

## Consequences

Refresh is fast enough to run interactively at the file scope that makes the report
useful in the first place.

The default scope has a known failure mode: clocked time that carries a project's tag
but lives in a file other than that project's own — for example, an item captured to
an inbox, clocked there, and not yet refiled — is invisible under the default scope,
and produces no error. On the dataset this was designed against, the two scopes agree
exactly, so divergence is zero today, but that agreement is a property of that data,
not an invariant the default scope guarantees. The remedy is `:scope
agenda-with-archives`, which trades the speed of the default for a scan wide enough to
catch tagged-but-unrefiled entries anywhere in the agenda.

A missing main file surfaces as a named warning rather than a silent zero-hours
report, so a misregistered project (most commonly, a project whose file is not named
after its tag and needs an explicit `:file`) is caught rather than misreported.

## Alternatives considered

**Wide scope for every cell.** Scanning the full agenda file set for every
(period, project) cell is correct — it cannot miss unrefiled entries — but costs
roughly an order of magnitude more time, matching the ~14s measurement above against
the same grid.

**Wide scope plus a per-tag prefilter.** Narrowing the wide file set by tag before
summing lands in between the two extremes on speed, but spends most of its budget
rediscovering the project-to-file mapping the project registry already holds,
making it a worse trade than either scoping directly to project files or scanning
everything.

## Where this decision also applies

`org-clock-projects-export` and `org-clock-projects-check` scope their scan the same
way and for the same reason, with the wide scan behind a prefix argument rather than a
block parameter. The export measures the same order-of-magnitude gap: 1.22s narrow
against 4.81s wide over one project, returning identical rows. Keeping the two scopes
identical is also what lets a matrix cell and a CSV be compared at all.

The export's dominant cost is a different one, and is not addressed here: it gates
entries on the period *before* parsing them, because the parse builds an inherited
property lookup per entry. Gating on the tag first does not help, since a project's tag
is a file-level tag that every entry in the file inherits.
