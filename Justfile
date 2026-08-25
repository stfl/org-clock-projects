default:
    @just --list

build:
    eask recompile

test:
    eask run script test

lint:
    eask lint package org-clock-projects.el
