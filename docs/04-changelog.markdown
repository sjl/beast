Changelog
=========

Here's the list of changes in each released version.

[TOC]

v1.1.0
------

Fixed a bunch of loading-related stuff to make Beast play nicer with Quicklisp's
internal loading/building process.

Changed an `etypecase` to a `ctypecase` in `define-aspect`'s field-cleaning
procedure.  Now if you try to define an aspect with something stupid as a field
like `(define-aspect foo "this is not a symbol or list")` you'll have a restart
available to fix the problem, instead of just crashing and burning.

v1.0.0
------

Initial version.  It works, but changes may happen in the future.
