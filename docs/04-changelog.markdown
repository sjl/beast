Changelog
=========

Here's the list of changes in each released version.

[TOC]

v1.2.0
------

Refactored a chunk of the code to do a few things:

1. Removed the (vendored) dependency on Quickutils.
2. Removed dependency on Roswell for running unit tests.
3. All system-running functions are expanded into `ARITY` nested loops, not just
   those with arity 2 or smaller.
4. Modernized the file/directory structure to match my recent projects.
5. Added more unit tests to cover parts of the code that weren't being tested
   before.
6. The internal system argument indexes are now vectors instead of lists.
7. Exported `all-entities` for debugging.

Other than the new `all-entities` function nothing user-visible should have
changed.  Please file a bug if you see any new or broken behavior.

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
