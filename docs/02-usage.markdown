Usage
=====

Beast aims to be a thin layer over CLOS, and so has a fairly small user-facing
API.

[TOC]

Aspects
-------

Aspects are facets/traits of your game objects that you want to model.  Some
examples could be things like: location, moveable, visible, edible, sentient.

### Defining Aspects

To define an aspect you use `define-aspect`:

    :::lisp
    (define-aspect location x y)
    (define-aspect edible nutrition-value)

`define-aspect` takes the name of the aspect and zero or more slot definitions.

The names of aspect slots will have the aspect name prepended to them with
a slash to avoid clashing between aspects, and the `initargs` and `accessors`
will be added for you.  So for example, this:

    :::lisp
    (define-aspect inspectable text)
    (define-aspect readable text)

Would macroexpand into something roughly like:

    :::lisp
    (defclass inspectable ()
      ((inspectable/text :initarg :inspectable/text
                         :accessor inspectable/text)))

    (defclass readable ()
      ((readable/text :initarg :readable/text
                      :accessor readable/text)))

### Aspect Slot Options

You can include extra slot options when defining an aspect's slots, and they'll
be passed along to the `defclass`.  This is especially handy for `:initform`
and `:type`.

    :::lisp
    (define-aspect container
      (contents :initform nil))

    (define-aspect throwable
      (accuracy :type single-float)
      (damage :type integer))

In the end it's just CLOS though, so if you want to add some `:documentation` or
use `:allocation :class` then go nuts!

### Aspect Type Predicates

When you define an aspect named `foo` Beast also defines a `foo?` predicate that
returns `(typep object 'foo)`, which comes in handy when using higher-order
functions:

    :::lisp
    (defun whats-for-dinner? ()
      (remove-if-not #'edible? (container/contents *fridge*)))

Entities
--------

Once you've got some aspects you'll want to define some entity classes that mix
them together.

### Defining Entities

You define entity classes using `define-entity`:

    :::lisp
    (define-entity dart (throwable))
    (define-entity bread (edible))
    (define-entity pie (edible throwable))
    (define-entity icebox (container))

The resulting classes will inherit from `entity` and each of the aspects (in
order).  This example would expand (roughly) into:

    :::lisp
    (defclass dart (entity throwable) ())
    (defun dart? (object) (typep object 'dart))

    (defclass bread (entity edible) ())
    (defun bread? (object) (typep object 'bread))

    (defclass pie (entity edible throwable) ())
    (defun pie? (object) (typep object 'pie))

    (defclass icebox (entity container) ())
    (defun icebox? (object) (typep object 'icebox))

### Entity Slot Definitions

You can also specify slot definitions at the entity level, and they'll be passed
along to `defclass`:

    :::lisp
    (define-entity cheese (edible)
      (variety :type (member :swiss :cheddar :feta)
               :initarg :variety
               :reader :cheese-variety))

Note that slot definitions on entities are passed along **raw**, without the
name-mangling or default-slot-option-adding that's done for aspects.  This may
change in the future.

### Creating and Destroying Entities

After you've defined your entity classes you can can create some entities using
`create-entity`:

    :::lisp
    (defparameter *my-fridge* (create-entity 'icebox))

    (dotimes (i 30)
      (push (create-entity 'cheese
              :edible/nutrition-value 10
              :variety (nth (random 3) '(:swiss :cheddar :feta)))
            (container/contents *my-fridge*)))

`create-entity` is a thin wrapper around `make-instance` that handles some extra
bookkeeping.  When you create an entity, Beast will keep track of it in a global
index.  We'll see the reason for this in the next section.

To destroy an entity (i.e. remove it from Beast's index) you can use
`(destroy-entity the-entity)`.  You can wipe the slate clean and remove all
entities at once with `(clear-entities)`.

### Callbacks

Beast also defines two generic functions called `entity-created` and
`entity-destroyed` which don't do anything by default, but are there for you to
add methods on if you want.  For example:

    :::lisp
    (define-aspect location x y)

    (defvar *world* (make-array (100 100) :initial-element nil))

    (defmethod entity-created :after ((e location))
      (push e (aref *world* (location/x e) (location/y e))))

    (defmethod entity-destroyed :after ((e location))
      (with-slots ((x location/x) (y location/y)) e
        (setf (aref *world* x y) (delete e (aref *world* x y)))))


Systems
-------

Beast's aspects and entities are just *very* thin sugar over CLOS, but systems
provide extra functionality that comes in handy when writing games.

A system is essentially a function that takes an entity as an argument with
zero or more aspects as type specifiers.  When you run a system the function
will be run on every entity that meet the requirements.  For example:

    :::lisp
    ; No specifiers, this just runs on every entity.
    (define-system log-all-entities (entity)
      (print entity))

    ; Runs on entities with the lifetime aspect.
    (define-system age ((entity lifetime))
      (when (> (incf (lifetime/age entity))
               (lifetime/lifespan entity))
        (destroy-entity entity)))

    ; Run on entities with both the visible and location aspects.
    (define-system render ((entity visible location))
      (draw entity (location/x entity)
                   (location/y entity)
                   (visible/color entity)))

Systems with more than one argument are currently supported, but should be
considered experimental.  The API may change in the future.

    :::lisp
    ; Run on all PAIRS of entities that have the appropriate aspects.
    (define-system detect-collisions ((e1 location collidable)
                                      (e2 location collidable))
      ; ...
      )

`define-system` defines a function with the same name as the system, and
a `run-...` function that will do the actual running for you:

    :::lisp
    (define-system log-all-entities (entity)
      (print entity))

    (run-log-all-entities)

You should always use the `run-...` function, but the other one can be handy to
have around for tracing/debugging/disassembling purposes.

Next Steps
----------

That's most of Beast in a nutshell.  If you've gotten this far you can dive in
and make something, or take a look at the [API Reference](../reference/).

Beast also does some stuff not discussed here like caching entities by
aspect/system and type-hinting system functions.  If you're curious about how it
works you can [read the source](https://hg.sr.ht/~sjl/beast/).
