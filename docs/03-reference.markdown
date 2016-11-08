# API Reference

The following is a list of all user-facing parts of Beast.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `BEAST`

### `CLEAR-ENTITIES` (function)

    (CLEAR-ENTITIES)

Destroy all entities.

  `destroy-entity` will be called for each entity.

  Returns a list of all the destroyed entites.

  

### `CREATE-ENTITY` (function)

    (CREATE-ENTITY CLASS &REST INITARGS)

Create an entity of the given entity class and return it.

  `initargs` will be passed along to `make-instance`.

  The `entity-created` generic function will be called just before returning the
  entity.

  

### `DEFINE-ASPECT` (macro)

    (DEFINE-ASPECT NAME &REST FIELDS)

Define an aspect class.

  `name` should be a symbol that will become the name of the class.

  `fields` should be zero or more field definitions.  Each field definition can
  be a symbol (the field name), or a list of the field name and extra CLOS slot
  options.

  Field names will have the aspect name and a slash prepended to them to create
  the slot names.  `:initarg` and `:accessor` slot options will also be
  automatically generated.

  Example:

    (define-aspect edible
      energy
      (taste :initform nil))
    =>
    (defclass edible ()
      ((edible/energy :initarg :edible/energy
                      :accessor edible/energy)
       (edible/taste :initarg :edible/taste
                     :accessor edible/taste
                     :initform nil)))

  

### `DEFINE-ENTITY` (macro)

    (DEFINE-ENTITY NAME ASPECTS &REST SLOTS)

Define an entity class.

  `name` should be a symbol that will become the name of the class.

  `aspects` should be a list of the aspects this entity should inherit from.

  `slots` can be zero or more extra CLOS slot definitions.

  Examples:

    (define-entity potion (drinkable))

    (define-entity cheese (edible visible)
      (flavor :accessor cheese-flavor :initarg :flavor))

  

### `DEFINE-SYSTEM` (macro)

    (DEFINE-SYSTEM NAME-AND-OPTIONS
        ARGLIST
      &BODY
      BODY)

Define a system.

  `name-and-options` should be a list of the system name (a symbol) and any
  system options.  A bare symbol can be used if no options are needed.

  `arglist` should be a list of system arguments.  Each argument should be
  a list of the argument name and zero or more aspect/entity classes.

  Defining a system `foo` defines two functions:

  * `foo` runs `body` on a single entity and should only be used for debugging,
    tracing, or disassembling.
  * `run-foo` should be called to run the system on all applicable entities.

  Available system options:

  * `:inline`: when true, try to inline the system function into the
    system-running function to avoid the overhead of a function call for every
    entity.  Defaults to `nil`.

  Examples:

    (define-system age ((entity lifetime))
      (when (> (incf (lifetime/age entity))
               (lifetime/lifespan entity))
        (destroy-entity entity)))

  

### `DESTROY-ENTITY` (function)

    (DESTROY-ENTITY ENTITY)

Destroy `entity` and return it.

  The `entity-destroyed` generic function will be called after the entity has
  been destroyed and unindexed.

  

### `ENTITY` (class)

A single entity in the game world.

### `ENTITY-CREATED` (generic function)

    (ENTITY-CREATED ENTITY)

Called after an entity has been created and indexed.

  The default method does nothing, but users can implement their own auxillary
  methods to run code when entities are created.

  

### `ENTITY-DESTROYED` (generic function)

    (ENTITY-DESTROYED ENTITY)

Called after an entity has been destroyed and unindexed.

  The default method does nothing, but users can implement their own auxillary
  methods to run code when entities are destroyed.

  

### `MAP-ENTITIES` (function)

    (MAP-ENTITIES FUNCTION &OPTIONAL (TYPE 'ENTITY))

Map `function` over all entities that are subtypes of `type`.

  Normally you should run code on entities using systems, but this function can
  be handy for debugging purposes.

  

