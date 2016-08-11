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

### `CREATE-ENTITY` (function)

    (CREATE-ENTITY CLASS &REST INITARGS)

### `DEFINE-ASPECT` (macro)

    (DEFINE-ASPECT NAME &REST FIELDS)

### `DEFINE-ENTITY` (macro)

    (DEFINE-ENTITY NAME ASPECTS &REST SLOTS)

### `DEFINE-SYSTEM` (macro)

    (DEFINE-SYSTEM NAME-AND-OPTIONS
        ARGLIST
      &BODY
      BODY)

### `DESTROY-ENTITY` (function)

    (DESTROY-ENTITY ENTITY)

### `ENTITY` (class)

#### Slot `ID`

* Allocation: `:INSTANCE`
* Initform: `(INCF BEAST::*ENTITY-ID-COUNTER*)`
* Reader: `ENTITY-ID`

#### Slot `%BEAST/ASPECTS`

* Allocation: `:CLASS`
* Initform: `NIL`

### `ENTITY-CREATED` (generic function)

    (ENTITY-CREATED ENTITY)

### `ENTITY-DESTROYED` (generic function)

    (ENTITY-DESTROYED ENTITY)

### `GET-ENTITY` (function)

    (GET-ENTITY ID)

### `MAP-ENTITIES` (function)

    (MAP-ENTITIES FUNCTION &OPTIONAL (TYPE 'ENTITY))

