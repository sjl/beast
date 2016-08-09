(defpackage #:beast
  (:use
    #:cl
    #:beast.quickutils)
  (:export
    #:entity
    #:entity-id

    #:get-entity
    #:map-entities
    #:clear-entities

    #:entity-created
    #:entity-destroyed

    #:define-entity

    #:define-aspect

    #:define-system
    #:run-system))
