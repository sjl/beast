(defpackage #:beast
  (:use
    #:cl
    #:beast.quickutils)
  (:export
    #:entity
    #:entity-id

    #:define-entity

    #:create-entity
    #:destroy-entity
    #:get-entity
    #:map-entities
    #:clear-entities

    #:entity-created
    #:entity-destroyed

    #:define-aspect

    #:define-system
    #:run-system))
