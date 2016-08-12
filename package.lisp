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
    #:clear-entities
    #:map-entities

    #:entity-created
    #:entity-destroyed

    #:define-aspect

    #:define-system))
