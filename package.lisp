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
    #:get-entity
    #:map-entities

    #:entity-created
    #:entity-destroyed

    #:define-aspect

    #:define-system))
