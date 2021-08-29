(defpackage :beast
  (:use :cl)
  (:export
    :entity
    :entity-id

    :define-entity

    :create-entity
    :destroy-entity
    :clear-entities
    :map-entities
    :all-entities

    :entity-created
    :entity-destroyed

    :define-aspect

    :define-system))
