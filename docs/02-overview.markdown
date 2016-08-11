Overview
========

When you're making a video game you need a way to model things in the game
world.  In the past couple of decades Entity/Component systems have become
popular:

* <http://gameprogrammingpatterns.com/component.html>
* <http://en.wikipedia.org/wiki/Entity_component_system>
* <http://www.gamedev.net/page/resources/_/technical/game-programming/understanding-component-entity-systems-r3013>

There are a couple of ECS libraries for Common Lisp already:

* [cl-ecs](https://github.com/lispgames/cl-ecs)
* [ecstasy](https://github.com/mfiano/ecstasy)

Both of these favor composition over inheritance -- game objects (entities)
*contain* various components, but they don't *inherit* from components.

Beast takes the opposite approach, favoring (restricted) inheritance over
composition.

Components in Beast are called "aspects" to try to overload the word "component"
a little bit less in this crazy world.  Aspects are essentially
[mixins](https://en.wikipedia.org/wiki/Mixin), with some sugar for defining them
and running systems over them:

    :::lisp
    (define-aspect throwable accuracy damage)
    (define-aspect edible nutrition-value)

    (define-entity dart (throwable))
    (define-entity cheese (edible))
    (define-entity pie (throwable edible))

    (define-system rot-food ((e edible))
      (decf (edible/nutrition-value e))
      (when (zerop (edible/nutrition-value e))
        (destroy-entity e)))

    (defparameter *steel-dart* 
      (create-entity 'dart
        :throwable/accuracy 0.9
        :throwable/damage 10))

    (defparameter *hunk-of-swiss*
      (create-entity 'cheese
        :edible/nutrition-value 50))

    (defparameter *banana-cream-pie*
      (create-entity 'pie
        :throwable/accuracy 0.3
        :throwable/damage 5
        :edible/nutrition-value 30))

Beast tries to be just a very thin layer over CLOS, because CLOS is quite
powerful.  You can use `typep`, generic methods, before/after/around methods,
and everything else CLOS gives you.

Like every engineering decision this comes with are tradeoffs.  You can't
(easily) add or remove aspects to/from a particular entity at runtime like you
can with cl-ecs.  And there's no way to give an entity multiple "copies" of
a single aspect.

The author has found this approach to work well for his needs.  You should take
a look at both approaches and decide which is best for you.  If you want to read
more, check out the [Usage](../usage/) document.
