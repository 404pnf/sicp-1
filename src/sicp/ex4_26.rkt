#lang racket

#|

Yes, if 'unless' were implemented in an applicative order language, then
it would be implemented as a special form and would be translated into
other functions before evaluation.

Now, if such a special form is used in a higher order function like 'map'
for instance:

(map unless '((p-list) (usual-list) (exception-list)))

then, unless gets expanded before evaluation. But map expects a function
as its second argument.

To see the implementation of unless, see metacircular2.rkt

|#
