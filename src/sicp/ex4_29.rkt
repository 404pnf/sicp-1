#lang racket

#|

Any program which will call itself or another function recursively or repeatedly (as another argument) 
will benefit from memoization.

non-memoized version will have count of 2 as x gets forced twice in the expression (* x x). In memoized
version, it will be 1.

|#