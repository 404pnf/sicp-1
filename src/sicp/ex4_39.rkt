#lang racket

#|

There are 5*5*5*5*5 possibilities. The order won't affect the results. 
But it will affect time. The various 'require' or 'assert' statements
restrict the number of options and the possibility tree.
 
Let us look at the statements now.

1.    (assert (distinct? (list baker cooper fletcher miller smith)))
2.    (assert (not (= baker 5)))
3.    (assert (not (= cooper 1)))
4.    (assert (not (= fletcher 5)))
5.    (assert (not (= fletcher 1)))
6.    (assert (> miller cooper))
7.    (assert (not (= (abs (- fletcher cooper)) 1)))

If we look the constraints in isolation, 

constraint #1, will have 5*4*3*2*1 = 120 successes.
constraint #2 will have 5*5*5*5*4  = 2400 successes.

When they are put together, let us say #1 and then #2, then we have
#1 executing for 5^5 times resulting in 120 successes and so #2 executing for 120 times.

If we reverse #2 and #1, then we have #2 executing for 5^5 times giving 2400 possible 
values and so #1 executes for 2400 times.

So, if any condition takes more time to execute, then these numbers will be an effect, so
order will affect the execution times.

|#