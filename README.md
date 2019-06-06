# Implementation of the branch and bound algorithm for solving integer programming problems

Here is a very simple implementation of the Dijkstra algorithm in R language. 

An instance of the function execution with `example1` (in the terminology of the code developed) is shown here: 

![alt text](https://github.com/sergioreyblanco/branchAndBound_integerProgramming/blob/master/execution.PNG) 


The whole algorithm is implementated in the function `branchAndBound`. It is divided in two well diferenced steps: the main one (where all the calculations are made) and the second one (where it is checked if the an optimal solution has been found).

The function takes as an input parameter a representation of a linear problem in the form of an "object" of the library `lpSolveAPI`. This object must have all its attributes (restrictions, sense of optimization, bounds, ...) well defined. The function gives a return value consisting of a list with two components in which the first is the optimal solution (values of all the variables that the objective function has) and the second is the value obtained when the variables of the objective function are replaced with the solution previously mentioned. Moreover, there three auxiliary functions used in the main one: `testInteger` which is used to verify if all of the components of a given array (the solution analysed at the current iteration) are integers, `furtherFromInteger` which is used to get the component of a given array (the solution analysed at the current iteration) that is further from its nearest integer number and `isFeasible` which is used to check if a given solution is feasible (in principle all the solutions provided by `lpSolveAPI` are feasible, however the library has a bug and sometimes returns unfeasible solutions).

Example "linear problem objects" (located in the last part of the code after the functions) are given, so you can test the function and create new similar objects. As these examples have objective functions of more than three variables, their representation in a 2 or 3 dimensional space is not possible. However, a graphical representation of the resolution of `example1` using this algorithm is shown here:

![alt text](https://github.com/sergioreyblanco/branchAndBound_integerProgramming/blob/master/example1.png)

The code needs one library to run, besides the classical R environment: `lpSolveAPI`. It can be installed like this: `install.packages("lpSolveAPI")` or `install.packages("lpSolveAPI")` (choose the form that best suits your needs). And then loaded as follows: `library(lpSolveAPI)`.
