
###############
### Implementation
###############

# Auxiliary function that determines if all the components of a vector are integers
testInteger <- function(x){
  test <- round(x, 10)-round(x, 0)
  tolerance <- rep(-1e-10,length(test))
  
  if(all(test < tolerance) || all(test == 0)){ return(TRUE) }
  else { return(FALSE) }
}

# Auxiliary function that gets the component furthest from being integer of a vector
furtherFromInteger <- function(x){
  j <- 1
  max <- -Inf
  furthest <- c(-1, -1)
  flag = FALSE
  for(i in x){
    if((abs(i-round(i, 0))) > 0.5){
      flag = TRUE
    }
    
    if(flag == FALSE && round(abs(i-round(i, 0)), 10) > round(max, 10)){
      max <- round(abs(i-round(i, 0)), 10)
      if(max != 0){
        furthest[1] <- i
        furthest[2] <- j
      }
      
    } else if(flag == TRUE && round((round(i, 0)+1)-i, 10) > round(max, 10)){
      max <- round((round(i, 0)+1)-i, 10)
      if(max != 0){
        furthest[1] <- i
        furthest[2] <- j
      }
    }
    flag = FALSE
    j <- j+1
  }
  return(furthest)
}


# Auxiliary function that determines if a solution is feasible
isFeasible <- function(A, b, x){
  flag_feasible = TRUE
  
  for(i in 1:nrow(A)){
    if(A[i,]%*%x>b[i]){
      flag_feasible = FALSE
    }
  }
  
  return (flag_feasible)
}


# The branch and bound algorithm is defined
branchAndBound <- function(bb, A, b){
  
  # Parameters initialization
  numVars <- ncol(A)
  UB <- +Inf
  LB <- -Inf
  tot <- 1
  xbest <- c()
  ci <- rep(0, numVars)
  cs <- rep(Inf, numVars)
  SubproblemsList <- list(list("SubProblem_1", -Inf, ci, cs))
  
  while(TRUE){
    
    ###### Step 1: main calculations ###### 
    #the problem with better potential is found
    min = +Inf
    CurrentSubproblem <- c()
    CurrentSubproblemNum <- -1
    j=1
    for(i in SubproblemsList){
      if(i[2] < min){
        min <- i[[2]]
        CurrentSubproblem <- i
        CurrentSubproblemNum <- j
      }
      j <- j+1
    }
    
    # the problem extracted from the list of problems is removed from it
    SubproblemsList <- SubproblemsList[-CurrentSubproblemNum]
    
    # the current problem is solved
    set.bounds(bb, lower = CurrentSubproblem[[3]], upper = CurrentSubproblem[[4]])
    message(CurrentSubproblem[[1]])
    message("Lower bounds:", get.bounds(bb)$lower)
    message("Upper bounds:", get.bounds(bb)$upper)
    
    result <- solve(bb)
    x <- get.variables(bb)
    if(isFeasible(A,b,x) == FALSE){
      result <- solve(bb) # the function is executed twice because of a bug in lpSolveAPI
      x <- get.variables(bb)
    }
    z <- get.objective(bb)
    message("Solution: ", x)
    message("z: ", z,"\n")
    
    
    # if the problem is unfeasible
    if(result == 2){
      
      message("Unfeasible subproblem\n")
      
      # if the problem is feasible
    }else if(result == 0){
      
      # if the solution is integer and z < UB
      if(testInteger(x) == TRUE && z < UB){
        
        # UB is updated
        UB = z
        xbest = x
        
        # problems with a potential equal or lower than UB are removed from the list
        j=1
        aux <- c()
        for(i in SubproblemsList){
          if(i[[2]] >= UB){
            aux <- c(aux, j)
          }
          j <- j+1
        }
        if(length(aux) > 0){
          SubproblemsList <- SubproblemsList[-aux]
        }
        
        # if the solution is not integer and z < UB  
      } else if(testInteger(x) == FALSE && z < UB){
        
        # the furthest from being integer and lower indexed variable is chosen
        ramificationVariable <- furtherFromInteger(x)
        
        # the newly generated problems are inserted in the list
        csaux<-CurrentSubproblem[[4]]
        csaux[ramificationVariable[2]] <- floor(ramificationVariable[1])
        cs <- csaux
        SubproblemsList[[length(SubproblemsList)+1]] <- list(paste("SubProblem_",tot+1,sep=""), z, CurrentSubproblem[[3]], cs)
        
        ciaux<-CurrentSubproblem[[3]]
        ciaux[ramificationVariable[2]] <- ceiling(ramificationVariable[1])
        ci <- ciaux 
        SubproblemsList[[length(SubproblemsList)+1]] <- list(paste("SubProblem_",tot+2,sep=""), z, ci, CurrentSubproblem[[4]])
        tot <- tot+2
        
        # if the solution is not integer and z >= UB  
      } else if(testInteger(x) == FALSE && z >= UB){
        
        message("The branch of",CurrentSubproblem[[1]],"is removed\n")
        
      }
    }
    
    ###### Step 2: completion check ###### 
    
    # LB is updated
    if(length(SubproblemsList) != 0){
      
      # problem with the best potential is found
      min = +Inf
      CurrentSubproblem <- c()
      CurrentSubproblemNum <- -1
      j=1
      for(i in SubproblemsList){
        if(i[[2]] < min){
          min <- i[[2]]
          CurrentSubproblem <- i
          CurrentSubproblemNum <- j
        }
        j <- j+1
      }
      LB = CurrentSubproblem[[2]]
      
      if(LB == UB){
        break;
      }
    } else if(length(SubproblemsList) == 0){
      break;
    }
    
  }
  
  return(list("solution" = xbest, "objective value" = UB))
}



############
### Example 1
############

A <- matrix(nrow = 3, ncol = 4)
A[1, ] <- c(3,5,10,14)
A[2, ] <- c(3,8,6,12)
A[3, ] <- c(3,7,11,14)

b <- c(80,80,90)
c <- c(-12,-64,-118,-142)
tipores <- rep('<=', 3)

bb <- make.lp(3,4)
lp.control(bb,sense='min')
for (i in 1:nrow(A)) set.row(bb, i, A[i, ])
set.objfn(bb, c)

set.bounds(bb, lower = c(0,0,0,0))
set.bounds(bb, upper = c(8,4,9,9))

set.rhs(bb, b)
set.constr.type(bb, tipores)
bb


############
### Example 2
############

A <- matrix(nrow = 2, ncol = 2)
A[1, ] <- c(1, 1)
A[2, ] <- c(5, 9)

b <- c(6 ,45)
c <- c(-5, -8)
tipores <- rep('<=', 2)

bb <- make.lp(2,2)
lp.control(bb,sense='min')
for (i in 1:nrow(A)) set.row(bb, i, A[i, ])
set.objfn(bb, c)

set.bounds(bb, lower = c(0, 0))
set.bounds(bb, upper = c(Inf, Inf))

set.rhs(bb, b)
set.constr.type(bb, tipores)
bb


############
### Example 3
############

A <- matrix(nrow = 3, ncol = 2)
A[1, ] <- c(-10, 20)
A[2, ] <- c(5, 10)
A[3, ] <- c(1,0)

b <- c(22,49,5)
c <- c(1,-4)
tipores <- rep('<=', 3)

bb <- make.lp(3,2)
lp.control(bb,sense='min')
for (i in 1:nrow(A)) set.row(bb, i, A[i, ])
set.objfn(bb, c)

set.bounds(bb, lower = c(0, 0))
set.bounds(bb, upper = c(Inf, Inf))

set.rhs(bb, b)
set.constr.type(bb, tipores)
bb


############
### Example 4
############

A <- matrix(nrow = 2, ncol = 2)
A[1, ] <- c(3,-3)
A[2, ] <- c(3,-3)

b <- c(2,1)
c <- c(1,1)
tipores <- c('<=', '>=')

bb <- make.lp(2,2)
lp.control(bb,sense='min')
for (i in 1:nrow(A)) set.row(bb, i, A[i, ])
set.objfn(bb, c)

set.bounds(bb, lower = c(0, 0))
set.bounds(bb, upper = c(Inf, Inf))

set.rhs(bb, b)
set.constr.type(bb, tipores)
bb



############
### Execution
############

branchAndBound(bb, A, b)
