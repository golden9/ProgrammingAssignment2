##############################################################################
## Function : makeCacheMatrix
## This function creates a special matrix which is able to cache it's inverse
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
  
    ## Setting the value of the matrix and cacheing it
    set <- function(y=matrix()){
    matrixStore <<- y
    matrixInverse <<- NULL      
    }
  
    ## Returning the valu of the cached matrix
    get <- function() matrixStore
  
    ## Calculating and cacheing the inverse of the matrix 
    setInverse <- function(y){
    matrixInverse <<- solve(y)
    }
  
    ## Returning the value of the cached inverse
    getInverse <- function() matrixInverse
  
    ## List of functions to manipulate caching of matirx and it's inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


###################################################################################
## CacheSolve ##
## Computes the inverse of the special matrix returned by the function 
## makeCacheMatrix. 
## 1. If the inverse has been calculated and the matrix has not changed then return
##    the cached value of the inverse
##
## 2. If the inverse has not been calculated then return the inverse
###################################################################################
cacheSolve <- function(x, ...) {
  ## Check is this matrix has changed to the previous matrix this function 
  ## was called on
  
  sameMatrix = TRUE
  mySpecialMatrix <- makeCacheMatrix(x)
  
  #Check if the matrix is NULL or has changed
  if (is.null(matrixStore)){
    sameMatrix = FALSE
    print("The cached matrix is NULL")
  } else if (!identical(x,matrixStore)){
    #set the matrix
    sameMatrix = FALSE
    print("The cached matrix is different")
  } 
  
  if (is.null(matrixInverse)){
    sameMatrix = FALSE
    print("The cached inverse is null")
  }
  
  ## Recalculate the inverse as the matrix has changed 
  ## or the inverse is not available. Cache the new inverse calculated 
  if (sameMatrix==FALSE){
    mySpecialMatrix$set(x)
    mySpecialMatrix$setInverse(x)
  } 
  
  ## Return the currently cached inverse matrix   
  matrixInverse
}
