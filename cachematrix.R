###############################################################################
#   Author:      <anonymous? peer review>
#   Contents:-
#       Function-    makeCacheMatrix
#       Function-    cacheSolve
###############################################################################


############
#   Function    -   makeCacheMatrix
#   Description -   A function in the form of a List object, containing a
#                   number of methods to set the parameters (base matrix *mx*
#                   and cached inverse matrix *inverse*) contained within.
#   Args        -   mx          -   base matrix to be stored
#   Methods     -   set         -   sets a new matrix (*mx*) and clears the
#                                   inverse matrix (*inverse*)
#               -   get         -   returns the matrix contained in the object
#               -   setInverse  -   sets the inverse matrix in the object
#               -   getInverse  -   returns the inverse matrix in the object
#
makeCacheMatrix <- function(mx = matrix())
{
    inverse <- NULL

#   set method
    set <- function(y)
    {
        mx <<- y
        inverse <<- NULL
    }
    
#   get method
    get <- function() mx
  
#   setInverse method
    setInverse <- function(newInverse) inverse <<- newInverse
    
#   getInverse method
    getInverse <- function() inverse

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}





############
#   Function    -   cacheSolve
#   Description -   A pure function to retrieve an Inverse matrix.  If the
#                   inverse (*inverse*) exists already it will be returned with
#                   no further calculation from the matrix object with a
#                   message.  If it doesn't exist already the function will
#                   continue and use the base matrix stored (*calcMatrix*) to
#                   calculate the inverse.  This will then be set in the matrix
#                   object before returning the inverse.
#   Args        -   x           -   base Cache Matrix object
#               -   ...         -   passthrough argument
#   Methods     -   *none*
#
cacheSolve <- function(x, ...)
{
    inverse <- x$getInverse()
    
    if(!is.null(inverse))
    {
        message("Retrieved cached inverse matrix")
        return (inverse)
    }
    
    calcMatrix <- x$get()
    inverse <- solve(calcMatrix, ...)
    x$setInverse(inverse)
    
    inverse
}