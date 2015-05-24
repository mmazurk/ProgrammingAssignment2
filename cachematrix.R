
## Mark Mazurkiewicz
## Programming Assignment 2

## The makeCachematrix function contains a set of four functions that allows one to store the contents of a matrix and also the inverse of the matrix once it is calculated.

makeCacheMatrix <- function(x = matrix()) {
     
     # We will use this variable to store the inverse once we calculate it
     m <- NULL
     
     # if we want to change the values in the matrix
     # set the value of the matrix and reset the m value
     # both are stored in the parent environment outside the set function
     set <- function(x,y,z) { 
          x <<- matrix(x,y,z)
          m <<- NULL
     }
     
     # get the value of the matrix
     # this just passes back the stored matrix
     get <- function() x
     
     # set the inverse once it is calculated
     # store the result in the variable we defined in the parent enviornment
     setInverse <- function(solve) { m <<- solve }
     
     # get the value of the inverse
     # this will be NULL if no inverse has been calculated
     getInverse <- function() m
     
     # return a list containing all functions
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function takes a object we created with our makeCacheMatrix function and either retrieves the cached inverse or it calcualtes the inverse and stores it.

cacheSolve <- function(x, ...) {
     
     # first let's get the matrix, emmpty or not
     mymatrix <- x$get()
     
     # check and see if the matrix is empty
     # return a message if it is
     if (is.na(mymatrix[1,1])) { 
          
          message("This matrix is empty")
          return
     }
     
     # Now let's get the inverse if it has been caluclated
     # If not, then we will need to calculate it in the next step
     if(!is.null(x$getInverse())) {
     m <- x$getInverse()
     message("getting cached data")
     return(m)
     }
     
     # if we have not calculated the inverse, do it now
     # and then cache the results
     m <- solve(mymatrix)
     x$setInverse(m)
     m
}
