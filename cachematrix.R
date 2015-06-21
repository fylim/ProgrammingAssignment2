## This R functions are meant to cache potentially time-consuming computations. 
## For example, taking the inverse of a numeric vector is typically a fast operation. However, for a very long vector, 
## it may take too long to compute the inverse, especially if it has to be computed repeatedly (e.g. in a loop).
## If the contents of a vector are not changing, it may make sense to cache the value of the inverse so that 
## when we need it again, it can be looked up in the cache rather than recomputed. 


## The first function, makeVector creates a special "vector", which is really a list containing a function to

## set the value of the vector
## get the value of the vector
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(x) {  
   x <<- y
   i <<- NULL
 }
  get <- function() x
  setinversed <- function(inversed) i <<- inversed
  getinversed <- function() i
  list(set = set, 
       get = get,
       setinversed = setinversed,
       getinversed = getinversed)
}


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinversed function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinversed()
    
    ##Check if i has been computed before, return i if there is a value
        if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    ##Since i is NULL, compute inverse and return i
    data <- x$get()
    i <- solve(data, ...)
    x$setinversed(i)
    i
  }
  

