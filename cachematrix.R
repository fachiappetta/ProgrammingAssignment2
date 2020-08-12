## Two functions, "makeCacheMatrix" and "cacheSolve" work to cache the inverse of a matrix

## "makeCacheMatrix" is a function that creates a special matrix object which starts with a null matrix argument
## it returns a list containing a function to: 
## get the matrix, set the value of the matrix, set the value of inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse = NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) (inverse <<- solve)
        getinverse  <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## "cacheSolve" is used to get the cache of the matrix. 
## If the inverse exists, it retrieves it.
## If the inverse if not there, it is calculated and then retrieved.

cacheSolve <- function(x, ...) {
       inverse <-x$getinverse()
       if(!is.null(inverse)) {
               message("getting cached data- inverse of the matrix")
               return(inverse)
       }
       mat <- x$get()
       inverse <- solve(mat, ...)
       x$setinverse(inverse)
       inverse
}
