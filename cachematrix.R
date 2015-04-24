## Function 1 sets and gets values from a matrix, inverts the matrix and creates 
## a cached version of the inverse. Function 2 returns the inverted matrix. If an cached
## inverted version of the inverted matrix exists, Function 2 will retrieve it, or 
## otherwise calculate it.


## makeCacheMatrix loads a matrix, computes its inverse using solve(), and stores
## a cached version of the inverted matrix.
## The four functions (set, get, setinverse, getinverse) are stored in a list.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL 
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x 
       setinverse <- function(solve) m <<- solve 
       getinverse <- function() m 
       list(set = set, get = get, 
            setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks whether the inverse of a given matrix can be retrieved from the
## cache. If this is possible, it is returned and the message "getting cached 
## data" is displayed on the console. Otherwise the matrix inversion is computed.

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       m <- x$getinverse()
       if (!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
