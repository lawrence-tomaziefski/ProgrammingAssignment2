## functions that cache the inverse of a matrix

##function that sets the values of a matrix, gets the values of a matrix, sets the inversion of the matrix, gets the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve(x) 
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##function computes the inverse of matrix returned by makeCacheMatrix above. If the inverse has already been calculated the function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
