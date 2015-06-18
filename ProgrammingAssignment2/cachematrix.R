# makeCacheMatrix creates a list, which contains functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix 
# 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve1) m <<- solve1
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


# cacheSolve calculates the inverse of the matrix cached by 
# the makeCachMatrix function.  
# It first checks to see if the inverse of the matrix is 
# calculated.  If so, it gets the inverse from the cache and 
# skips the computation.  Otherwise, it calculates the inverse
# of the matrix and sets the value of inverse 
# via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
        
}



