## These two functions are quite similar to the two in the example.
## 

## Function to return the special matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Set m to NULL
        m <- NULL
        ## Set input value to set
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Set get, setsolve and getsolve.  It's the same as the example.
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        ## Return the special matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## Function to calculate Inverse of Matrix using solve()

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        ## check whether we can gat the cached inverse directly.
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        
        ##return the inverse
        m
}
