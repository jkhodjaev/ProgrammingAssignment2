## this function creates matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        invrs=NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}

## This function computes the inverse of matrix returned by the function above
cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data,...)
        x$setinverse(invrs)
        invrs
        ## Return a matrix that is the inverse of 'x'
}
