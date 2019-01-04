##makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
    
        # set the matrix
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        #get the matrix
        get <- function() x
    
        # set the reversed matrix
        setinverse <- function(inverse) inv <<- inverse
    
        # get the reversed matrix
        getinverse <- function() inv
    
        # return a list
        list(set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()  # get the reversed matrix
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)  # compute the inverse of the "matrix"
        x$setinverse(inv)
        inv
}
