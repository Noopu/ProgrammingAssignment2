## 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse, assuming that the matrix supplied is always
## invertible.
## 'cacheSolve' computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.  

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' from cache if already computed, else computes it and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverted matrix")
        return(inv)
    }
    x <- x$get()
    inv <- solve(x, ...)
    x$setinverse(inv)
    inv
}
