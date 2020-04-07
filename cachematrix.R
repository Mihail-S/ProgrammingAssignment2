# makeCacheMatrix returns a list containing functions to
# 1. set the entries of matrix, set 
# 2. get the entries of matrix, get 
# 3. set the entries of inverse matrix, setinv
# 4. get the entries of inverse matrix, getinv

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set   <- function(y) {
               x <<- y
             inv <<- NULL
    }
    get    <- function() x
    setinv <- function(invs) inv <<- invs
    getinv <- function() inv
    list(set    = set, 
         get    = get, 
         setinv = setinv, 
         getinv = getinv)
}


# cacheSolve returns ivn, the inverse of the matrix. 
# cacheSolve assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    tmp <- x$get()
    inv <- solve(tmp)

    x$setinv(inv)
    inv
}
