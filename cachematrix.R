## These two functions together compute, cache and output the inverse 
## of an inversable matrix. If the result is found in cache, 
## than the "compute" step is skipped, which saves time.

## Outputs a list containing inside functions that
##set and get the input matrix, and set and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Outputs the inverse of the matrix fed to makeCacheMatrix. The arguments
## are the output of makeCacheMatrix, not the original matrix. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)             ##if inv is found, "return" will print it and stop the function
        }
        data <- x$get()
        inv <- solve(data, ...)         ##if inv was not already found, "solve" will compute it.
        x$setinv(inv)
        inv
}
