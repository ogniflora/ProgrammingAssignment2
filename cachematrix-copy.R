## Two functions that are used to create a special object 
## that stores a matrix and caches its inverse 

## The first function accepts a matrix as argument and 
## creates sub-functions, set, get, set_inv, get_in, which work on that argument

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        set_inv <- function(inv) s <<- inv
        get_inv <- function() s
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}

## The second function returns the cached inverse matrix, if it has already been calculated, 
## otherwise it calculates it and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$get_inv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$set_inv(s)
        s
}


