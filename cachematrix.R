## A pair of functions defined below, when used together, enable support of
## cached matrix inverse calculation

## makeCacheMatrix creates a special data structure
## that supersedes the original matrix in all ensuing inverse caltucations.
## It takes an invertible matrix as an argument (default value is an empty matrix)
## It returns a list of function references that are used to evaluate,
## cache and retrieve the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(i) inv <<- i
    
    getinv <- function() inv
    
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Note that to take full advantage of this data structure,
## all nevessary updates of the original matrix should be performed
## though set() member function, as opposed to direct reassignments


## cacheSolve evaluates matrix inverse, or retrieves cached value
## if it was evaluated before
## It takes the value returned by makeCacheMatrix() as an argument
## It returns the matrix inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        return(inv)
    }
    
    y <- x$get()
    
    x$setinv(solve(y))
    
    x$getinv()
}
