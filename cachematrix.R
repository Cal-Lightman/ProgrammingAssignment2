## The two functions are going to create special object so as to store a metrix
## and cache the inverse of the metrix at the same time.

## Create a special matrix, which is actually a list containing four functions
## that set the the metrix, get the metrix, set the inverse of the metrix and get
## the inverse of the metrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, 
         setsolve = setsolve, 
         getsolve = getsolve)
}


## Retrieve whether the solution was already cached, return it directly
## or compute, cache and return it.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

