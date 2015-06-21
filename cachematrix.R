## The functions below create a cache of the inverse of a matrix. The cached
## inverse matrix can be used for future applications where the inverse matrix
## is needed without needing to re-calculate the inverse matrix.

## The makeChacheMatrix function creates a matrix. This matrix does four
## things: it first sets, then gets the value of the matrix. It then sets,
## then gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function checks to see if the invers of a matrix has already
## been determined. If so, it retrieves that inverse matrix from the cache and
## returns it. Otherwise, it calculates the inverse matrix, returns it, and
## stores it in the cache for futer use.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
