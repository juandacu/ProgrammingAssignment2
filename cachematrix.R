## The two functions below creates an object that stores
##a matrix an Chache the inverse of such matrix


## "makeCacheMatrix" creates a matrix object that sets and gets its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inversematrix) m <<- inversematrix
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## "CacheSolve" first checks if the inverse of the matrix has been previously calculated
## and if so, it returns the inverse matrix already cached from the function above.
## If not, it will calculate it and return the inverse matrix

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

