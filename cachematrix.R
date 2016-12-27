## Caching inverse of a matrix
## P programming inverse of a matrix

## Set the value and the inverse of the matrix.

makeCacheMatrix:<- function(x = numeric()) {
        INV <- NULL
        set <- function(mat) {
                x <<- mat
                INV <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) INV <<- inverse
        getinv <- function() INV
        list(set = set, get = get,
             setinv = setinv,
             getinv =  getinv)
}

## Matrix inverse

cacheSolve <- function(x, ...) {
        INV <- x$getinv()
        if(!is.null(INV)) {
                message("getting cached data")
                return(INV)
        }
        matr <- x$get()
        INV <- solve(matr)
        x$setinv(INV)
        INV
}
