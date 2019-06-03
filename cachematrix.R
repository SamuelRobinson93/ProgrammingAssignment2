## Because calculating the inverse of a matrix is costly for computation,
## these formulas cache the inverse of a matrix.
## If the cached value is available, it gets the value.
## If the cached value is not available, it calculates the inverse and caches
## the value.

makeCacheMatrix <- function(x = matrix()) {
              mat_inv <- NULL
              set <- function(y) {
                    x <<- y
                    mat_inv <<- NULL
              }
              get <- function() x
              setmat_inv <- function(inverse) mat_inv <<- inverse
              getmat_inv <- function() mat_inv
              list(set = set, get = get,
                   setmat_inv = setmat_inv,
                   getmat_inv = getmat_inv)
}


## This function identifies whether the inverse is available. If not, it will
## calculate the inverse of the matrix and return its values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getmat_inv()
        if (!is.null(mat_inv)) {
              message("Cached data incoming...")
              return(mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setmat_inv(mat_inv)
        mat_inv
}
