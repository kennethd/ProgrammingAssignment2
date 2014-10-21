## makeCacheMatrix accepts x, an invertible square matrix, and returns a list
## of four named functions bound to the input data in the environment of the
## function body (an object constructor).  Regarding "invertible square matrices",
## Wikipedia says this...
## 
##   "In linear algebra, an n-by-n square matrix A is called invertible (also
##   nonsingular or nondegenerate) if there exists an n-by-n square matrix B such
##   that  AB == BA == In  where In denotes the n-by-n identity matrix and the
##   multiplication used is ordinary matrix multiplication. If this is the case,
##   then the matrix B is uniquely determined by A and is called the inverse of A,
##   denoted by A−1.
## 
##   A square matrix that is not invertible is called singular or degenerate. A
##   square matrix is singular if and only if its determinant is 0. Singular
##   matrices are rare in the sense that a square matrix randomly selected from a
##   continuous uniform distribution on its entries will almost never be singular.
## 
##   Non-square matrices (m-by-n matrices for which m ≠ n) do not have an inverse.
## 
## and an "identity matrix" is:
##
##   "In linear algebra, the identity matrix or unit matrix of size n is the 
##   n × n square matrix with ones on the main diagonal and zeros elsewhere.
##
## cacheSolve accepts as an argument the vector/list object already bound to the
## functions added by makeCacheMatrix, and returns the inverse of the matrix x
## originally passed to makeCacheMatrix(), from cache in object's env if exists, 
## or after calculating it & storing it in the cache if requested for the first time


## bind invertible square matrix "x" to functions:
##   * get() to retrieve data matrix x
##   * set(x) to update data matrix x
##   * getinverse() returns current value of inverse variable within object's env
##   * setinverse(inv) set value of inverse variable within object's env
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y = matrix()) {
        x <<- y # update value of x in outer scope of makeCacheMatrix body
        inv <<- NULL # unset value of inv in outer scope in case prev set
    }
    get <- function() x
    setinverse <- function(inverse) {
        inv <<- inverse # set in outer scope of function body   
    }
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## return the inverse of the original matrix for object x, which is the object
## returned from makeCacheMatrix.
##
## This uses a pattern called "memoize" in other languages, where the result of
## the operation is cached after the first time it is requested, and subsequent
## requests for the value are returned from the cache. 
## 
## The inverse itself is calculated by calling solve() with the original matrix
## passed to makeCacheMatrix as parameter, any optional parameters will also be
## passed along to solve() (but note, this function does not take those into 
## account when reading the cache... if you choose to call this with optional 
## parameters to solve(), be sure to invalidate the cache before doing so, 
## e.g. x$setinverse(NULL))
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (! is.null(inv)) {
        message("returning inverse from cache")
        return(inv)
    }
    orig_x <- x$get()
    inv <- solve(orig_x, ...)
    x$setinverse(inv)
    inv
}
