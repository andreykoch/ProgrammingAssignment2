## This script has two functions and performs 4 tests.
## First function (makeCacheMatrix) creates a list of functions that: caches
## matrix (function's argument), gets matrix, caches its inverse, gets its inverse.
## Second function (cacheSolve) checks if there's inverted matrix in cache
## and if the matrix that is the argument of makeCacheMatrix has not changed.
## If both conditions are satisfied, it returns inverted matrix from cache.
## If at least one conditions is not satisfied, it computes new matrix inverse

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(X = matrix()) {
        # set initial value for matrix inverse
        X.inv <- NULL
        # this function caches matrix
        setmatr <- function(Y) {
                X <<- Y
                #X.inv <<- NULL
        }
        # this function gets matrix
        getmatr <- function() X
        # this function caches matrix inverse
        setinv <- function(inv) X.inv <<- inv
        # this function gets matrix inverse
        getinv <- function() X.inv
        list(setmatr = setmatr, getmatr = getmatr,
             setinv = setinv,   getinv = getinv)
}

## This function checks two conditions and either returns 
## cached inverted matrix or computes new inverted matrix
## Arguments: X -- special 'matrix' as an outcome of makeCacheMatrix
## Y -- matrix that is argument of makeCacheMatrix (original matrix)
cacheSolve <- function(X, Y, ...) {
        # get matrix from cache
        Z <- X$getmatr()
        # get matrix inverse from cache
        X.inv <- X$getinv()
        # check if matrix inverse exists
        if(!is.null(X.inv)) {
                message("inverse matrix exists in cache")
                # check if new matrix is identical to the original
                # matrix and get its inverse from cache
                if(identical(Z, Y)) {
                        message("matrix has not changed")
                        message("getting cached inverse matrix")
                        X.inv
                } else {
                        # compute matrix inverse if new matrix 
                        # is not identical to the original one
                        message("matrix has changed")
                        message("computing/caching inverse matrix")
                        X.inv <- solve(Z, ...)
                        X$setinv(X.inv)
                        X.inv
                }
        } else {
                # compute matrix inverse if inverted matrix 
                # doesn't exist in cache
                message("inverse matrix doesn't exist in cache")
                message("computing/caching inverse matrix")
                X.inv <- solve(Z, ...)
                X$setinv(X.inv)
                X.inv
        }
}

# Define two different matrices
edge <- 100
x.old <- matrix(rnorm(edge^2), edge, edge)
x.new <- matrix(rnorm(edge^2), edge, edge)

# Test 1. Compute and cache matrix inverse
message("---- Test 1 ----")
# get special "matrix" object
x.spe <- makeCacheMatrix(x.old)
# compute and cache inverse
x.inv <- cacheSolve(x.spe, x.old)
message(c("determinant of matrix inverse = ", det(x.inv)))

# Test 2. Get cached matrix inverse
message("---- Test 2 ----")
x.inv <- cacheSolve(x.spe, x.old)
message(c("determinant of matrix inverse = ", det(x.inv)))

# Test 3. Change original matrix, compute and cache new matrix inverse
message("---- Test 3 ----")
# set new matrix
x.spe$setmatr(x.new)
# compute and cache new matrix inverse
x.inv <- cacheSolve(x.spe, x.old)
message(c("determinant of matrix inverse = ", det(x.inv)))

# Test 4. Get cached new matrix inverse
message("---- Test 4 ----")
x.inv <- cacheSolve(x.spe, x.new)
message(c("determinant of matrix inverse = ", det(x.inv)))
