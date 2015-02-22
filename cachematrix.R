##
## Author: Alexander McLin
## Last Updated: 2/21/2015
##
##
## Description:
##
## makeCacheMatrix and cacheSolve are two functions that work together to implement
## efficient computations of matrix inverses.
##
## To use, call makeCacheMatrix, either passing in a matrix as a parameter or create
## an empty matrix by default with no passed parameter. The function will return a 
## list of several functions that wraps around the matrix and implement an interface 
## for caching inverse computations.
##
## The list functions as a proxy for the matrix and can be passed to the cacheSolve 
## function to compute the inverse of the wrapped matrix and caches the result so each 
## time cacheSolve is called for the same matrix(or rather the list that wraps around the matrix) 
## the cached result will be returned.
##
## Test Cases:
##
## Test Case 1:
##
## A = matrix(c(2,2,3,2), nrow = 2, ncol = 2)
## m <- makeCacheMatrix(A)
## B <- cacheSolve(m)
## B 
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
##
## A %*% B == identity matrix
##
## Test Case 2:
##
## A <- matrix(c(1,0,1,2,4,0,3,5,6), nrow = 3, ncol = 3)
## m <- makeCacheMatrix(A)
## B <- cacheSolve(m)
## B
##            [,1]        [,2]        [,3]
## [1,]  1.0909091 -0.54545455 -0.09090909
## [2,]  0.2272727  0.13636364 -0.22727273
## [3,] -0.1818182  0.09090909  0.18181818
##
## A %*% B == identity matrix
##

## makeCacheMatrix: Matrix or None -> A List of Four Functions - (Set, Get, SetInverse, GetInverse)
##
## Usage: m <- makeCacheMatrix() or
##        m <- makeCacheMatrix(matrix(<your data>))
##
##        To retrieve original matrix: 
##                    m$get()
##
##        To replace with another matrix: 
##                    m$set(<another matrix>)
##
##        To get cached inverse:
##                    m$getinverse()
##
##        To remember the inverse:
##                    m$setinverse(<matrix inverse>)

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL # inverse is initially set to NULL

                # set overwrites x with y and overwrites i with NULL.
                set <- function(y) {
                    x <<- y
                    i <<- NULL
                }

                # get returns the matrix stored in the x variable.
                get <- function() x

                # setmean overwrites m with given inverse value.
                setinverse <- function(inverse) i <<- inverse

                # getinverse returns the inverse
                getinverse <- function() i

                list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: CacheMatrix -> CacheMatrix's inverse
##
## Usage: i <- cacheSolve(<CacheMatrix>)
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # get cached inverse and check to see if it's not null meaning it
        # was already previously computed and we can return it immediately.
        i <- x$getinverse()
        if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
        }
        
        # i was null so we need to compute the inverse
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i) # remember inverse for future.
        i 
}
