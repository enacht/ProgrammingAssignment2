### "Matrix inversion is usually a costly computation and there may be some benefit
### to caching the inverse of a matrix rather than compute it repeatedly."
### The two presented functions are used to cache the inverse of an invertible matrix.

###########################################################################
### First makeCacheMatrix creates a list containing a function to
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of inverse of the matrix
### 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

###########################################################################
### The following function cacheSolve returns the inverse of the matrix.
### First it checks if the inverse of a matrix has already been computed.
### If yes it gets the result from the cache and no computations are performed.
### If not it computes the inverse of the matrix and sets the value in the cache with setinverse function.
### Atention: the matrix must be invertible! 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("No need for computation. Retrieve the inverse from the cache:")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

###########################################################################
### My sample run. Example can be checked on site: 
### http://www.arndt-bruenner.de/mathe/scripts/inversematrix.htm 
### German only, sorry.
### matrix:       -1    2 
###                0    4 
### inversematrix -1    1/2
###                0    1/4
### x = rbind(c(-1, 2), c(0, 4))
### m = makeCacheMatrix(x)
### No cache available. Perform computation.
### cacheSolve(m)
### [,1] [,2]
### [1,]   -1 0.50
### [2,]    0 0.25
### cacheSolve(m)
### No need for computation. Retrieve the inverse from the cache:
### [,1] [,2]
### [1,]   -1 0.50
### [2,]    0 0.25