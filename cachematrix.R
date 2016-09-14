# These functions calculate, cache, and return the inverse of matrix x, or 
# they returns the inverse of matrix x if previously calculated and cached. 


makeCacheMatrix <- function(x = matrix()) {    # makeCacheMatrix() is defined;
                                               # arg x given default matrix mode
    inv <- NULL     # inv will hold the matrix x's inverse once calculated;
                    # initially set to NULL in the local environment
    set <- function(y) {    # set() defined to: 
        x <<- y             ## assign new value of matrix in parent environt;
        inv <<- NULL        ## reset inv to NULL if passed a new matrix
    }
    get <- function() x     # get() defined to return matrix x
    setinverse <- function(inverse) inv <<- inverse
                            # assigns value of inv in parent environment
    getinverse <- function() inv    ## returns the value of inv where called
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
        # returns a list consisting of x$set(), x$get(), etc.
        # x$get() = matrix whose inverse is to be calculated
        # x$getinverse() = NULL, before running cacheSolve()
                      ## = inverse of x$get(), after running cacheSolve()
}


cacheSolve <- function(x, ...) {
 
        inv <- x$getinverse()   # 'inv' will = NULL if inverse of x not cached,
                                # or will take on value of the cached inverse
        if(!is.null(inv)) {     # if inv !=NULL (ie inverse already calculated):
                message("getting cached data")  ## display message;
                return(inv)                     ## return cached value.
            }   # if inverse has not yet been calculated and cached:
        data <- x$get()         ## 'data' takes the value of the x$get matrix;
        inv <- solve(data, ...) ## 'inv' takes the value of the inverse of x$get
        x$setinverse(inv)       ## pass inv to setinverse(), cacheing it;
        inv                     ## return and print inv.
}
