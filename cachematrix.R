## These two functions work together to avoid the calculation of the inverse of 
## a matrix, if it has already been calculated for that matrix.



## makeCacheMatrix() is a function which works like a "wrapping" around a list. 
## This list contains 4 functions (set, get, setinv, getinv). Aside this list the 
## function contains as well some code to manipulate the list values. 

## Asume we have a matrix bar, retrieved by the following code:
## set.seed(1)
## universe <- 1:1000
## draw <- sample(universe, 25)
## bar <- matrix(draw, 5, 5)
## inverseBar <- solve(bar)

## We can assign bar to a new variable foo using this function as follows: 
## foo <- makeCacheMatrix()
## foo$set(bar)

## The value of foo can be queried like this:
## foo$get()

## The two other list values (getinv, setinv) are manipulated by the other 
## function, cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setinv <- function(inv) cache <<- inv
    getinv <- function() cache
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## The usage of this function is just like this: cacheSolve(foo)
## This function takes the argument which has been passed over, in this case foo.
## In the first row the value of the inverse stored in foo is queried and assigned 
## to the variable cachetest.

## As defined in the function above, getinv() just returns the value assigned 
## to the variable cache (in the function above). 
## The if-statement below tests if the value of cache is different to NULL.
## If this is the case (is.null() not TRUE) the stored value is returned and an 
## according message gets print out.

## If the inverse has not yet been calculated (which means, cache is still NULL) 
## the function proceeds and the matrix stored in foo is queried via get().
## The inverse for this matrix is calculated with solve() and stored in foo via 
## setinv(). Finally, the inverse is returned.

cacheSolve <- function(x, ...) {
    cachetest <- x$getinv()
    if(!is.null(cachetest)) {
        message("getting cached inverse")
        return(cachetest)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
