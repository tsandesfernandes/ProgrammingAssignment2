## Put comments here that give an overall description of what your
## functions do

##exemplo para criar um vector

##makeVector <- function(x = numeric()) {
##  m <- NULL
##  set <- function(y) {
##    x <<- y
##    m <<- NULL
##  }
##  get <- function() x
##  setmean <- function(mean) m <<- mean
##  getmean <- function() m
##  list(set = set, get = get,
##       setmean = setmean,
##       getmean = getmean)
##}

## Write a short comment describing this function

# 1 - setar o valor da matriz
# 2 - pega valor da matriz
# 3 - setar o valor da matriz inversa
# 4 - pegar o valor da matriz inversa

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mInversa <- NULL   
    #1
    setMatriz <- function(y) 
    {
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


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv    
}
