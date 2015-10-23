## Together these fucntions are able to cache the inverse of a matrix
## within the current environemnt so that it can be used in later calculations

## The makeCacheMatrix is a list of four functions named set, get, set inverse and get inverse.

makeCacheMatrix <- function(x = matrix()) {  ##Declare and define a function named makeCacheMatrix()
  ma <- NULL ## Assign the numeric value NULL to local variable 'ma'
  set <- function(y) {  ## Uses superassigment to equate 'y' with 'x' and assign 'ma' as NULL in the global environment
    x <<- y
    ma <<- NULL
  }
  get <- function() x  ## Calling get() returns the local value of 'x'
  setinverse <- function(inv) ma <<- inv  ##Declares the variable 'inv' as a function of 'ma'
  getinverse <- function() ma ## Returns the local value of 'ma'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes, caches, and returns the new matrix inverse

cacheSolve <- function(x, ...) {
  ma <- x$getinverse()
  if(!is.null(ma)) {   ## If matrix has already been cache, returns a message that it is looking up the cached matrix
    message("getting cached data")  
    return(ma)  ## Returns the cached inverse
  }
  data <- x$get()  ##If no cache matrix is found, declares a new variable called data to hold the matrix inverse
  ma <- solve(data, ...) ## Uses the R funtion solve() to find the inverse of the matrix and assigns it to the variable 'ma'
  x$setinverse(ma) ## Calls the function setinverse() on 'ma' which assigns 'inv' to the local value of 'ma'
  ma ##Returns the inverted matrix
}

