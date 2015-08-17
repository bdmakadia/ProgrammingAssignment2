##Caching the invertible of matrix
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##The following function creates makeCacheMatrix which consists of list containing functions to set/get/setinverse/getinverse various values of matrix
## For the following function to work matrix should be invertible


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() m
  list(		set = set, 
  			get = get, 
  			setinverse = setinverse, 
  			getinverse = getinverse
  )
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##The following function calculates the inverse of the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
## The following function calculates the inverse of the matrix created using the makeCacheMatrix function

cacheSolve <- function(x, ...) {
	 m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
