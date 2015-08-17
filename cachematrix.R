##Caching the inverse of matrix

##The following function creates makeCacheMatrix which consists of list of functions to set/get/setinverse/getinverse various values of matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse
  )
}


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
