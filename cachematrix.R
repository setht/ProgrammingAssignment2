## functions paid to create a matrix object that has implemented caching mechanisms 
## and the function that both solves and caches the solution so subsequent calls will return the cached object rather than solving

## builds a matrix object that include methods for getting/setting pertintent data for it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## solves and caches results for matrix objects enhanced with makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}


hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8

sh8 <- makeCacheMatrix(h8)
inv <- cacheSolve(sh8)

print(inv)
