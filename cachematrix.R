##The function 'makeCacheMatrix' creates a matrix that is a list consisting of 4 functions: 'set', 'get', 'calcInverse', 'getInverse'.
##Variables x and invrs are stored in the enclosing enviroment of aforementioned functions because '<<-' operator for assigning to variables in the parent environments was used (i.e. global assignment)

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(z) {
    x <<- z
    invrs <<- NULL
  }
  get <- function () x
  calcInverse <- function() invrs <<- solve(x) ##Calculates the inverse of square matrix
  getInverse <- function() invrs
  list(set = set, 
       get = get,
       calcInverse = calcInverse,
       getInverse = getInverse)

}


## 'cacheSolve' function calculated the inverse of matrix created using 'makeCacheMatrix'. First, it checks if it can load computed inverse from cache. If so, she returns it. 
## Otherwise, it inverts the matrix and sets the inverse in the cache using 'calcInverse' function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##1 Try to load cached data, if already generated
        invrs <- x$getInverse()
        if(!is.null(d)) {
          cat("Loaded calculated cached data\n")
          return(invrs)
        }
        
        ##2 If inverse matrix is not available, generate it. 
        cat("Calculating inverse from scratch")
        md <- x$get()
        invrs <- solve(md, ...)
        x$calcInverse(invrs)
        invrs
}



