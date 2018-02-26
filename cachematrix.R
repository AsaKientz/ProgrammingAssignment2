## 2018/02/18
## Asa Kientz
## Programming assignment for Coursera "Intro to R Programming" course, week 3

## Following are a pair of functions that calculate and cache the inverse of a matrix
## for later retrieval without the need to recalculate

## 'x' is a matrix.
## makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the matrix inverse
## 4) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## calculates the inverse of the matrix in the special "vector" created with
## the "makeCacheMatrix" function.  However, it first checks to see 
## if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinverse function.

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
      ## Return the inverse of matrix 'x'
}
