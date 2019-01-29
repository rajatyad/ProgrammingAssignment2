## The main idea here is to delineate functions that can help reduce the time it takes to solve problems
##that require calculation of same value again and again by inputting the said value into the cache memory
##, ie the RAM of the computer 

## The following function is used to Cache the special matrix that is then
##utilised by the following function to compute the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse  )
}

## The following function calculates the inverse of the special "vector" 
##created with the above function. However, it first checks to see if 
##the inverse has already been calculated. If so, it gets the inverse from 
##the cache and skips the computation

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
  
##checking the function

y<-matrix(rnorm(25),5,5)
ym<-makeCacheMatrix(y)
cacheSolve(ym)
[,1]        [,2]        [,3]      [,4]        [,5]
[1,] -0.13149165  0.04574306 -0.36887540 0.1834229  0.46166261
[2,]  0.08036640  0.51677419  0.03649867 0.2821515  0.02073707
[3,]  0.12178267  0.08716822 -0.59707866 0.4169704 -0.27223246
[4,]  0.06196468 -0.38693150 -0.24842474 0.7079532  0.42389641
[5,]  0.35073469 -0.18849602 -0.26318613 0.2765426  0.06577202
