## this pair of functions creates matrix wrapper that can cache matrix inverse
## and computes or returns cached value of the matrix inverse 

## function creates a matrix wrapper that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
    i <- NULL
    
    set <- function( y ) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setInverse <- function( inverse ) i <<- inverse
    getInverse <- function() i
    
    list( set = set, get = get,
          setInverse = setInverse, getInverse = getInverse )
}


## function computes or returns the cached value of inverse for the matrix wrapper 
cacheSolve <- function( x, ... ) {
    i <- x$getInverse()
    
    if( !is.null( i ) ) {
        message( "getting cached inverse matrix" )
        return( i )
    }
    
    data <- x$get()
    i <- solve( data )
    x$setInverse( i )
    i
}