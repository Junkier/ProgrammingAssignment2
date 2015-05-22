## 'makeCacheMatrix' can restore matrix and its inverse matrix.It also can set the new data into it.
## Therefore,we can get the matrix & inverse matrix without calculation everytimes.
## On the other hand,'cacheSolve' can return the inverse by getting the data from 'makeCacheMatrix' 
## if the inverse has already been calculated.Otherwise it'll compute it then feedback to us.
## The advantage is that this function can help us save couples of times !!!


## makeCacheMatrix:
## It's a list including 4 funs. (set;get;setinverse;getinverse)
## We can use these funs.to set / call the matrix and its inverse matrix.(caching) 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
             x <<- y
             inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set=set,get=get,
             setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve:
## put the 'makeCacheMatrix'output form into x ,then we're able to get the caching inverse matrix
## if we had set the inverse by makeCacheMatrix.
## And if it's NULL , it'll calculate and return the inverse matrix .

cacheSolve <- function(x, ...) {
        inv2<-x$getinverse()
        if(!is.null(inv2)){
                message("getting cached matrix !!!!")
                return(inv2)
        }
        matrix <- x$get()
        inv2 <- solve(matrix)
        x$setinverse(inv2)
        inv2

}
