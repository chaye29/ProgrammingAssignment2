## Put comments here that give an overall description of what your
## functions do
## This kind of method is generally used in order to create a matrix that can cache it's inverse
## input x as a matrix is the main argument 
## returns:
## A matrix with methods to set values and/or set inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## cached inverse of matrix
        inv<-NULL
        ## getter/setter for matrix
        get<-function()x
        set<-function(y){
                x<<-NULL           
}
        ##getter/setter for inverse matrix
        getinv<-function()inv
        setinv<-function(inverse)inv<<-inverse
        #returns list of functions for matrix
        list(get=get, set=set, getinv=getinv, setinv=setinv)
}
## Write a short comment describing this function
## This method computes the inverse of a matrix.
## If the inverse was calculated before, it returns the cached inverse
## input x as a matrix is the main argument
## ...: assume as another arguments
## Returns:
## The inverse of the matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        ## return cached matrix inverse if it's already estimated and/or computed
        if (!is.null(inv)) {
                message("inverse is cached")
                return(inv)
}
        ##compute inverse of matrix
        m <- x$get()
        inv <- solve(m, ...)
        ## cache inverse
        x$setinv(inv)
        ## return inverse matrix
        return(inv)
}
## How to run
## EXAMPLE USAGE
mat <- matrix(c(0, 2, 1, 0), nrow = 2, byrow = TRUE)
cache_mat <- makeCacheMatrix(mat)
cacheSolve(cache_mat)
inverse is cached
[,1] [2,]
[1,] 0.0 1
[2,] 0.5 0
cacheSolve(cache_mat)
inverse is cached
[,1] [,2]
[1,] 0.0 1
[2,] 0.5 0
