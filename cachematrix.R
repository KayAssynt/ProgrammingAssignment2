## This pair of functions cache the inverse of a matrix, saving on costly repeat computations.
## Assumes that matrix is always invertible

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL 
        ## Two variables are created within the function: 
        ## x is initialised by an optional parameter, otherwise it's set to an empty matrix
        ## im is initialised to NULL, indicating no stored value
        ## (it will eventually hold the cached inverse matrix)
        
        
        ## Four nested functions are defined (set, get, setsolve, getsolve), 
        ## allowing for access to m and x within makeCacheMatrix
        
        set <- function(y){     ##Set the matrix x to the value passed in
                if(!identical(x,y)) {  
                                ## Checks if new matrix is different to the 
                                ## old one and if it is it clears the old inverse
                x<<-y           ## Assigns matrix x in higher environment
                im<<-NULL}      ## Sets im to NULL so that the inverse of 
                                ## the new x matrix needs to be calculated 
        }
        
        get <-function() x      ## return the value of x stored in the makeCacheMatrix object
        
        setsolve <-function(solve) im<<-solve
        
                                ## set the value of im, i.e. store parameter called 'solve'
        
        getsolve<-function() im ## return the value of m stored in the makeCacheMatrix object
        
        
        list(set=set, get=get,  
             setsolve=setsolve,
             getsolve=getsolve)
                                ## Create a generic list of nested functions defined above 
                                ## which is returned by makeCacheMatrix. It takes tag=value pairs
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## cacheSolve requires one parameter, x, that must be a makeCacheMatrix
        ## object.  Optional parameters after that are passed to the
        ## solve() function.  Two local variables im and data are used
        ## in cacheSolve
        
        im<-x$getsolve()        ## Use the getsolve() in the makeCacheMatrix object and 
                                ## store the cached inverse value in the local variable im 
        if(!is.null(im)){       ## If im is not NULL then there is cached inverse for an identical matrix
                message("getting cashed data")
                return(im)      ## return the cached inverse
        }              
        data<-x$get()           ## Else, using get() function in the makeCacheMatrix object, 
                                ## assign the matrix of data to the local variable 'data'
        im<-solve(data,...)     ## Calculate the inverse of data passing 
                                ## any optional parameters received
        x$setsolve(im)          ## Cache the newly calculated inverse matrix in macheCacheMatrix
        im                      ## return newly calculated inverse matrix
}