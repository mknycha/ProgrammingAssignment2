##makeCacheMatrix creates a kind of reference to the our matrix.
##This way, we will be able to pass functions and values inside it to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #We create an inv variable. Currently it's empty but using <<- operator
                #we wll be able to change its value later (from inside a function).
    get <- function() x #This function returns the matrix
    setinv <- function(solve) inv <<- solve #assignes value to inv (defined under makeMatrix function)
    getinv <- function() inv #This function returns the inv value
    return(list(get=get, 
                setinv=setinv,
                getinv=getinv)) #Returns a list of the functions. Therefore, these are available in CacheSolve when we run it.
}

##cacheSolve takes as an input the return of makeCacheMatrix for particular matrix. 
##For instance, the valid input can be: makeCacheMatrix(matrix(rnorm(16),4,4)).
##Then, it checks if the value of inv (returned from makeCacheMatrix). If it's not empty, then it's current value will be returned. 
##Otherwise it gets the matrix, calculates it's inversion and stores it in under inv variable.

cacheSolve <- function(x, ...) {
    m <- x$getinv() #Retrieve the value of inv defined earlier and assign it to m variable.
    if(!is.null(m)) { #Check if m is empty. If it is, message and the m value is returned, ending the function.
        message("getting cached data")
        return(m)
    }
    data <- x$get() #Retrieve the matrix and assign it to data.
    m <- solve(data, ...) #Calculate the inversion of a matrix.
    x$setinv(m) #Assign the matrix inversion to inv
    m ## Return a matrix that is the inverse of 'x'
}
