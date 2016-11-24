
makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
        x <<- y
        invmax <<- NULL
    }
    get <- function() x
    setinvmax <- function(invmax) m <<- invmax
    getinvmax <- function() m
    list(
        set = set,
        get = get,
        set_inverse_matrix = setinvmax,
        get_inverse_matrix = getinvmax
    )
}

cacheSolve <- function(x,...){
    m <- x$get_inverse_matrix()
    
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse_matrix(m)
    m
}