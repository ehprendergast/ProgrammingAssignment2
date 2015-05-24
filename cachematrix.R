# the script is designed to calculate the inverse of a matrix unless it is already saved in cache
# in which case it will take the mean from the cache instead

# defines a function makeCacheMatrix which takes a default empty numeric vector or one
# numeric argument otherwise

makeCacheMatrix <- function(x = matrix()) {
        # set the variable m to NULL automatically, before anything else
        m <- NULL
        
        # create a function called set() which parent-assigns its argument to x
        # and parent-assigns m to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # create a function get() that just returns the value of the variable x
        get <- function() x
        
        # create a function that takes an argument (which is the matrix inverse) and 
        # parent-assigns it to m
        setsolve <- function(inverse) {
                m <<- inverse
        }
        
        # create a function that returns the variable m
        getsolve <- function() m
        
        # return a list that executes set(), get(), setsolve(), and getsolve()
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
        
}

# create a cacheSolve function that takes a list argument produced from makeCacheVector
cacheSolve <- function(x, ...) {
        # call the getsolve() function from x
        m <- x$getsolve()
        
        # check to see if m provided by getsolve() exists, return a message if it does
        # and then return that m, stopping the program
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # assign to data the value returned by get() from x (which pulls x
        # from makeCacheVector)
        data <- x$get()
        
        # take the solve of data plus any arguments passed through cachemean
        # and assign it to m (m must have been null)
        
        m <- solve(data, ...)
        
        # set the inverse you just calculated to x using setsolve()
        x$setsolve(m)
        
        # return m
        m        
}
