#' General Purpose Optimization Techniques
#' 
#' This script provides some examples of applying BFGS/L-BFGS and simulated annealing optimization techniques.
#' 
#' @author Brian Silva
#' 

## Some initial setup

# The packages we are going to be using
packages <- c('GenSA', 'optimx',  'plot3D')

# Install the packages if necessary
install.packages(packages)

# Load packages
lapply(packages, require, character.only=T)

# Function to measure elapsed time
elapsed <- function(fn) {
  
  return(round(system.time(suppressWarnings(fn))[3], 4))
  
}

## Examples - BFGS/L-BFGS-B

# Log-likelihood of normal distribution
norm.loglik <- function(theta, X){
  mu <- theta[1]
  sigma2 <- theta[2]
  n = length(X)
  return(-0.5 * n * log(2 * pi) - 0.5 * n * log(sigma2) - (0.5 / sigma2) * sum((X - mu)^2))
}

norm.loglik.grad <- function(theta, X) {
  mu <- theta[1]
  sigma2 <- theta[2]
  n = length(X)
  return(c(1/sigma2 * (sum(X) - n*mu), 
           0.5/sigma2 * ((1/sigma2) * sum((X-mu)^2) - n)))
}

# Optimization
X <- rnorm(10000, mean=2, sd=3)

# With gradient
time <- elapsed(ret <- optimx(par=c(0, 1), 
                              fn=norm.loglik, 
                              gr=norm.loglik.grad, 
                              lower=c(-Inf, 0), 
                              upper=c(Inf, Inf), 
                              X=X, 
                              method='L-BFGS-B', 
                              control=list(maximize=T)))

ret
paste('Elapsed:', time, 'seconds')

# Without gradient
# Uses finite-difference approximation
time <- elapsed(ret <- optimx(par=c(0, 1), 
                              fn=norm.loglik, 
                              lower=c(-Inf, 0), 
                              upper=c(Inf, Inf), 
                              X=X, 
                              method='L-BFGS-B', 
                              control=list(maximize=T)))

ret
paste('Elapsed:', time, 'seconds')

# Practice

# TODO: Minimize bfgs.prac using optimx's BFGS method with initial x = c(0, 0)

bfgs.prac <- function(x) {
  
  return(exp(x[1] - 1) + exp(-x[2] + 1) + (x[1] - x[2])^2)
  
}

bfgs.prac.grad <- function(x) {
  
  return(c(exp(x[1]-1) + 2 * (x[1] - x[2]), 
           -exp(-x[2] + 1) - 2 * (x[1] - x[2])))
  
}

# Add your code to the function below

time <- elapsed(ret <- optimx(# Your code here
                              ))

ret
paste('Elapsed:', time, 'seconds')

# Compare your result with the following

# With gradient
time <- elapsed(ret <- optimx(par=c(0, 0), 
                              fn=bfgs.prac, 
                              gr=bfgs.prac.grad, 
                              method='BFGS'))

ret
paste('Elapsed:', time, 'seconds')

# Without gradient
time <- elapsed(ret <- optimx(par=c(0, 0), 
                              fn=bfgs.prac, 
                              method='BFGS'))

ret
paste('Elapsed:', time, 'seconds')


## Examples - Simulated Annealing

# Rastrigin function
Rastrigin <- function(x) {

    return(sum(x^2 - 10 * cos(2 * pi * x)) + 10 * length(x))

}

# Plot Rastrigin where x = (x1, x2)
resolution <- 200
M <- mesh(seq(-5.12, 5.12, length.out=resolution),
          seq(-5.12, 5.12, length.out=resolution))
x1 <- M$x
x2 <- M$y
z <- apply(array(c(x1, x2), dim=c(resolution, resolution, 2)), c(1,2), Rastrigin)
surf3D(x1, x2, z, phi=30, colkey=F, main='Rastrigin Function')

# Optimization
dimension <- 30

# -5.12 <= x_i <= 5.12
lower <- rep(-5.12, dimension)
upper <- rep(5.12, dimension)

time <- elapsed(ret <- GenSA(lower=lower, 
                             upper=upper, 
                             fn=Rastrigin))

ret[c('value', 'par', 'counts')]
paste('Elapsed:', time, 'seconds')

# Ackley's function
Ackley <- function(x) {

    return(-20 * exp(-0.2 * sqrt((1/length(x)) * sum(x^2))) - exp((1/length(x)) * sum(cos(2 * pi * x))) + 20 + exp(1))

}

# Plot Ackley's Function where x = (x1, x2)
resolution <- 200
M <- mesh(seq(-5, 5, length.out=resolution),
          seq(-5, 5, length.out=resolution))
x1 <- M$x
x2 <- M$y
z <- apply(array(c(x1, x2), dim=c(resolution, resolution, 2)), c(1,2), Ackley)
surf3D(x1, x2, z, phi=30, colkey=F, main='Ackley\'s Function')

# Optimization
dimension <- 30

# -5 <= x_i <= 5
lower <- rep(-5, dimension)
upper <- rep(5, dimension)

time <- elapsed(ret <- GenSA(lower=lower, 
                             upper=upper, 
                             fn=Ackley))

ret[c('value', 'par', 'counts')]
paste('Elapsed:', time, 'seconds')

# Practice

# TODO: find the global minimum of the Levi function using simulated annealing
# Note: dimension = 2

# Levi function
Levi <- function(x) {
  
  return((sin(3 * pi * x[1]))^2 + (1 + (sin(3 * pi * x[2]))^2) * (x[1] - 1)^2 + (1 + (sin(2 * pi * x[2]))^2) * (x[2] - 1)^2)
  
}

# Plot Levi Function where x = (x1, x2)
resolution <- 200
M <- mesh(seq(-10, 10, length.out=resolution),
          seq(-10, 10, length.out=resolution))
x1 <- M$x
x2 <- M$y
z <- apply(array(c(x1, x2), dim=c(resolution, resolution, 2)), c(1,2), Levi)
surf3D(x1, x2, z, phi=20, colkey=F, main='Levi Function')

# Add your code to the function below (Note: dimension=2 and -10 <= x_1, x_2 <= 10)

time <- elapsed(ret <- GenSA(# Your code here
                             ))

ret[c('value', 'par', 'counts')]
paste('Elapsed:', time, 'seconds')

# Compare your result with the following

# Optimization
dimension <- 2

# -10 <= x_i <= 10
lower <- rep(-10, dimension)
upper <- rep(10, dimension)

time <- elapsed(ret <- GenSA(lower=lower, 
                             upper=upper, 
                             fn=Levi))

ret[c('value', 'par', 'counts')]
paste('Elapsed:', time, 'seconds')
