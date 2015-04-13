#' General Purpose Optimization Techniques
#' 
#' Some examples of applying simulated annealing and L-BFGS optimization techniques.
#' 
#' @author Brian Silva
#' 

# The two packages we are going to be using
packages <- c('GenSA', 'lbfgs')

# Install the packages
install.packages(packages)

# Load packages
lapply(packages, require, character.only=T)

# Example - Simulated Annealing

# An example using GenSA for simulated annealing

# Practice

# An example to practice using GenSA for simulated annealing

# Example - Limited Memory BFGS

# An example using lbfgs for L-BFGS

# Practice

# An example to practice using lbfgs for L-BFGS
