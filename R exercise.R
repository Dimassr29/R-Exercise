#Dimas Setyawan Ramadhansyah17523152
#Yudhistira Adinugraha Hutabarat 17523180

#Limit
# No 1

f <- function(x){
  fx <- (1- cos(x))/x
  return (fx)
}

library(Ryacas)
x <- Sym("x")
Limit(f(x),x,0)

# No 2
f <- function(x){
  fx <- (2*(-3+x)^2-18)/x
  return (fx)
}

library(Ryacas)
x <- Sym("x")
Limit(f(x),x,0)

# No 3
f <- function(x){
  fx <- (x- sqrt(3*x+4))/4-x
  return (fx)
}

library(Ryacas)
x <- Sym("x")
Limit(f(x),x,4)

#Differintation
#No 1

y <- function(x, n){ 
  return(n * x^(n-1)) 
} 

library(Ryacas) 
x <- Sym("x") 
Simplify(deriv(sqrt(x)*x+1, x))

#No 2
y <- function(x, n){ 
  return(n * x^(n-1)) 
} 

library(Ryacas) 
x <- Sym("x") 
Simplify(deriv(2*x^2-3 / sqrt(x), x))

#No 3
y <- function(x, n){ 
  return(n * x^(n-1)) 
} 

library(Ryacas) 
x <- Sym("x") 
Simplify(deriv((x-1)/(x+1), x))

#No 4
#a) 
fx <- function (x){
  result <- 3 * x/(2 * root(x, 2))
  return(result)
}
fx(4)

#b)
fx <- function (x){
  result <- 8 * (x^2 * root(x, 2)) + 3/(2 * (x * root(x, 2)))
  return(result)
}
fx(2)

#c)
fx <- function (x){
  result <- 2/(x^2 + 2 * x + 1)
  return(result)
}
fx(3)

#Integration

#No 1
integrand <- function(x){
  return(2*x^3)
}
library(Ryacas)
x <- Sym ("x")
Integrate(2*x^3,x)
integrate(f = integrand, lower = 0, upper = 3)

#No 2
integrand <- function(x){
  return(1-5*x^4)
}
library(Ryacas)
x <- Sym ("x")
Integrate(1-5*x^4,x)
integrate(f = integrand, lower = -1, upper = 2)

#No 3
integrand <- function(x){
  return(x^4-3*x^2+5)
}
library(Ryacas)
x <- Sym ("x")
Integrate(x^4-3*x^2+5,x)
integrate(f = integrand, lower = -1, upper = 2)

#No 4 

integrand <- function(x){
  return(x^2 + (1/(2* sqrt(x))))
}

integrate(f = integrand, lower = 1, upper = 4)

library(Ryacas)
x <- Sym("x")
Integrate(x^2 + (1/(2* sqrt(x))),x)


#No 5 

integrand <- function(x){
  return(x*((1-3*x)^2))
}

integrate(f = integrand, lower = 0, upper = 2)

library(Ryacas)
x <- Sym("x")
Integrate(x*((1-3*x)^2),x)

