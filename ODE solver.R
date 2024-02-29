#forward
SHOeuler <- function(ini, k, m, g, beta, delta, n){
  inimat <- matrix(ini, nrow = 2, ncol=1)
  X = list()
  mu = list()
  for (x in 1:n) {
    xap <- inimat[2,]
    muap <- g -(k/m)*inimat[1,]-(beta/m)*inimat[2,]
    matp <- matrix(c(xap,muap),nrow = 2, ncol=1)
    akhir = inimat + delta*matp
    inimat <- akhir
    X = append(X,akhir[1,])
    mu = append(mu, akhir[2,])
  }
  cat('x:',akhir[1,])
  cat('mu:',akhir[2,])
  plot(X,mu)
}
SHOeuler(c(-1,-2),2,0.5,0,0,0.1,200)

SHOeuler(c(-1,-2),2,0.5,0,0,0.11,200)

SHOeuler(c(-1,-2),2,1,0,0,0.05,10)

#backward
SHObeuler <- function(ini, k, m, g, beta, delta, n){
  inimat <- matrix(ini, nrow = 2, ncol=1)
  X = list()
  mu = list()
  for (x in 1:n) {
    xap <- inimat[2,]
    muap <- g -(k/m)*inimat[1,]-(beta/m)*inimat[2,]
    matp <- matrix(c(xap,muap),nrow = 2, ncol=1)
    fakhir = inimat + delta*matp
    xap <- fakhir[2,]
    muap <- g -(k/m)*fakhir[1,]-(beta/m)*fakhir[2,]
    matp <- matrix(c(xap,muap),nrow = 2, ncol=1)
    akhir = inimat + delta*matp
    inimat <- akhir
    X = append(X,akhir[1,])
    mu = append(mu, akhir[2,])
  }
  cat('x:',akhir[1,])
  cat('mu:',akhir[2,])
  plot(X,mu)
}
SHObeuler(c(-1,-2),2,0.5,0,0,0.1,5)

SHObeuler(c(-1,-2),2,0.5,0,0,0.1,200)
SHObeuler(c(-1,-2),2,0.5,0,0,0.11,200)

#trapezoidal
SHOtrapez <- function(ini, k, m, g, beta, delta, n){
  inimat <- matrix(ini, nrow = 2, ncol=1)
  X = list()
  mu = list()
  for (x in 1:n) {
    xap <- inimat[2,]
    muap <- g -(k/m)*inimat[1,]-(beta/m)*inimat[2,]
    matp <- matrix(c(xap,muap),nrow = 2, ncol=1)
    fakhir = inimat + delta*matp
    xap <- fakhir[2,]
    muap <- g -(k/m)*fakhir[1,]-(beta/m)*fakhir[2,]
    matp <- matrix(c(xap,muap),nrow = 2, ncol=1)
    akhirb = inimat + delta*matp
    akhir = (fakhir+akhirb)/2
    inimat <- akhir
    X = append(X,akhir[1,])
    mu = append(mu, akhir[2,])
  }
  cat('x:',akhir[1,])
  cat('mu:',akhir[2,])
  plot(X,mu)
}
SHOtrapez(c(-1,-2),2,0.5,0,0,0.1,5)

SHOtrapez(c(-1,-2),2,0.5,0,0,0.01,500)

SHOtrapez(c(-1,-2),2,0.5,0,0,0.01,5000)

SHOtrapez(c(-1,-2),2,0.5,0,0,0.05,10)
