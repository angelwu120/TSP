# TSP

setwd("/Users/Angie/Desktop/")
raw150 <- read.csv("data150cities.csv",sep = ",")
raw15 <- read.csv("data15points.csv",sep = ",")

## Generate city matrix ##
city <- data.matrix(raw150[,2:3])
city <- na.omit(city)
n <- nrow(city)
d <- array(data=NA, c(n,n))

## Caculate the distances between every two cities ##
for (i in 1:n) {
  for (j in 1:n) {
    d[i,j] <- ((city[i,1] - city[j,1])^2 + (city[i,2] - city[j,2])^2)^(1/2)
    if (d[i,j]==0) {
      d[i,j] <- NA
    }
  }
}

## Find the nearest nodes and move them from v into the set c ##
m <- which(d==d[which.min(d)],arr.ind=T)
v <- array(data=1:n,c(n,1))
c <- array(data=0:0,c(n,n))
I <- m[1,1]
J <- m[1,2]
v[I] <- NA
v[J] <- NA
c[I,J] <- 1
c[J,I] <- 1

## Select the closest node k in the set v and move it from v to c ##
while (length(v[is.na(v)])<n) {
  x <- array(data=NA,c(n,n,n))
   for (i in 1:n) {
      for (j in 1:n) {
        if (c[i,j]==1 ) {
          for (k in 1:n) {
            if (!is.na(v[k])) {
                x[i,j,k] <- d[i,k] + d[k,j] - d[i,j]
             }
           }
         }
       }
    }
  m <- which(x==x[which.min(x)],arr.ind=T)
  v[m[1,3]] <- NA
  c[m[1,1],m[1,2]] <- 0
  c[m[1,1],m[1,3]] <- 1
  c[m[1,3],m[1,2]] <- 1
}

d[is.na(d)]<-0

## Total cost ##
sum(d*c) 


## Implement package "tspmeta": https://cran.r-project.org/web/packages/tspmeta/index.html ##
install.packages("tspmeta")
library(tspmeta)
dist.mx <- dist(city)
tsp.ins <- tsp_instance(city, dist.mx )
tour <- run_solver(tsp.ins, method="2-opt")
autoplot(tsp.ins, tour)
tour
