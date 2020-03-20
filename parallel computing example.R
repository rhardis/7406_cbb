library(doParallel)
registerDoParallel(cores=8)

x = iris[which(iris[,5] != "setosa"), c(1,5)]
trials=1000000

ptime1 = system.time({
  r = foreach(icount(trials), .combine=cbind) %dopar% {
    ind = sample(100, 100, replace=T)
    result1 = glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
ptime1

#On Rich's computer, ptime2=~14.9 seconds for trials=10000
ptime2 = system.time({
  r = foreach(icount(trials), .combine=cbind) %do% {
    ind = sample(100, 100, replace=T)
    result1 = glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  }
})[3]
ptime2