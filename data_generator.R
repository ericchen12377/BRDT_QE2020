cvec <- seq(0,10,1)
nvec <- seq(1,500,1)
Rvec <- c(seq(0.8,0.95,0.01))
seed <- 10
M <- 5000
library(plyr)


#real
a <- 9; b <- 60.86
pi_real <- pi_MCSim_beta(M, seed, a, b)

AddAP_costs <- function(data,M,pi,Cf,Cv,Cw,N,G){
  n = data$n
  c = data$c
  AP = c()
  D = c()
  W_cost = c()
  G_cost = c()
  exp_cost = c()
  for(i in 1:dim(data)[1]){
    AP[i] = bacceptprob(n[i],c[i],pi)
    D[i] = bcost_RDT(Cf,Cv,n[i])
    W_cost[i] = bcost_WS(Cw,N,n[i],c[i],pi)
    G_cost[i] = bcost_RG(G)
    exp_cost[i] = D[i] + AP[i]*W_cost[i] + (1 - AP[i])*G_cost[i]
  }
  data$AP = AP
  data$D = D
  data$W = W_cost
  data$G = G_cost
  data$exp_cost = exp_cost
  return(data)
}
library(foreach)
library(doParallel)
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)
data <- as.data.frame(data_real_CR05)
AP_fprob <- foreach(i = 1:dim(data)[1],.combine = 'rbind',.packages = 'OptimalRDT') %dopar% {
  #cat(paste("Starting iteration",i,"\n"),file="MS1_log.txt", append=TRUE)
  c(bacceptprob(n[i],c[i],pi_real),bcost_WS(1,1,n[i],c[i],pi_real))
}


AddAP_fprob <- function(data,pi){
  n = data$n
  c = data$c
  require(foreach)
  require(doParallel)
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  AP_fprob <- foreach(i = 1:dim(data)[1],.combine = 'rbind',.packages = 'OptimalRDT') %dopar% {
    #cat(paste("Starting iteration",i,"\n"),file="MS1_log.txt", append=TRUE)
    c(bacceptprob(n[i],c[i],pi),bcost_WS(1,1,n[i],c[i],pi))
  }
  data$AP <- AP_fprob[,1]
  data$fprob <- AP_fprob[,2]
  stopCluster(cl)
  return(data)
}
Addcosts <- function(data,Cf,Cv,Cw,N,G){
  n = data$n
  c = data$c
  AP = data$AP
  fprob = data$fprob
  D <- Cf + Cv*n
  W_cost <- Cw*N*fprob
  G_cost = G
  exp_cost = D + AP*W_cost + (1 - AP)*G_cost
  data$D = D
  data$W = W_cost
  data$G = G_cost
  data$exp_cost = exp_cost
  return(data)
}
findoptimal <- function(data){
  require(dplyr)
  return(data %>% group_by(c,R) %>% filter(CR == max(CR)) %>% filter(exp_cost == min(exp_cost)))
}

data <- AddAP_fprob(data,pi_real)
data_real_CR05 <- Addcosts(as.data.frame(data),Cf=0,Cv=125,Cw=5,N=2000000,G=100000)
data_real_CR05_optimal <- findoptimal(data_real_CR05[data_real_CR05$CR<=0.05,])
testAPcost <- data_real_CR05_optimal






data_real_CR05 <- critvals_M_info96086[critvals_M_info96086[, 'R'] >= 0.8&
                                    critvals_M_info96086[, 'CR'] <= 0.05&
                                      critvals_M_info96086[, 'PR'] >=0,
                                         c('n','c','R', 'CR', 'PR')]

data_real_CR05 <- critvals_M_info96086[critvals_M_info96086[, 'R'] >= 0.8,
                                       c('n','c','R', 'CR', 'PR')]
data_real_CR05 <- AddAP_fprob(as.data.frame(data_real_CR05),pi_real)




data_real_CR05 <- Addcosts(as.data.frame(data_real_CR05),Cf=0,Cv=125,Cw=5,N=10000,G=10000)

data_real_CR05_optimal <- findoptimal(data_real_CR05)

data_optimal_real_CR05 <- critvals_min_info96086_CR05[critvals_min_info96086_CR05$R >= 0.8,
                                                      c('n','c','R', 'CR', 'PR')]

data_optimal_real_CR05 <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=100,G=10000)



#informative prior 0218
a <- 2; b <- 18
pi_0218 <- pi_MCSim_beta(M, seed, a, b)

data_optimal_info0218_CR05 <- critvals_min_info0218_CR05[[2]][critvals_min_info0218_CR05[[2]]$R >= 0.8,
                                                      c('n','c','R', 'CR', 'PR')]

data_optimal_info0218_CR05 <- AddAP_costs(data_optimal_info0218_CR05,M,pi = pi_0218,Cf=0,Cv=125,Cw=5,N=100,G=10000)




##############################################################################
###generate data for different prior
data <- as.data.frame(critvals_M_info96086[critvals_M_info96086[, 'R'] >= 0.8,
                              c('n','c','R')])

AddAP_fprob_CR <- function(data,pi){
  n = data$n
  c = data$c
  R = data$R
  require(foreach)
  require(doParallel)
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  AP_fprob_CR <- foreach(i = 1:dim(data)[1],.combine = 'rbind',.packages = 'OptimalRDT') %dopar% {
    #cat(paste("Starting iteration",i,"\n"),file="MS1_log.txt", append=TRUE)
    c(bacceptprob(n[i],c[i],pi),bcost_WS(1,1,n[i],c[i],pi),bconsumerrisk(n[i],c[i],pi,R[i]))
  }
  data$AP <- AP_fprob_CR[,1]
  data$fprob <- AP_fprob_CR[,2]
  data$CR <- AP_fprob_CR[,3]
  stopCluster(cl)
  return(data)
}
#informative prior 0109
a <- 1; b <- 9
pi_0109 <- pi_MCSim_beta(M, seed, a, b)
data_info0109 <- AddAP_fprob_CR(data,pi_0109)

#informative prior 0218
a <- 2; b <- 18
pi_0218 <- pi_MCSim_beta(M, seed, a, b)
data_info0218 <- AddAP_fprob_CR(data,pi_0218)

#informative prior 0436
a <- 4; b <- 36
pi_0436 <- pi_MCSim_beta(M, seed, a, b)
data_info0436 <- AddAP_fprob_CR(data,pi_0436)

#informative prior 0.52
a <- 0.5; b <- 2
pi_052 <- pi_MCSim_beta(M, seed, a, b)
data_info052 <- AddAP_fprob_CR(data,pi_052)

#informative prior 7.530
a <- 7.5; b <- 30
pi_7530 <- pi_MCSim_beta(M, seed, a, b)
data_info7530 <- AddAP_fprob_CR(data,pi_7530)

#informative prior 1560
a <- 15; b <- 60
pi_1560 <- pi_MCSim_beta(M, seed, a, b)
data_info1560 <- AddAP_fprob_CR(data,pi_1560)

#non_informative prior 0218
a <- 1; b <- 1
pi_0101 <- pi_MCSim_beta(M, seed, a, b)
data_info0101 <- AddAP_fprob_CR(data,pi_0101)

save.image("G:/My Drive/research topics/2017 SRDT & warranty/QE target/Coding/QE_revision/real_demo.RData")


findoptimal_noCR <- function(data){
  require(dplyr)
  return(data %>% group_by(c,R) %>% filter(exp_cost == min(exp_cost)))
}
data_info0109_cost <- Addcosts(as.data.frame(data_real_CR05),Cf=0,Cv=125,Cw=5,N=200000,G=100000)
data_info0109_cost_optimal <- findoptimal_noCR(data_info0109_cost[data_info0109_cost$CR<=0.05,])
data_info0109_cost_optimal[which.min(data_info0109_cost_optimal$exp_cost),]

data_info0109_cost_optimal <- findoptimal(data_info0109_cost[data_info0109_cost$CR<=0.05,])
data_info0109_cost_optimal[which.min(data_info0109_cost_optimal$exp_cost),]

testAPcost <- data_info0109_cost_optimal
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)
cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min-50000,max),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}


temp <- data_real_CR05


temp <- data_info0109
temp <- data_info0218
temp <- data_info0436
temp <- data_info052
temp <- data_info7530
temp <- data_info1560
temp <- data_info0101


Cf = 0
Cv = 15
Cw = 3
rbind.data.frame(
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=20000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=50000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=100000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=150000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=200000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=300000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=400000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=500000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=1000000,G=150000)[,1:4],
  Prior_compare(temp,Cf=Cf,Cv=Cv,Cw=Cw,N=2000000,G=150000)[,1:4]
)

temp <- Addcosts(as.data.frame(temp),Cf=0,Cv=5,Cw=3,N=140000,G=100000)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]



testAPcost <- temp

Prior_compare <- function(tmp,Cf,Cv,Cw,N,G){
  tmp <- Addcosts(as.data.frame(tmp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
  tmp <- findoptimal(tmp[tmp$CR<=0.05,])
  return(tmp[which.min(tmp$exp_cost),])
}

Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=20000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=50000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=100000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=150000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=200000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=300000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=400000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=500000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=1000000,G=150000)
Prior_compare(temp,Cf=0,Cv=125,Cw=5,N=2000000,G=150000)












