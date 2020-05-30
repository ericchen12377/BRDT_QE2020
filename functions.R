
######define the indicator function
bIndicator<-function(pi,R)
{
  if(pi<=1-R)
    return(1)
  else
    return(0)
}

######define the inside sum core of integration fucntion
bcore<-function(n,c,pi){
  sum1<-c()
  for(i in 0:c){
    sum1[i+1]<-dbinom(i,n,prob=pi)
  }
  return(sum(sum1))
} 
## pbinom(1,2,0.3) = bcore(2,1,0.3)


######define the consumer risk function
bconsumerrisk<-function(M,n,c,pi,R){
  sum1<-c()
  sum2<-c()
  for(i in 1:M){
    sum1[i]<-pbinom(c,n,pi[i])*bIndicator(pi[i],R)
    sum2[i]<-pbinom(c,n,pi[i])
  }
  return(1-sum(sum1)/sum(sum2))
}

######define the producer risk function
bproducerrisk<-function(M,n,c,pi,R){
  sum1<-c()
  sum2<-c()
  for(i in 1:M){
    sum1[i]<-(1-pbinom(c,n,pi[i]))*bIndicator(pi[i],R)
    sum2[i]<-1-pbinom(c,n,pi[i])
  }
  return(sum(sum1)/sum(sum2))
}

######define the acceptance probability function
bacceptprob<-function(M,n,c,pi){
  sum<-c()
  for(i in 1:M){
    sum[i]<-pbinom(c,n,pi[i])
  }
  return(mean(sum))
}

######define the rejection probability function
brejectprob<-function(M,n,c,pi){
  sum<-c()
  for(i in 1:M){
    sum[i]<-1-pbinom(c,n,pi[i])
  }
  return(mean(sum))
}

######define the expected failure probability function
bfailureprob <- function(R,pi,M){
  sum<-c()
  for(i in 1:M){
    sum[i]=pi[i]*bIndicator(pi[i],R)
  }
  return(mean(sum))
}

#######define the cost function
bcost<-function(n,R,Cq,N,W){
  C1=c()
  C2=c()
  #RDT cost
  C1 = (Cq*W)*n
  #warranty cost
  C2 = N*W*(1-R)
  ######Total cost########
  result = c(C1+C2,C1,C2)
  return(result)
}

#cost modification function
cost_modify <- function(data,Cq,N,W){
  data$Rc = (Cq*W)*data$n
  data$Wc = N*W*(1-data$R)
  data$Cost = data$Rc + data$Wc
  return(data)
}

AddAP <- function(data,M,pi){
  n = data$n
  c = data$c
  AP = c()
  for(i in 1:dim(data)[1]){
    AP[i] = bacceptprob(M,n[i],c[i],pi)
  }
  data$AP = AP
  return(data)
}
cost_modify_AP <- function(data,Cq,N,W,G){
  data$Rc = (Cq*W)*data$n
  data$Wc = N*W*(1-data$R)
  data$APWc = data$AP*data$Wc
  data$APG = (1-data$AP)*G
  data$APCost = data$Rc + data$APWc + data$APG
  data$Cost = data$Rc + data$Wc
  return(data)
}

#M is simultion sample size
#n is test sample size
#c is maximum allowable failure
#pi is failure probability in test period
#R is minimum reliability requirement
#Cq is per unit reliability cost ratio against warranty cost W
#Nq is sales volumn against test sample size n
#W is per unit warranty cost
criteria_function<- function(M,n,c,pi,R,Cq,N,W){
  cr = bconsumerrisk(M,n,c,pi,R)
  pr = bproducerrisk(M,n,c,pi,R)
  cost = bcost(n,R,Cq,N,W)
  return(c(c,n,R,cr,pr,cost))
}

listtomatrix = function(list){
  y <- c()
  for(i in 1:length(list)){
    temp = list[[i]]
    y = rbind(y,temp)
  }
  return(y)
}
#find minimum sample size for each c and R, based on CR

na.num=function(x)
{
  sum(is.na(x))==0
}

Todataframe = function(data){
  result = c()
  data = as.data.frame(data)
  for(i in 1:dim(data)[2]){
    result = cbind(result,as.numeric(data[,i]))
  }
  result = as.data.frame(result)
  return(result)
}


findminimum = function(data,cvec,nvec,Rvec,CRthres){
  all = as.data.frame(data)
  temp = c()
  for ( i in 1:length(Rvec)){
    for( j in 1:length(cvec)){
      test = all[all$c==cvec[j]&all$R==Rvec[i],]
      test_CR = test[test$CR<=CRthres,]
      nmin = min(as.numeric(test_CR$n))
      temp = rbind(temp,test_CR[test_CR$n==nmin,])
    }
  }
  result = temp
  return(result)
}











######plot function
bplot_cost = function(x,xtext){

  plot(x[1,],ylim = c(0,max(x)),type = 'b',ann = F,xaxt='n')
  axis(side=1,at=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),labels=xtext)
  index = match(min(x[1,]),x[1,])
  points(index,x[1,index],pch=19,col=2,cex=1)
for(i in 2:dim(x)[1]){
  lines(x[i,],type = 'b')
  index = match(min(x[i,]),x[i,])
  points(index,x[i,index],pch=19,col=2,cex=1)
}
}







