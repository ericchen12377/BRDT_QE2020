testAPcost <- data_optimal_real_CR05
Rvec <- seq(0.8,0.95,0.01)
cvec <- seq(0,10,1)

pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_APtradeoff_pattern.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))
N = 11
cols=gray.colors(N, start = 0, end = 0.9)
plot(c(0,200),c(0,1.1),ylab="Acceptance probability",
     xlab="Minimum sample size",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$n[testAPcost$R==Rvec[i]],testAPcost$AP[testAPcost$R==Rvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=c(0:10))
}
points(rep(130,length(cols)),seq(0.5,1.1,length=length(cols)),pch=15,cex=3,col=sort(cols,decreasing = F))
text(rep(150,2),c(0.5,1.1),labels=paste("R=",c(0.95,0.8),sep=""),cex=2)
arrows(x0=150, y0=1.05,x1=150, y1=0.55, angle= 30,code=2,lwd=5,col=cols[10])
legend("topright",col = cols[5],pch=c(0:10)
       ,legend = c("c=0","c=1","c=2","c=3","c=4","c=5",
                   "c=6","c=7","c=8","c=9","c=10"),bty="n",cex = 2,lwd=3)
legend("topleft",col = 1,pch=17
       ,legend = c("CR <= 0.05"),bty="n",cex = 2,lwd=3)
dev.off()








pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_traditional.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))
testAPcost[testAPcost$D==min(testAPcost$D),]
max = round_any(max(testAPcost$D),10000,ceiling)
min = round_any(min(testAPcost$D),10000,floor)


cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min,max),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2,cex.axis=2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$D[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}
temp=testAPcost[testAPcost$D==min(testAPcost$D),]
points(temp$R,temp$D,col=3,pch=19)
text(temp$R+0.03,min+15000, labels = paste("n =",temp$n,",","c =",temp$c,",","R =",temp$R)
     ,col=3,cex=2,pch=19)
points(rep(0.96,length(cols)),seq(min+10000,max-10000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+10000,max-10000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,mean(temp$c)),3,rep(1,length(cvec)-1-mean(temp$c))))
legend("topleft",col = 3
       ,legend = c("Minimum cost, CR <= 0.05"),bty="n",cex = 2,lwd=3,pch=19,lty=NA)

dev.off()




pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_all.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

##Risk-adjusted##
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=1000,Cv=125,Cw=5,N=170000,G=100000)
mean(testAPcost$D)
mean(testAPcost$W)
mean(testAPcost$G)
mean(testAPcost$W*testAPcost$AP)
mean(testAPcost$G*(1 - testAPcost$AP))
testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)
#Cost vs R
cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min,max),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}
temp=testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
points(temp$R,temp$exp_cost,col=2,pch=19)
points(rep(0.96,length(cols)),seq(min+8000,max-8000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+8000,max-8000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))
text(temp$R+0.08,min, labels = paste("n =",temp$n,",","c =",temp$c,",","R =",temp$R,",","AP =",round(temp$AP,4))
     ,col=2,cex=2,pch=19)

tempt=testAPcost[testAPcost$D==min(testAPcost$D),]
points(tempt$R,tempt$exp_cost, col=3,pch=15)
text(tempt$R+0.05,tempt$exp_cost+15000
     ,labels = paste("n =",tempt$n,",","c =",tempt$c,",","R =",tempt$R,",","AP =",round(tempt$AP,4))
     ,col=3,cex=2)

legend("topleft",col = c(2,3),pch = c(19,15)
       ,legend = c("Proposed model","Conventional model")
       ,bty="n",cex = 2,lwd=3,lty=NA)

dev.off()





pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_separate.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

testAPcost$APW <- testAPcost$W*testAPcost$AP
testAPcost$APG <- testAPcost$G*(1 - testAPcost$AP)
###########Seperate###############
plot(c(0.8,0.98),c(0,max-50000),ylab="Cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$D[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=1)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APW[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=17)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APG[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=15)
}
legend(0.85,70000,pch = c(1,17,15)
       ,legend = c("BRDT cost","Expected WS cost","Expected RG cost")
       ,bty="n",cex = 2,lwd=3,lty=NA)
points(rep(0.96,length(cols)),seq(0+15000,max-55000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(0+15000,max-55000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))


dev.off()




######Increasing Pattern######
##Risk-adjusted##
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=10000,G=100000)
mean(testAPcost$D)
mean(testAPcost$W)
mean(testAPcost$G)
mean(testAPcost$W*testAPcost$AP)
mean(testAPcost$G*(1 - testAPcost$AP))
testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)
pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_all_increasing.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))


#Cost vs R
cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min,max),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}
temp=testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
points(temp$R,temp$exp_cost,col=2,pch=19)
points(rep(0.96,length(cols)),seq(min+8000,max-8000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+8000,max-8000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))
text(temp$R+0.08,min, labels = paste("n =",temp$n,",","c =",temp$c,",","R =",temp$R,",","AP =",round(temp$AP,4))
     ,col=2,cex=2,pch=19)

tempt=testAPcost[testAPcost$D==min(testAPcost$D),]
points(tempt$R,tempt$exp_cost, col=3,pch=15)
text(tempt$R+0.08,tempt$exp_cost+5000
     ,labels = paste("n =",tempt$n,",","c =",tempt$c,",","R =",tempt$R,",","AP =",round(tempt$AP,4))
     ,col=3,cex=2)

legend("topleft",col = c(2,3),pch = c(19,15)
       ,legend = c("Proposed model","Conventional model")
       ,bty="n",cex = 2,lwd=3,lty=NA)

dev.off()





pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_separate_increasing.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

testAPcost$APW <- testAPcost$W*testAPcost$AP
testAPcost$APG <- testAPcost$G*(1 - testAPcost$AP)
###########Seperate###############
plot(c(0.8,0.98),c(0,max-50000),ylab="Cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$D[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=1)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APW[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=17)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APG[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=15)
}
legend(0.85,70000,pch = c(1,17,15)
       ,legend = c("BRDT cost","Expected WS cost","Expected RG cost")
       ,bty="n",cex = 2,lwd=3,lty=NA)
points(rep(0.96,length(cols)),seq(0+15000,max-55000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(0+15000,max-55000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))


dev.off()











######Ushape Patterns######
##Risk-adjusted##
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=170000,G=100000)
mean(testAPcost$D)
mean(testAPcost$W)
mean(testAPcost$G)
mean(testAPcost$W*testAPcost$AP)
mean(testAPcost$G*(1 - testAPcost$AP))
testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)

pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_all_Ushape.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

#Cost vs R
cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min,max),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}
temp=testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
points(temp$R,temp$exp_cost,col=2,pch=19)
points(rep(0.96,length(cols)),seq(min+8000,max-8000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+8000,max-8000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))
text(temp$R+0.08,min, labels = paste("n =",temp$n,",","c =",temp$c,",","R =",temp$R,",","AP =",round(temp$AP,4))
     ,col=2,cex=2,pch=19)

tempt=testAPcost[testAPcost$D==min(testAPcost$D),]
points(tempt$R,tempt$exp_cost, col=3,pch=15)
text(tempt$R+0.05,tempt$exp_cost+15000
     ,labels = paste("n =",tempt$n,",","c =",tempt$c,",","R =",tempt$R,",","AP =",round(tempt$AP,4))
     ,col=3,cex=2)

legend("topleft",col = c(2,3),pch = c(19,15)
       ,legend = c("Proposed model","Conventional model")
       ,bty="n",cex = 2,lwd=3,lty=NA)

dev.off()





pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_separate_Ushape.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

testAPcost$APW <- testAPcost$W*testAPcost$AP
testAPcost$APG <- testAPcost$G*(1 - testAPcost$AP)
###########Seperate###############
plot(c(0.8,0.98),c(0,max-50000),ylab="Cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$D[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=1)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APW[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=17)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APG[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=15)
}
legend(0.85,70000,pch = c(1,17,15)
       ,legend = c("BRDT cost","Expected WS cost","Expected RG cost")
       ,bty="n",cex = 2,lwd=3,lty=NA)
points(rep(0.96,length(cols)),seq(0+15000,max-55000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(0+15000,max-55000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))


dev.off()





######Decreasing Patterns######
##Risk-adjusted##
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=2000000,G=100000)
mean(testAPcost$D)
mean(testAPcost$W)
mean(testAPcost$G)
mean(testAPcost$W*testAPcost$AP)
mean(testAPcost$G*(1 - testAPcost$AP))
testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)

pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_all_decreasing.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

#Cost vs R
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
temp=testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
points(temp$R,temp$exp_cost,col=2,pch=19)
points(rep(0.96,length(cols)),seq(min+8000,max-8000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+8000,max-8000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))
text(temp$R,min-40000, labels = paste("n =",temp$n,",","c =",temp$c,",","R =",temp$R,",","AP =",round(temp$AP,4))
     ,col=2,cex=2,pch=19)

tempt=testAPcost[testAPcost$D==min(testAPcost$D),]
points(tempt$R,tempt$exp_cost, col=3,pch=15)
text(tempt$R+0.08,tempt$exp_cost-40000
     ,labels = paste("n =",tempt$n,",","c =",tempt$c,",","R =",tempt$R,",","AP =",round(tempt$AP,4))
     ,col=3,cex=2)

legend("top",col = c(2,3),pch = c(19,15)
       ,legend = c("Proposed model","Conventional model")
       ,bty="n",cex = 2,lwd=3,lty=NA)

dev.off()





pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_proposed_separate_decreasing.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))

testAPcost$APW <- testAPcost$W*testAPcost$AP
testAPcost$APG <- testAPcost$G*(1 - testAPcost$AP)
###########Seperate###############
plot(c(0.8,0.98),c(0,max-50000),ylab="Cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$D[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=1)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APW[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=17)
}
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$APG[testAPcost$c==cvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=15)
}
legend('center',pch = c(1,17,15)
       ,legend = c("BRDT cost","Expected WS cost","Expected RG cost")
       ,bty="n",cex = 2,lwd=3,lty=NA)
points(rep(0.96,length(cols)),seq(0+15000,max-55000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(0+15000,max-55000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))


dev.off()







########################################################
###comprehensive study###
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=3000,G=150000)
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=20000,G=150000)
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=35000,G=150000)
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=50000,G=200000)
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=300000,G=1500000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=1200000,G=1500000)
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=3500000,G=1500000)
testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=5000000,G=1500000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=30000000,G=1500000)


mean(testAPcost$D)
mean(testAPcost$W)
mean(testAPcost$G)
mean(testAPcost$W*testAPcost$AP)
mean(testAPcost$G*(1 - testAPcost$AP))
testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
testAPcost[testAPcost$D==min(testAPcost$D),]


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


testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=20000,G=150000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=90000,G=150000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=185000,G=150000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=275000,G=150000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=365000,G=150000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=365000*5,G=150000)

testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=365000*10,G=150000)


mean(testAPcost$D)
mean(testAPcost$W)
mean(testAPcost$G)
mean(testAPcost$W)/mean(testAPcost$G)
mean(testAPcost$W*testAPcost$AP)
mean(testAPcost$G*(1 - testAPcost$AP))
testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
testAPcost[testAPcost$D==min(testAPcost$D),]




#########################
sensitive = function(N){
  testAPcost <- AddAP_costs(data_optimal_real_CR05,M,pi_real,Cf=0,Cv=125,Cw=5,N=N,G=150000)
  WtG = mean(testAPcost$W)/mean(testAPcost$G)
  DtG = mean(testAPcost$D)/mean(testAPcost$G)
  temp = testAPcost[testAPcost$exp_cost==min(testAPcost$exp_cost),]
  temp1 = testAPcost[testAPcost$D==min(testAPcost$D),]
  return(c(temp$n,temp$c,temp$R,temp$AP,temp$exp_cost,temp$D,temp$AP*temp$W,temp$AP*temp$G,WtG,DtG,temp1$exp_cost))
}

sensitive(20000)
N = seq(0,4000000,10000)
Cq = seq(1,20,1)

#sensitive on N
Nsens = c()
for( i in 1:length(N)){
  temp <- sensitive(N[i])
  Nsens = rbind(Nsens,temp)
}
Nsens = cbind(N,Nsens)
colnames(Nsens) <- c("N","n","c","R","AP","APCost","Rc","APWc","APG","WcG","RcG","traditional")
Nsens = as.data.frame(Nsens)
###Trade-off with increasing W
#AP vs n (diff R) - info96086
pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/real_demo_increasingWG_pattern.pdf",
    width=10, height=8)
par(family='serif')
par(mfrow = c(1,1))
par(mar = c(5, 5, 2, 2))
N = 11
cols=gray.colors(N, start = 0, end = 0.9)
plot(c(0,200),c(0,1.1),ylab="Acceptance probability",
     xlab="Minimum sample size",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:N)
{
  par(new = TRUE)
  lines(testAPcost$n[testAPcost$R==Rvec[i]],testAPcost$AP[testAPcost$R==Rvec[i]],lty=1,col=cols[N+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=c(0:10))
}
points(rep(130,length(cols)),seq(0.5,1.1,length=length(cols)),pch=15,cex=3,col=sort(cols,decreasing = F))
text(rep(150,2),c(0.5,1.1),labels=paste("R=",c(0.95,0.8),sep=""),cex=2)
arrows(x0=150, y0=1.05,x1=150, y1=0.55, angle= 30,code=2,lwd=5,col=cols[10])
legend("topright",col = cols[5],pch=c(0:10)
       ,legend = c("c=0","c=1","c=2","c=3","c=4","c=5",
                   "c=6","c=7","c=8","c=9","c=10"),bty="n",cex = 2,lwd=3)
lines(Nsens$n,Nsens$AP,lwd = 3,type = "b",pch=17,cex=1.5)

arrows(x0=61, y0=0.0025605307,x1=75, y1=0.0009856712, angle= 15,code=2,lwd=3,col=1)
legend("topleft",col = 1,pch=17
       ,legend = c("Optimal plans, increasing warranty cost"),bty="n",cex = 1.7,lwd=3)

dev.off()





############################################################################
### prior distributions
pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/Betaprior-binomial.pdf",
    width=10, height=8)
par(family = 'serif')
par(mfrow = c(1,1))
par(mar = c(5, 6, 2, 2))
x <- seq(0, 1, length = 1000)
plot(x,dbeta(x,1,1),axes = FALSE,type="l",ylim=c(0,12),xlim=c(-0.15,1),
     cex.lab=2,main="",xlab=expression(pi),ylab=expression(p(pi),cex.axis=2)
     ,lty=3,lwd=3)
lines(x,dbeta(x,1,9),type="l",lty=3,lwd=3)
lines(x,dbeta(x,2,18),type="l",lty=2,lwd=3)
lines(x,dbeta(x,4,36),type="l",lty=1,lwd=3)
lines(x,dbeta(x,0.5,2),type="l",lty=3,lwd=3)
lines(x,dbeta(x,15,60),type="l",lty=1,lwd=3)
lines(x,dbeta(x,7.5,30),type="l",lty=2,lwd=3)
axis(1,cex.axis=2)
axis(2,cex.axis=2)
text(0.9,1.6, "Beta(1,1)",cex=2)
text(-0.1,9, "Beta(1,9)",cex=2)
text(0.15,9.5, "Beta(4,36)",cex=2)
text(-0.08,6.5, "Beta(2,18)",cex=2)
text(0.15,12, "Beta(0.5,2)",cex=2)
text(0.38,8.5, "Beta(15,60)",cex=2)
text(0.38,6.5, "Beta(7.5,30)",cex=2)

legend("topright",col = c(1,1,1),lty=c(1,2,3)
       ,legend = c("Low variance","Medium variance"
                   ,"High variance")
       ,bty="n",cex = 2,lwd=3)
dev.off()



###compare informative priors
pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/comp_prior_mean.pdf",
    width=10, height=8)
par(family = 'serif')
par(mfrow = c(1,1))
par(mar = c(5, 6, 2, 2))
Cf=0;Cv=125;Cw=5;N=150000;G=100000

#more supportive prior
temp <- data_info0218
temp <- Addcosts(as.data.frame(temp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]
testAPcost <- temp
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)
cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min-5000,max+50000),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[length(cvec)+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}

points(temp[which.min(temp$exp_cost),]$R,temp[which.min(temp$exp_cost),]$exp_cost,
       col=3,pch=17,cex=1.5)
abline(h=temp[which.min(temp$exp_cost),]$exp_cost,col=3,lty=2,lwd=3)

#real
temp <- data_real_CR05
temp <- Addcosts(as.data.frame(temp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]
testAPcost <- temp
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[length(cvec)+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}

points(temp[which.min(temp$exp_cost),]$R,temp[which.min(temp$exp_cost),]$exp_cost,
       col=2,pch=19,cex=1.5)
abline(h=temp[which.min(temp$exp_cost),]$exp_cost,col=2,lty=1,lwd=3)




#less supportive prior
temp <- data_info7530
temp <- Addcosts(as.data.frame(temp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]
testAPcost <- temp
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[length(cvec)+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}
points(temp[which.min(temp$exp_cost),]$R,temp[which.min(temp$exp_cost),]$exp_cost,
       col=4,pch=1,cex=1.5)
abline(h=temp[which.min(temp$exp_cost),]$exp_cost,col=4,lty=3,lwd=3)



points(rep(0.96,length(cols)),seq(min+50000,max+50000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+50000,max+50000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))

legend("topleft",col = c(2,3,4),pch = c(19,17,1),lty=c(1,2,3)
       ,legend = c("Informative prior from real example",
                   "Informative and more supportive prior",
                   "Informative and less supportive prior")
       ,bty="n",cex = 2,lwd=3)
dev.off()




###compare informative vs non-informative priors
pdf("G:/My Drive/research topics/2017 SRDT & warranty/QE target/manuscript/figures/comp_prior_info.pdf",
    width=10, height=8)
par(family = 'serif')
par(mfrow = c(1,1))
par(mar = c(5, 6, 2, 2))
Cf=0;Cv=125;Cw=5;N=150000;G=100000

#more supportive prior
temp <- data_info0218
temp <- Addcosts(as.data.frame(temp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]
testAPcost <- temp
max = round_any(max(testAPcost$exp_cost),10000,ceiling)
min = round_any(min(testAPcost$exp_cost),10000,floor)
cols=gray.colors(length(cvec), start = 0, end = 0.9)
plot(c(0.8,0.98),c(min-5000,max+50000),ylab="Overall cost",
     xlab="Reliability requirement",main="",type="n",
     cex.lab=2, cex.axis = 2)
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[length(cvec)+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}

points(temp[which.min(temp$exp_cost),]$R,temp[which.min(temp$exp_cost),]$exp_cost,
       col=3,pch=17,cex=1.5)
abline(h=temp[which.min(temp$exp_cost),]$exp_cost,col=3,lty=2,lwd=3)

#real
temp <- data_real_CR05
temp <- Addcosts(as.data.frame(temp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]
testAPcost <- temp
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[length(cvec)+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}

points(temp[which.min(temp$exp_cost),]$R,temp[which.min(temp$exp_cost),]$exp_cost,
       col=2,pch=19,cex=1.5)
abline(h=temp[which.min(temp$exp_cost),]$exp_cost,col=2,lty=1,lwd=3)




#less supportive prior
temp <- data_info0101
temp <- Addcosts(as.data.frame(temp),Cf=Cf,Cv=Cv,Cw=Cw,N=N,G=G)
temp <- findoptimal(temp[temp$CR<=0.05,])
temp[which.min(temp$exp_cost),]
testAPcost <- temp
for(i in 1:length(cvec))
{
  par(new = TRUE)
  lines(testAPcost$R[testAPcost$c==cvec[i]],testAPcost$exp_cost[testAPcost$c==cvec[i]],lty=1,col=cols[length(cvec)+1-i]
        ,xlab = "",ylab = "",type = "b",lwd=2,pch=19)
}
points(temp[which.min(temp$exp_cost),]$R,temp[which.min(temp$exp_cost),]$exp_cost,
       col=4,pch=1,cex=1.5)
abline(h=temp[which.min(temp$exp_cost),]$exp_cost,col=4,lty=3,lwd=3)



points(rep(0.96,length(cols)),seq(min+50000,max+50000,length=length(cols)),
       pch=15,cex=3,col=sort(cols,decreasing = T))
text(rep(0.975,2),seq(min+50000,max+50000,length=length(cols)),labels=paste("c=",cvec,sep=""),
     cex=2,col=c(rep(1,length(cvec))))

legend("topleft",col = c(2,3,4),pch = c(19,17,1),lty=c(1,2,3)
       ,legend = c("Informative prior from real example",
                   "Informative prior",
                   "Non-informative prior")
       ,bty="n",cex = 2,lwd=3)
dev.off()








