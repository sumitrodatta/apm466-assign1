install.packages("jrvfinance")
library("jrvFinance")
#import two separate sheets, neither of which is the full raw data of all bonds
#this data only contains clean prices and only the 11 bonds selected (not all 32), used to calculate YTM
bondsClean=read.csv("APM466 Bonds Final (Clean Prices).csv", header=TRUE)
bondsClean=as.data.frame(bondsClean)
#this data only contains dirty prices (converted from clean in raw data) and only the 11 bonds selected (not all 32)
#the dirty price data is used to calculate spot and forward rates
bondsDirty=read.csv("APM466 Bonds Final (Dirty Prices).csv", header=TRUE)
bondsDirty=as.data.frame(bondsDirty)

s=rep(1,11)
t=spotscolumnnames=yieldscolumnnames=fwdscolumnnames=rep(1,10)
v=rep(1,4)
RawSpots = RawYields = data.frame(s,s,s,s,s,s,s,s,s,s)

FinalYields = FinalSpots = data.frame(t,t,t,t,t,t,t,t,t,t)
Forwards = data.frame(v,v,v,v,v,v,v,v,v,v)

Dates = c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08","2020-01-09","2020-01-10","2020-01-13",
          "2020-01-14","2020-01-15")
####YTM####

#yields to maturity using jrvfinance
for (i in c(1:11)) 
{
  for (j in c(1:10))
  {
    #bond yield vars: (settle date, maturity, coupon rate, coupon freq, price, daycount,compounding freq, redemp value)
    RawYields[i,j]=bond.yield(Dates[j],bondsClean$Maturity.Date[i],bondsClean$Coupon[i],freq=2,
                           bondsClean[i,j+9],"ACT/ACT",comp.freq=2,redemption_value = 100)
  }
}
View(RawYields)

####Spots####

#calculate the semi-ann spot rate within the first six months:
#2*[(price/(0.5*coupon + face)^(-1/2*TTM))-1]
for (i in c(1:10))
{
  price=bondsDirty[1,9+i]
  coupon=bondsDirty[1,3]*100/2 # mult by 100 for the notional value of the coupon,  div by 2 for semi-annual
  face=100
  maturityInMonths=bondsDirty[1,5]/12
  RawSpots[1,i]=2*((price/(coupon+face))^(-1/(2*maturityInMonths))-1) #this gives semi-annual
}

#calculate spot rates for rest of time periods
for (i in c(2:11))
{
  for (j in c(1:10))
  {
    price=bondsDirty[i,9+j]
    coupon=bondsDirty$Coupon[i]*100/2 # mult by 100 for the notional value of the coupon,  div by 2 for semi-annual
    face=100
    coupons_PV=0 #initialize the present value of coupons
    maturityInMonths=bondsDirty$Months.until.Maturity[i]/12
    coupon_times = seq((6-bondsDirty$Months.since.Last.Coupon[i])/12,(bondsDirty$Months.until.Maturity[i]-1)/12,1/2)
    for (k in c(1:length(coupon_times)))
    {
      coupons_PV=coupons_PV+coupon*(1+RawSpots[k,j]/2)^(-2*coupon_times[k])
    }
    newprice=price-coupons_PV
    coupons_PV=0 #reset coupon present value
    RawSpots[i,j]=2*((newprice/(coupon+face))^(-1/(2*maturityInMonths))-1)
  }
}
View(RawSpots)

####Naming Columns + Rows####

#rename columns for spots to Spots for [Date]"
#rename columns for spots to Yields for [Date]"
#rename columns for forwards to Forwards for [Date]"
for (a in c(1:length(Dates)))
{
  spotscolumnnames[a] = c(paste("Spots for",Dates[a]))
  yieldscolumnnames[a] = c(paste("Yields for",Dates[a]))
  fwdscolumnnames[a] = c(paste("Forwards for",Dates[a]))
}
names(RawSpots)=names(FinalSpots)=spotscolumnnames
names(RawYields)=names(FinalYields)=yieldscolumnnames
names(Forwards)=fwdscolumnnames
bondsDirty=data.frame(bondsDirty,RawSpots)

####Interpolation for yields and spots####

#interpolate for n-month yield and n-month spot, where n is multiples of 6 up to 60
for (i in c(1:10))
{
  for (j in c(1:10))
  {
    FinalYields[j,i]=approx(bondsDirty$Months.until.Maturity,RawYields[[i]],xout=6*j)$y
    FinalSpots[j,i]=approx(bondsDirty$Months.until.Maturity,RawSpots[[i]],xout=6*j)$y
  }
}
rownames(FinalYields)=rownames(FinalSpots)=seq(6,60,6)

####Forwards####

#forward 1-yr, n-yr= 1 yr forward from n-yr spot
#for example, 1yr2yr is 1 yr forward starting at 2 yrs ->need spot for 2 and 3 yrs
for (j in c(1:4))
{
  for (i in c(1:10))
  {
    n_yr=(1+FinalSpots[2*j,i]/2)^(2*j)
    one_yr_fwd=(1+FinalSpots[2+2*j,i]/2)^(2+2*j)
    Forwards[j,i]=2*((one_yr_fwd/n_yr)^(1/2)-1)
  }
}

####covariance matrices for log-return of yields####
log_return_yields1=log_return_yields2=log_return_yields3=log_return_yields4=log_return_yields5=vector("numeric",9)

for (i in c(1:9))
{
  log_return_yields1[i]=log(FinalYields[2,i]/FinalYields[2,i+1])
  log_return_yields2[i]=log(FinalYields[4,i]/FinalYields[4,i+1])
  log_return_yields3[i]=log(FinalYields[6,i]/FinalYields[4,i+1])
  log_return_yields4[i]=log(FinalYields[8,i]/FinalYields[8,i+1])
  log_return_yields5[i]=log(FinalYields[10,i]/FinalYields[10,i+1])
}

log_returns_yields=data.frame(log_return_yields1,log_return_yields2,log_return_yields3,log_return_yields4,log_return_yields5)
covariance_of_log_returns=cov(log_returns_yields,log_returns_yields)

####covariance matrices for fwd rates
fwdrates12=fwdrates13=fwdrates14=fwdrates15=vector("numeric",9)
for(i in c(1:9))
{
  fwdrates12[i]=log(Forwards[1,i]/Forwards[1,i+1])
  fwdrates13[i]=log(Forwards[2,i]/Forwards[2,i+1])
  fwdrates14[i]=log(Forwards[3,i]/Forwards[3,i+1])
  fwdrates15[i]=log(Forwards[4,i]/Forwards[4,i+1])
}

fwdrates=data.frame(fwdrates12,fwdrates13,fwdrates14,fwdrates15)
covariance_of_fwdrates=cov(fwdrates,fwdrates)

####eigenitems of the above covariance matrices####
eigenitems_yield=eigen(covariance_of_log_returns,symmetric=TRUE)
eigenitems_fwd=eigen(covariance_of_fwdrates,symmetric=TRUE)

####Yield Plot####

pdf(file=paste(getwd(),"/YTM_plot.pdf",sep=""),width=4,height=3.5)
plot(seq(6,60,6),FinalYields$`Yields for 2020-01-02`,type="l",ylim=c(0.015,0.022), col="blue",
     xlab="Months from January 2020",ylab="Yield in Decimal (semi-ann compounded)", main ="All Yield Curves")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-03`,col="green")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-06`,col="red")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-07`,col="blueviolet")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-08`,col="violet")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-09`,col="yellowgreen")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-10`,col="sienna")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-13`,col="powderblue")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-14`,col="gold")
lines(seq(6,60,6),FinalYields$`Yields for 2020-01-15`,col="orange")
legend("topright",Dates,lty=c(1,1), lwd=c(2,2),cex=.5, bty = "n", 
       col=c("blue","green","red","blueviolet","violet","yellowgreen","sienna","powderblue","gold","orange"))
dev.off()

####Spot Plot####
pdf(file=paste(getwd(),"/Spot_plot.pdf",sep=""),width=4.5,height=4)
plot(seq(6,60,6),FinalSpots$`Spots for 2020-01-02`,type="l", ylim=c(0.013,0.025), col="blue",
     xlab="Months from January 2020",ylab="Spot in Decimal (semi-ann compounded)", main ="All Spot Curves")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-03`,col="green")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-06`,col="red")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-07`,col="blueviolet")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-08`,col="violet")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-09`,col="yellowgreen")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-10`,col="sienna")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-13`,col="powderblue")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-14`,col="gold")
lines(seq(6,60,6),FinalSpots$`Spots for 2020-01-15`,col="orange")
legend("topright",Dates,lty=c(1,1), lwd=c(2,2),cex=.5, bty = "n", 
       col=c("blue","green","red","blueviolet","violet","yellowgreen","sienna","powderblue","gold","orange"))
dev.off()

####Forwards Plot####
pdf(file=paste(getwd(),"/Fwd_plot.pdf",sep=""),width=4.5,height=4)
plot(seq(1,4),Forwards$`Forwards for 2020-01-02`,type="l",ylim=c(0.013,0.022), col="blue",
     xlab="Years from 2020",ylab="Forward in Decimal (semi-ann compounded)", main ="All 1-Yr, N-Yr Forward Curves")
lines(seq(1,4),Forwards$`Forwards for 2020-01-03`,col="green")
lines(seq(1,4),Forwards$`Forwards for 2020-01-06`,col="red")
lines(seq(1,4),Forwards$`Forwards for 2020-01-07`,col="blueviolet")
lines(seq(1,4),Forwards$`Forwards for 2020-01-08`,col="violet")
lines(seq(1,4),Forwards$`Forwards for 2020-01-09`,col="yellowgreen")
lines(seq(1,4),Forwards$`Forwards for 2020-01-10`,col="sienna")
lines(seq(1,4),Forwards$`Forwards for 2020-01-13`,col="powderblue")
lines(seq(1,4),Forwards$`Forwards for 2020-01-14`,col="gold")
lines(seq(1,4),Forwards$`Forwards for 2020-01-15`,col="orange")
legend("topleft",Dates,lty=c(1,1), lwd=c(2,2),cex=.5, bty = "n", 
       col=c("blue","green","red","blueviolet","violet","yellowgreen","sienna","powderblue","gold","orange"))
dev.off()