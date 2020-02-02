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
Spots = data.frame(s,s,s,s,s,s,s,s,s,s)

Yields = data.frame(s,s,s,s,s,s,s,s,s,s)

Dates = c("2020-01-02","2020-01-03","2020-01-06","2020-01-07","2020-01-08","2020-01-09","2020-01-10","2020-01-13",
          "2020-01-14","2020-01-15")

#yields to maturity using jrvfinance
for (i in c(1:11))
{
  for (j in c(1:10))
  {
    Yields[i,j]=bond.yield(Dates[j],bondsClean$Maturity.Date[i],bondsClean$Coupon[i],freq=2,
                           bondsClean[i,j+9],"ACT/ACT",comp.freq=2,redemption_value = 100)
  }
}


#calculate the spot rate within the first six months: ((0.5*coupon + face)/price) -1
for (i in c(1:10))
{
  price=bondsDirty[1,9+i]
  coupon=bondsDirty$Coupon[1]*100/2 # mult by 100 for the notional value of the coupon,  div by 2 for semi-annual
  face=100
  Spots[1,i]=(price/(coupon+face))^-1-1 #this gives annual, convert to semi-annual after
}

# #calculate the semi-ann spot rate within the first six months: 
# #2*[(price/(0.5*coupon + face)^(-1/2*TTM))-1]
# for (i in c(1:10))
# {
#   price=bonds[1,9+i]
#   coupon=bonds[1,3]*100/2 # mult by 100 for the notional value of the coupon,  div by 2 for semi-annual
#   face=100
#   t=bonds[1,5]/12
#   Spots[1,i]=2*((price/(coupon+face))^(-1/(2*t))-1) #this gives semi-annual
# }


