#--------------------------#
#      Simulation Ex.      #
#         11/23/2015       #
#                          #
#--------------------------#

library(graphics)
library(ks)
library(BayesBridge)
library(Rlab)
library(triangle)

rm(list=ls())
#Set the number of simulations you want to run
simNum <- 10000
VaR.Perc <- 0.05
numYears <- 15
Severance.Tax <- .06
WACC.Factor <- .1

#-----------------------------------------------------------------------#
#                            PRODUCTION RISK                            #
#-----------------------------------------------------------------------#
#INPUT Wells you are attempting UNIFORM DISCRETE DISTRUBTION
Num.Wells.Attempt <- sample(10:30,simNum,replace=T)
Num.Wells.Actual <- rep(0,simNum)
for(i in 1:simNum){
  Wells.This.Sim <- 0
  for(j in 1:Num.Wells.Attempt[i]){
    #INPUT Probability of Hydrocarbons TRUNCATED NORMAL
    P.Hydrocarbons <- rtnorm(num=1, mu=0.99, sig=0.05, left=0, right=1)
    #INPUT Probability of Reservoir TRUNCATED NORMAL
    P.Reservoir <- rtnorm(num=1, mu=0.75, sig=0.10, left=0, right=1)
    P.Well <- P.Hydrocarbons*P.Reservoir
    Wells.This.Sim = Wells.This.Sim + rbern(n=1, prob=P.Well)
  }
  Num.Wells.Actual[i] <- Wells.This.Sim
}

Perc.Wells.Producing <- (Num.Wells.Actual / Num.Wells.Attempt)

#Calculate CVaR
sorted.wells <- sort(Perc.Wells.Producing)
VaR.index <- round(VaR.Perc*simNum)
CVaR <- mean(sorted.wells[1:VaR.index])

#Calculate VaR
Var1.index <- round(VaR.Perc*simNum)+1
Var1 <- sorted.wells[Var1.index]

#Histogram of the percent wells producing of simluations
hist(Perc.Wells.Producing, breaks=20, 
     main=paste("Percent Wells Producing for ",simNum," Simulations",sep=""),
     xlab="Percent Producing", col="lightblue")
abline(v = VaR, col="red", lwd=2)
mtext("VaR", at=Var1, col="red")

#Create and draw histrogram of the truncated normal probability of finding hydrocarbons 
tP.Hydrocarbons <- rtnorm(num=10000, mu=0.99, sig=0.05, left=0, right=1)
hist(tP.Hydrocarbons, breaks=20, 
     main=paste("Percent wells Hydrocarbons 
                Truncated Normal Distribution"),
     xlab="% with Hydrocarbons", col="lightblue")

#Create and draw histrogram of the truncated normal probability of finding resevoirs 
tP.Reservoir <- rtnorm(num=10000, mu=0.75, sig=0.10, left=0, right=1)
hist(tP.Reservoir, breaks=20, 
     main=paste("Percent wells with Resevoirs
                Truncated Normal Distribution"),
     xlab="% with Resevoirs", col="lightblue")

#Check above distribution against a normal distrubtion if you like
nP.Hydrocarbons <- rnorm(n=10000, mean=0.99, sd=0.05)
hist(nP.Hydrocarbons, breaks=20, 
     main=paste("% Hydrocarbon, Normal Sampling"),
     xlab="Percent Hydrocarbons", col="lightblue")



#You have a 5% chance of having only 53%  or less of your drilling attempts yield a wet well
#Of the worst 5% of scenarios, you're average successfull wet well yield is 47%


#-----------------------------------------------------------------------#
#                           OTHER INITIAL COSTS                         #
#-----------------------------------------------------------------------#

#Seismic and Lease Costs
price.per.acre <- 960
Lease.Acres <- rnorm(n=simNum, mean=12000, sd=1000)
Acre.Cost <- price.per.acre*Lease.Acres   #!!!!!!!!!!!  Year 0 expense

price.per.section <- 43000
Seismic.Sections <- rnorm(n=simNum, mean=50, sd=7)
Seismic.Cost <- price.per.section*Seismic.Sections   #!!!!!!!!!!!  Year 0 expense


#Completion costs (1 time cost for number of succesful wells)
Price.per.successfull.well <- rnorm(n=simNum, mean=390000, sd=50000)
Completion.Costs <- Num.Wells.Actual*Price.per.successfull.well   #!!!!!!!!!!!!  Year 0 expense
mean(Completion.Costs)


Completion.Costs.million <- Completion.Costs/1000000
hist(Completion.Costs.million, breaks=20, 
     main=paste("Distribution of Well Completetion Costs"),
     xlab="Completion Costs per Simulation in Millions of Dollars", col="lightblue")



#Drilling costs  
Drilling.Cost.Return.Samples <- c(0.267539211,	0.23917239,	0.324538618,	-0.277263988,	0.205942189,	0.191287778,	0.161277788,	0.32927115,	0.286641165,	0.153317173,	0.167877921,	0.112799154,	-0.021314314,	-0.053378581,	0.169267149,	0.101124003,	0.108882886,	0.439479171,	-0.136365247,	0.256863463,	0.192131568,	0.395038764,	0.136468403,	-0.036610049,	0.409980347,	0.032184453,	0.210321167,	-0.043446374,	0.190773742,	0.107547933)
#Used to get bandwidth of kde
#Density.Drilling.Cost.Return <- density(Drilling.Cost.Return.Samples)
#Density.Drilling.Cost.Return

Est.Drilling.Cost.Return <- rkde(fhat=kde(Drilling.Cost.Return.Samples, h=0.05093), n=1000000)
#r <- hist(Est.Drilling.Cost.Return, breaks=100, main='Distribution and Kernal for Drilling Prices', xlab='Per Well Drilling Price')
Initial.Year <- 2006
Final.Year <- 2015
Ave.Drill.Cost <- array(rep(NA),c((Final.Year-Initial.Year+1),simNum))
Ave.Drill.Cost[1,] <- array(rep(2279800),simNum)
for(j in 1:simNum){
  for(i in (Initial.Year-Initial.Year+1):(Final.Year-Initial.Year)){
    Ave.Drill.Cost[i+1,j] <- Ave.Drill.Cost[i,j] * (1+sample(Est.Drilling.Cost.Return,size=1,replace=TRUE))
  }
}
Drilling.Onetime.Sum <- array(rep(NA),simNum)
Drilling.Onetime.Sum <- Ave.Drill.Cost[10,] * Num.Wells.Attempt

Drilling.Onetime.Sum.1000s <- Drilling.Onetime.Sum/1000
hist(Drilling.Onetime.Sum.1000s, breaks=20, 
     main=paste("Distribution of Drilling Costs per Well"),
     xlab="Drilling Costs in 1000s of Dollars", col="lightblue")
mean(Drilling.Onetime.Sum)

#Professional Overhead, Initial Cost
PO.Mean <- 430000
PO.Onetime.Sum <- array(rep(NA),simNum)
for(sim in 1:simNum){
  PO.Onetime <- array(rep(NA),c(Num.Wells.Attempt[sim],simNum))
  for(wells in 1:Num.Wells.Attempt[sim]){
    PO.Onetime[wells,sim] <- rtriangle(n=1, a=.4*PO.Mean, b=.65*PO.Mean, c=.5*PO.Mean)
  }
  PO.Onetime.Sum[sim] <- sum(PO.Onetime[,sim])
}

PO.Onetime.Sum.million <- PO.Onetime.Sum/1000000
hist(PO.Onetime.Sum.million, breaks=20, 
     main=paste("PO"),
     xlab="Drilling Costs in 1000s of Dollars", col="lightblue")
mean(PO.Onetime.Sum)

#Professional Overhead, Continuing 
PO.Per.Year <- array(rep(NA),c(numYears,simNum))
for(sim in 1:simNum){
  PO.Per.Well <- array(rep(NA),c(numYears,Num.Wells.Actual[sim]))
  
  for(year in 1:numYears){
    for(well in 1:Num.Wells.Actual[sim]){
      PO.Per.Well[year,well] <- rtriangle(n=1, a=.4*PO.Mean, b=.65*PO.Mean, c=.5*PO.Mean)
    }
  }
  
  for(year in 1:numYears){
    PO.Per.Year[year,sim] <- sum(PO.Per.Well[year,])
  }
}

mean(PO.Per.Year)


#Calculate total Initial Costs here
Total.Initial.Cost = Acre.Cost + Seismic.Cost + Completion.Costs + Drilling.Onetime.Sum + PO.Onetime.Sum


#Statistics for Total Initial Cost
TIC.millions <- Total.Initial.Cost/1000000
High <- round((1-VaR.Perc)*simNum)
sorted.TIC.millions <- sort(TIC.millions)
High.index <- round((1-VaR.Perc)*simNum)
#Not super correct to call these VaRs and CVaRs, and VaR would be index -1
VaR.TIC <- sorted.TIC.millions[High.index]
CVaR.TIC <- mean(sorted.TIC.millions[High.index:simNum])

VaR.TIC
CVaR.TIC

hist(TIC.millions, breaks=20, 
     main=paste("Total Initial Cost over  ",simNum," Simulations",sep=""),
     xlab="Cost in Millions of Dollars", col="lightblue")
abline(v = VaR.TIC, col="red", lwd=2)
mtext("Cost at 95% highest simulation", at=VaR.TIC, col="red")



#-----------------------------------------------------------------------#
#                            PRICE RISK                                 #
#-----------------------------------------------------------------------#

#Initial.Production.Rate <- rlnorm(n=simNum, meanlog=log(400), sdlog=log(120))
#Rate.Of.Decline <- runif(n=simNum, min=.15, max=.32)
m = 400
v = 120^2
mu = log((m^2)/sqrt(v+m^2))
sigma = sqrt(log(v/(m^2)+1))
Total.Oil.Sim = array(rep(NA),c(numYears,simNum))
for(sim in 1:simNum){
  #each well needs to be simulated independantly
    # START CORRELATION CODE
    R <- matrix(data=cbind(1,0.64, 0.64, 1), nrow=2)
    U <- t(chol(R))
    
    S.r <- rlnorm(n=Num.Wells.Actual[simNum], meanlog=mu, sdlog=sigma)
    B.r <- runif(n=Num.Wells.Actual[simNum], min=.15, max=.32)
    Both.r <- cbind(B.r, S.r)
    SB.r <- U %*% t(Both.r)
    SB.r <- t(SB.r)
    
    Initial.Production.Rate <- SB.r[,2]
    Rate.Of.Decline <- SB.r[,1]
    ### END CORRELATION CODE

  
    Rate.At.Year.Index <- array(rep(0),c(numYears+1,Num.Wells.Actual[simNum]))
    Rate.At.Year.Index[1,] <- Initial.Production.Rate
    
    for(i in 2:(numYears+1)){
      Rate.At.Year.Index[i,] <- (1-Rate.Of.Decline)*Rate.At.Year.Index[i-1,]
    }
    
    Oil.Volume.At.Year.Index <- array(rep(NA),c(numYears,Num.Wells.Actual[simNum]))
    
    for(i in 1:numYears){
      Oil.Volume.At.Year.Index[i,] <- 365*((Rate.At.Year.Index[i,] + Rate.At.Year.Index[i+1,])/2)
    }
    
    Oil.Vol.Total.Per.Sim <- array(rep(NA),c(numYears))
    for(i in 1:numYears){
      Oil.Vol.Total.Per.Sim[i] <- sum(Oil.Volume.At.Year.Index[i,])
    }
    
    for(i in 1:numYears){
      Total.Oil.Sim[i,sim] <- Oil.Vol.Total.Per.Sim[i]
    }
  
}
Oil.Vol.Total.Per.Sim

#-----------------------------------------------------------------------#
#                            REVENUE RISK                               #
#-----------------------------------------------------------------------#

#Oil prices for each year
#Start year is 2015, end year is 2040
High.Oil.Price <- c(131.66,	139.12,	144.04,	146.43,	148.26,	150.28,	151.68,	153.12,	155.06,	156.87,	158.62,	161.11,	163.80,	167.32	,170.64,	173.69,	176.83,	180.16,	182.80,	185.09,	187.92,	190.91,	193.55,	196.37,	199.88,	204.24)
Low.Oil.Price <- c(73.62,	70.00,	69.00,	68.80,	68.70,	68.90,	69.20,	69.50,	69.80,	70.10,	70.40,	70.70,	71.00,	71.30,	71.60,	71.90,	72.20,	72.50,	72.80,	73.10,	73.40,	73.70,	74.00,	74.30,	74.60,	74.90)
Likely.Oil.Price <- c(97.15,	93.44,	91.84,	92.50,	94.38,	96.57,	99.05,	101.57,	104.22,	106.69,	108.99,	110.92,	113.35,	115.31,	117.34,	118.99,	121.07,	123.40,	125.63,	127.71,	129.77,	131.61,	133.75,	135.77,	138.46,	141.46)

Oil.Price.At.Year.Index <- array(rep(NA),c(numYears,simNum))

for(j in 1:simNum){
  for(i in 1:numYears){
    Oil.Price.At.Year.Index[i,j] <- rtriangle(n=1, a=Low.Oil.Price[i], b=High.Oil.Price[i], c=Likely.Oil.Price[i])
  }
}
head(Oil.Price.At.Year.Index[10:20])

#NRI's
NRI <- rnorm(n=simNum, mean=.75, sd=.02)



#-----------------------------------------------------------------------#
#                       OPERATING EXPENSES                              #
#-----------------------------------------------------------------------#


Cost.per.Barrel <- rnorm(n=simNum, mean=6.84, sd=1)



#-----------------------------------------------------------------------#
#                       FINAL CALCULATION                               #
#-----------------------------------------------------------------------#

# NPV = -InitialCosts + FNR_year1 / (1+WACC) + FNR_year2 / (1+WACC)^2 + .... + FNR_year15 / (1+WACC)^15

Denominator <- array(rep(NA),c(numYears))

for(i in 1:numYears){
  Denominator[i] <- (1 + WACC.Factor)^i
}
FNR <- array(rep(NA),c(numYears,simNum))
Tax.Factor <- array(rep(1-Severance.Tax),c(simNum))
for(i in 1:numYears){
  #need Oil.Volume.At.Year.Index to be sum of all wells
  FNR[i,] <- Total.Oil.Sim[i,]*Oil.Price.At.Year.Index[i,]*NRI*Tax.Factor - Cost.per.Barrel*Total.Oil.Sim[i,] - PO.Per.Year[i,]
}

for(j in 1:simNum){
  for(i in 1:numYears){
    FNR[i,j] <- FNR[i,j] / Denominator[i]
  }
}

NPV <- array(rep(NA),c(simNum))

for(j in 1:simNum){
  NPV[j] <- sum(FNR[,j])  - Total.Initial.Cost[j]
}

#look at NPV stats
quantile(NPV)
mean(NPV)
median(NPV)
hist(NPV)

sorted.NPV <- sort(NPV)
VaR.index <- round(VaR.Perc*simNum)
CVaR.NPV <- mean(sorted.NPV[1:VaR.index])
NPV05 <- sorted.NPV[1:VaR.index]
NPV95 <- sorted.NPV[(simNum-VaR.index):simNum]
mean(NPV95)


# Calculate Value at Risk for simulated NPV
Var1.index <- round(VaR.Perc*simNum)+1
Var1.NPV <- sorted.NPV[Var1.index]

# Histograms for Report
NPV05.millions <- NPV05/1000000
CVaR.NPV.millions <- CVaR.NPV/1000000
hist(NPV05.millions, breaks=25, 
    main=paste("Distribution of Bottom 5% NPV
CVaR = $", round(CVaR.NPV.millions), " million", sep=''),
    xlab="Millions of Dollars", col="lightblue")
abline(v= CVaR.NPV.millions, col="red", lwd=2)
mtext("CVaR", at=CVaR.NPV.millions, col="red")


NPV95.millions <- NPV95/1000000
mean(NPV95.millions)
hist(NPV95.millions, breaks=25, 
     main=paste("Distribution of Top 5% NPV
Average of top 5% = $",round(mean(NPV95.millions)), " million", sep=''),
     xlab="Millions of Dollars", col="lightblue")
abline(v = mean(NPV95.millions), col="red", lwd=2)
mtext("Average", at=mean(NPV95.millions), col="red")

NPV.millions <- NPV/1000000
hist(NPV.millions, breaks=25,
     main=paste("Distribution of NPV
   CVaR = $", round(CVaR.NPV.millions), " million  Mean = $", round(mean(NPV.millions)), " million", sep=''),
     xlab="Millions of Dollars", col="lightblue")
abline(v= CVaR.NPV.millions, col="red", lwd=2)
mtext("CVaR", at=CVaR.NPV.millions, col="red")
abline(v= mean(NPV.millions), col="red", lwd=2)
mtext("Average NPV", at= mean(NPV.millions), col="red")



