setwd("C:/Users/chris/OneDrive/Documents/School/Current Courses/2. Winter 2022/REN R 476/Term Project")
library(rstan)
library(blmeco)
library(tidyverse)

dat1 <- read.csv("FeralHorse1.csv")
dat3 <- read.csv("FeralHorsePop.csv")

View(dat1)

# Analysis for FeralHorse1

head(dat1);str(dat1)
attach(dat1)

sink("dat1")
cat("
    dat1 {
    int<lower=0> N; //Number of Sites
    int<lower=1> J; //Number of Replicates at each site
    int<lower=0, upper=21> y[N, J]; //Detection at Each site on each sampling rep
    int<lower=0, upper=21> x[N]; //Observed occupancy at each site
    real DataDate[N, J];
    }
    
    parameters {
    real a0; //specifying regression parameters
    real b0;
    real b1;
    real b2
    }
    
    transformed parameters {
    real<lower=0,upper=21> psi[N];
    real<lower=0,upper=1> p[N, J];
    for(i in 21:N) {
    psi[i] = inv_logit(a0); //intercept-only model for occupancy
    for(j in 21:J) {
    p[i, j] = inv_logit(b0 + b1*DataDate[i, j] + b2*DataDate[i, j]*DataDate[i, j]; //Detection probability on inverse logit
    }
    }
    }
    
    model {
    // Priors
    a0 ~ normal(0,5);
    b0 ~ normal(0,5);
    b1 ~ normal(0,5);
    b2 ~ normal(0,5);
    
    // likelihood
    for(i in 1:N) {
    if(x[i]==1) {
    1 ~ bernoulli(psi[i]);
    y[i] ~ bernoulli(p[i];
    }
    if(x[i]==0) {
    increment_log_prob(log_sum_exp(log(psi[i]) + loglm(p[i,1]) + loglm(p[i,2]) + loglm(p[i,3]) + loglm(p[i,4]) + loglm(p[i,5]) + loglm(p[i,6]), loglm(psi[i]))); //?
    }
    }
    }
    
    ",fill = TRUE)
sink()

plot(dat1, ylim=c(0,21), xlim=c(1, 102))
traceplot(dat1, "a0")

# Analysis for dat2

dat2 <- read.csv("FeralHorse2.csv")
head(dat2);str(dat2)
attach(dat2)
View(dat2)

# Let's use the abbreviated dataset now, see what we can see stats wise!

dat4 <- read.csv("FeralHorsesAbbrev.csv")
View(dat4)
head(dat4);str(dat4)
attach(dat4)

# Letz make V E C T O R S 4 tha population boyo BUT FIRST. we subset

CutblockSundre <- c(3,6,4,6,3,9,10,2,13,9,9,3,3,2,4,4,3,10,9,4,3,7,10,15,5,3,6,3,2,7,4,7,6,3,5,2,3,3,4,6,7,6,6,2,3,5,5,4,8,9,6,1,14,2,8,6,5,21,3,2,4,8,9,5,2,2,7,3,2,1,3,4,2,1,5,1,4,5,5,2,3,8,5)
CutblockGhost <- c(13,13,3,10,7,8,3,8,5,4,5,4,4,6,11,7,6,7,6,10,6,4)
CutblockElbow <- c(4,4,6,9,6,3,4)
CutblockNordegg <- c(10,3,9,7,11)

ConiferGhost <- c(2,7,2,3,1,10)
ConiferSundre <- c(6,4,11,6)
ConiferClearwater <- c(6)

DeciduousGhost <- c(6,4,3)
DeciduousSundre <- c(10,6,7,8,6,6,3,4,9,7)
DeciduousElbow <- c(6)

MixedwoodSundre <- c(12,3,5,3,2,5,9,7,9,8,3,4)
MixedwoodGhost <- c(3,2,7)
MixedwoodElbow <- c(8)
MixedwoodClearwater <- c(7)

IndustrialGhost <- c(1,7)
IndustrialSundre <- c(8,6,3,6,4,5,3,3,1,4,12,4,6,3,5,3)
IndustrialClearwater <- c(12)
IndustrialBrazeau <- c(7,3,6)
IndustrialNordegg <- c(8,4,2,4,1)

GrasslandSundre <- c(3,2,1,5,10,4,6,8,4,5,6,3,4)
GrasslandGhost <- c(2,6,3,6,4,4,3,2,3,6,6,3,6,6,11,7,4,13)
GrasslandClearwater <- c(5)
GrasslandNordegg <- c(2,7)

RiparianGhost <- c(4,5,1,4,6)
RiparianElbow <- c(18,7,3,10)
RiparianSundre <- c(16,5,1,7,17,9,6,3,9,11,5,2,8,11,1,7,7,3,9,5,5,2,7)
RiparianClearwater <- c(4,14,4,4,8,5)
RiparianNordegg <- c(2,6,6,1,9,8,11)

ShrubGhost <- c(4,13,7,7,8,3,4,1,9)
ShrubSundre <- c(5,5)
ShrubClearwater <- c(6,7,3,9,1,5,12,5,9)
ShrubNordegg <- c(3)

RoadsideSundre <- c(6,8)
RoadsideClearwater <- c(5,2,9)

# By habitat

Cutblock <- c(CutblockElbow,CutblockGhost,
              CutblockNordegg,CutblockSundre)
Conifer <- c(ConiferClearwater,ConiferGhost,
             ConiferSundre)
Deciduous <- c(DeciduousElbow,DeciduousGhost,
               DeciduousSundre)
Mixedwood <- c(MixedwoodClearwater,MixedwoodElbow,
               MixedwoodGhost,MixedwoodSundre)
Industrial <- c(IndustrialBrazeau,IndustrialClearwater,
                IndustrialGhost,IndustrialNordegg,
                IndustrialSundre)
Grassland <- c(GrasslandClearwater,GrasslandGhost,
               GrasslandSundre,GrasslandNordegg)
Riparian <- c(RiparianClearwater,RiparianElbow,
              RiparianGhost,RiparianNordegg,
              RiparianSundre)
Shrub <- c(ShrubClearwater,ShrubNordegg,
           ShrubSundre,ShrubGhost)
Roadside <- c(RoadsideSundre,RoadsideClearwater)

# By Equine Zone

Ghost <- c(CutblockGhost,ConiferGhost,DeciduousGhost,MixedwoodGhost,
           IndustrialGhost,GrasslandGhost,RiparianGhost,ShrubGhost)
Elbow <- c(CutblockElbow,DeciduousElbow,MixedwoodElbow,RiparianElbow)
Sundre <- c(CutblockSundre,ConiferSundre,DeciduousSundre,MixedwoodSundre,
            IndustrialSundre,GrasslandSundre,RiparianSundre,ShrubSundre,
            RoadsideSundre)
Clearwater <- c(ConiferClearwater,MixedwoodClearwater,IndustrialClearwater,
                GrasslandClearwater,RiparianClearwater,ShrubClearwater,
                RoadsideClearwater)
Nordegg <- c(CutblockNordegg,IndustrialNordegg,GrasslandNordegg,
             RiparianNordegg,ShrubNordegg)
Brazeau <- c(IndustrialBrazeau)

# Figure time BABEE

par(mfrow=c(3, 3))
hist(Ghost, col="grey", labels=T, breaks=7, 
     xlab = "Observations", ylab = "Number of Individuals")
hist(Elbow, col="grey", labels=T, breaks=7, 
     xlab = "Observations", ylab = "Number of Individuals")
hist(Sundre, col="grey", labels=T, breaks=7, 
     xlab = "Observations", ylab = "Number of Individuals")
hist(Clearwater,col="grey", labels=T, breaks=7, 
     xlab = "Observations", ylab = "Number of Individuals")
hist(Nordegg,col="grey", labels=T, breaks=7, 
     xlab = "Observations", ylab = "Number of Individuals")
hist(Brazeau,col="grey", labels=T, breaks=7, 
     xlab = "Observations", ylab = "Number of Individuals")
