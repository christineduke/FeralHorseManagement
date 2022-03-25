# We will be excluding foals from the analysis as there is only 1 observation, and as such is not significant in any stats through all equine zones and habitats

setwd()
library(rstan);library(blmeco)
library(tidyverse)

dat1 <- read.csv("FeralHorsesAbbrev.csv")
View(dat1)
head(dat1);str(dat1)
attach(dat1)

# Look for significant differences between Equine Zones and occupancy rates

anova1 <- aov(total~habitat)
anova1
TukeyHSD(anova1)
anova2 <- aov(total~equine_zone)
anova2
TukeyHSD(anova2)

sadult_Pop <- aov(s_adult ~ habitat)
sadult_Pop
TukeyHSD(sadult_Pop)

sadult_Pop2 <- aov(s_adult ~ equine_zone)
sadult_Pop2
TukeyHSD(sadult_Pop2)

adult_Pop <- aov(adult ~ habitat)
adult_Pop
TukeyHSD(adult_Pop)

adult_Pop2 <- aov(adult ~ equine_zone)
adult_Pop2
TukeyHSD(adult_Pop2)

# Export Results

summary(anova1)
T_anova1 <- TukeyHSD(anova1)
TK <-(T_anova1)
TK_data<-as.data.frame(TK[1])
write.csv(TK_data, 'TotalCount_Habitat.csv')

summary(anova2)
T_anova2 <- TukeyHSD(anova2)
TK2 <- (T_anova2)
TK_data2 <- as.data.frame(TK2[1])
write.csv(TK_data2, 'TotalCount_Zone.csv')

summary(sadult_Pop)
T_anova3 <- TukeyHSD(sadult_Pop)
TK3 <- (T_anova3)
TK_data3 <- as.data.frame(TK3[1])
write.csv(TK_data3, 'Subadult_Habitat.csv')

summary(sadult_Pop2)
T_anova4 <- TukeyHSD(sadult_Pop2)
TK4 <- (T_anova4)
TK_data4 <- as.data.frame(TK4[1])
write.csv(TK_data4, 'Subadult_Zone.csv')

summary(adult_Pop)
T_anova5 <- TukeyHSD(adult_Pop)
TK5 <- (T_anova5)
TK_data5 <- as.data.frame(TK5[1])
write.csv(TK_data5, 'Adult_Habitat.csv')

summary(adult_Pop2)
T_anova6 <- TukeyHSD(adult_Pop2)
TK6 <- (T_anova6)
TK_data6 <- as.data.frame(TK6[1])
write.csv(TK_data6, 'Adult_Zone.csv')

# Look at some differences

# Habitat - Riparian - Cutblock (0.8)
#           Riparian - Industrial (0.4)
#           Riparian - Grassland (0.6)
#           Riparian seems to be the most different, not too shocking

# Equine Zones - Ghost Equine Zone - Elbow Equine Zone (0.8)
#                Sundre Equine Zone - Elbow Equine Zone (0.8)
#                Ghost - Elbow - Sundre Zones exhibit the greatest differences (not super 
#                significant but HEY it's a difference)

# Create vectors for figures

# By habitats within zones

CutblockSundre <- c(3,6,4,6,3,9,10,2,13,9,9,3,3,2,4,4,3,10,9,4,3,7,
                    10,15,5,3,6,3,2,7,4,7,6,3,5,2,3,3,4,6,7,6,6,2,
                    3,5,5,4,8,9,6,1,14,2,8,6,5,21,3,2,4,8,9,5,2,2,
                    7,3,2,1,3,4,2,1,5,1,4,5,5,2,3,8,5)
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

# By overall habitat

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


# Generate basic visualizations

par(mfrow=c(3, 3))
hist(Ghost, col="grey", labels=T,
     main = "Ghost River Equine Zone", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Elbow, col="grey", labels=T,
     main = "Elbow River Equine Zone", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Sundre, col="grey", labels=T,
     main = "Sundre Equine Zone", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Clearwater,col="grey", labels=T,
     main = "Clearwater Equine Zone", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Nordegg,col="grey", labels=T,
     main = "Nordegg Equine Zone", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Brazeau,col="grey", labels=T,
     main = "Brazeau Equine Zone", 
     xlab = "Observation Range", ylab = "Number of Individuals")

par(mfrow=c(3, 3))
hist(Cutblock, col="grey", labels=T,
     main = "Cutblock Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Conifer, col="grey", labels=T,
     main = "Conifer Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Deciduous, col="grey", labels=T,
     main = "Deciduous Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Mixedwood,col="grey", labels=T,
     main = "Mixedwood Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Grassland,col="grey", labels=T, 
     main = "Grassland Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Riparian,col="grey", labels=T, 
     main = "Riparian Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")

par(mfrow=c(2,2))
hist(Shrub, col="grey", labels=T, 
     main = "Shrub Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Industrial,col="grey", labels=T, 
     main = "Industrial Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(Roadside,col="grey", labels=T, 
     main = "Roadside Habitat", 
     xlab = "Observation Range", ylab = "Number of Individuals")
hist(total, col="grey", labels=T,
     main = "Total Counts of Horses in all Equine Zones during the Survey",
     xlab = "Observation Range", ylab = "Number of Individuals")


# Most adults and sub-adults occur in Riparian and Cutblock habitats in Sundre

# Sundre is the zone with the most occurrences of individuals within the survey, with mostsub-adult detections occurring within the Cutblock habitat type, 
# followed by adults in Riparian


# Basic look at population growth rates

dat2 <- read.csv("FeralHorsePop.csv")
View(dat2)
attach(dat2)

growth_rate = dat2 %>%
        # first sort by year
        arrange(Year) %>%
        mutate(Diff_Year = Year - lag(Year),  # Difference in time (just in case there are gaps)
               Diff_growth = Minimum.Horse.Count - lag(Minimum.Horse.Count), # Difference in route between years
               Rate_percent = (Diff_growth / Diff_Year)/Minimum.Horse.Count * 100) # growth rate in percent
growth_rate
write.csv(growth_rate, 'GrowthRate_Overall.csv')

dat3 <- read.csv("GrowthRate_Overall.csv")

library(dplyr)
library(reshape2)
library(ggplot2)
library(growthcurver)
library(purrr)

ggplot(dat3, aes(x = Year, y = Minimum.Horse.Count)) + geom_point(alpha=0.7)

model <- SummarizeGrowth(dat3$Year, dat3$Minimum.Horse.Count)
model$vals

par(mar=c(5,4,4,2))
plot(model,col="grey",
     xlab="Year", ylab="Minimum  Horse  Count (# of individuals)")


# Summary of counts

dat1 %>%
        group_by(equine_zone) %>%
        summarize(total = sum(total))

dat1 %>%
        group_by(habitat) %>%
        summarize(total = sum(total))

dat1 %>%
        group_by(habitat) %>%
        summarize(total = sum(adult))
dat1 %>%
        group_by(habitat) %>%
        summarize(total = sum(s_adult))
dat1 %>%
        group_by(habitat) %>%
        summarize(total = sum(foal))
