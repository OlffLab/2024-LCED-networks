# analysis of Anderson 2007 dataset
library(tidyverse)
# load the lavaan library
library(lavaan)
library(semPlot)

setwd("G:/My Drive/Teaching/Courses/Advanced Pop Comm Ecology/ACPE 2021_2022/SEM_Anderson2007")
# key variables of interest: 
# ALL_LHU - total large herbivore density
# RES_LHU - density resident herbivores 
# FIRE_FRQ - fire frequency
# PRECIP - annual rainfall
# NMS - plant species composition (NMDS ordination axis score)
# THETRI - biomass of Themada triandra, a tall grass positively responding to fire
# BIOMASS - total aboveground plant biomass
# SOIL_RN - total soil reactive nitrogen (ammonium+nitrate)
# LF_N - plant leaf nitrogen content
# LF_NA - plant leaf sodium content
Anderson2007<-read_csv("anderson2007data.csv") %>%
  mutate(SOIL_RN=SOIL_NO3+SOIL_NH4)  # total soil reactive nitrogen
names(Anderson2007)
Anderson2007 <- Anderson2007 %>% # rescale variable to similar scale
  mutate(RES_LHU=RES_LHU/500,
         LF_Na=LF_Na/2000,
         FIRE_FRQ=FIRE_FRQ/5,
         BIOMASS=BIOMASS/100)
psych::pairs.panels(Anderson2007 %>% select(RES_LHU, FIRE_FRQ,PRECIP,NMS,
                                            THETRI,BIOMASS,SOIL_RN,LF_N, LF_Na),
                    stars = T, ellipses = F)
# multiple regression approach 
multreg<-lm(LF_N~BIOMASS + RES_LHU + FIRE_FRQ + NMS, data=Anderson2007)
summary(multreg)

plantqual.model <- 'LF_N ~  BIOMASS + RES_LHU + FIRE_FRQ + NMS
                    LF_Na ~  BIOMASS + RES_LHU + FIRE_FRQ + NMS
                    BIOMASS ~ FIRE_FRQ + RES_LHU
                    NMS ~  FIRE_FRQ'
plantqual.fit <- lavaan::sem(plantqual.model, data=Anderson2007)
lavaan::varTable(plantqual.fit)

# show the model results
# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR
summary(plantqual.fit, standardized=T, fit.measures=T,rsquare=T)

semPlot::semPaths(plantqual.fit,'std',layout='tree')




