# clear everything in the R environment
rm(list = ls())

# get the libraries needed
library(cheddar)
library(tidyverse)

# load the data of the Tuesday Lake foodweb in 1984
setwd("G:/My Drive/Teaching/Courses/Linking Community and Ecosystem Dynamics/LinkingCommEcosystems2022/R data and scripts/Cheddar_Data")
TL84<-cheddar::LoadCommunity("./TL84",fn='read.csv')
head(TL84)
attributes(TL84)
#Show the nodes
nodesTL84<-data.frame(TL84$nodes)

#plot the food web 
cheddar::PlotWebByLevel(TL84,
                        y.layout='stagger', #change vertical pos
                        stagger=0.4, # how much stagger
                        pch=19, # what symbol
                        cex=3,  # point size
                        max.nodes.per.row = 25,
                        show.nodes.as="both",  # both points and labels
                        ) 
# who is species 38?
TL84$nodes[38,]

# highlight Daphnia
cheddar::PlotWebByLevel(TL84,
                        y.layout='stagger', #change vertical pos
                        stagger=0.3, # how much stagger
                        pch=19, # what symbol
                        cex=3,  # point size
                        max.nodes.per.row = 6,
                        show.nodes.as="both",# both points and labels
                        highlight.nodes='Daphnia pulex',
                        highlight.links=TrophicLinksForNodes(TL84,"Daphnia pulex"),
                        link.lwd=2) 
# show a weighted network
data(Benguela, package="cheddar")
cheddar::PlotWebByLevel(Benguela,
                        pch=16,
                        show.nodes.as="labels",
                        node.labels="node",
                        link.lwd=Benguela$trophic.links$diet.fraction/5)
# with body mass as circles
cheddar::PlotWebByLevel(Benguela,
                        pch=16,
                        cex=log(Benguela$nodes$M)*1.5, # circle size size
                        show.nodes.as="both",
                        node.labels="node",
                        link.lwd=Benguela$trophic.links$diet.fraction/5)
