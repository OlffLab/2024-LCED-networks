# clear everything in the R environment
rm(list = ls())

# get the libraries needed
library(cheddar)
library(tidyverse)

# load the data of the Tuesday Lake foodweb in 1984
TL84<-cheddar::LoadCommunity("data/Cheddar_Data/TL84",fn='read.csv')
head(TL84)
attributes(TL84)
#Inspect the nodes
nodesTL84<-data.frame(TL84$nodes)

#plot the food web 
cheddar::PlotWebByLevel(TL84,
                        show.nodes.as="both", 
                        cex=3, # symbol size
                        y.layout='stagger')
                        
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
