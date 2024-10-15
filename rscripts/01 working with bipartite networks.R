# clear everything in the R environment
rm(list = ls())

# get the libraries needed
library(igraph)
library(bipartite)
library(tidyverse)

# get the dataset Ollerton 2003 needed
# inspect which examples data are included in bipartite
data(package="bipartite")
data("ollerton2003",package="bipartite") # get the dataset from the matrix
class(ollerton2003)

# visualize the ollerton2003 network
bipartite::visweb(as.matrix(ollerton2003),labsize = 2)
# or show as a diagonal
bipartite::visweb(as.matrix(ollerton2003),labsize = 2,type="diagonal")

# visualize the small1976 network
data("small1976",package="bipartite")

bipartite::visweb(small1976,labsize = 2)
# make diagonal
bipartite::visweb(small1976,labsize = 2,type="diagonal")

# plot as a bipartite network
bipartite::plotweb(small1976)
bipartite::plotweb(ollerton2003)

# calculate different network statistics 
stats_olerton2003 <-bipartite::networklevel(ollerton2003,index="ALL")
stats_olerton2003 # inspect
stats_small1976 <-bipartite::networklevel(small1976,index="ALL")
stats_small1976 # inspect the statistics 

# proportion of possible links realized
stats_small1976$connectance
stats_olerton2003$connectance 

# index of specialisation
stats_small1976$H2
stats_olerton2003$H2 

# slope of extinction
d_small1976 <- bipartite::second.extinct(small1976,
                            participant="higher",
                            method="abundance",
                            nrep=20,
                            details=F
                            )
slope.bipartite(d_small1976,plot.it=T)
# for next network
d_ollerton2003 <- bipartite::second.extinct(ollerton2003,
                               participant="higher",
                               method="abundance",
                               nrep=5,
                               details=F
)
slope.bipartite(d_ollerton2003,plot.it=T)

# move the data to igraph format
class(small1976)
d1_small1976<-igraph::graph_from_incidence_matrix(small1976,
                                                  weighted = T)
class(d1_small1976)
plot.igraph(d1_small1976,
            layout=layout.circle,
            edge.width=E(d1_small1976)$weight/5,
            vertex.color="grey",
            vertex.size=5)
            
