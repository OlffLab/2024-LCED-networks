#### Data types and structures in R when working with networks

library(tidyverse) # data manipulation inc dplyr, tibble
library(igraph)    # analysing, plotting networks

# make a VECTOR, which is a one-dimensional array of elements, for example 8 species
nodes <- c("GE","HA","CA","PM","PA","FR","EA","AM")
nodes
length(nodes)  # number of species in my network

# make an ARRAY, a vector with additional dimensions equivalent to a MATRIX
nodes_array<-array(nodes,dim=c(3,5))
nodes_array
class(nodes_array) # show the type of data structure 
nodes_array[1,] # first row
nodes_array[,2] # [whichrow,whichcolumn] second column
nodes_array[2,2] # second row, second column

# make a DATAFRAME for the properties of the nodes of a network (reflecting species traits)
# in practice, read these things from a google sheets, an xlsx or a csv file
bodymass <- c(0.5,2,150,23,40,80,23,22)
n_conc <-c(1.1,0.7,0.4,3,2.5,2.0,0.8,0.5)
funcgroup <-c("H","H","H","P","P","P","P","P")
nodes_df <- data.frame(cbind(nodes,bodymass,n_conc,funcgroup))
nodes_df
class(nodes_df)
names(nodes_df) # show the variables
nodes_df$bodymass # show a particular variable

# make a DATAFRAME for the edges or links of a network
node1 <-c("GE","GE","GE","HA","HA","HA","HA","CA","CA","CA")
node2<- c("PM","PA","FR","PM","PA","FR","EA","FR","EA","AM")
interaction_s <-c(3,2,1,4,2,1,1,1,3,2)
links_df<-cbind(node1,node2,interaction_s) %>% #piping
          tibble::as_tibble() %>%
          dplyr::mutate(interaction_s=as.numeric(interaction_s)) # transform to numbers
links_df
class(links_df)
links_df_sum <- links_df %>%
                dplyr::group_by(node1) %>%
                dplyr::summarise(interact_s_mean=mean(interaction_s))
links_df_sum

# <- gets the value off (assignment)
# == logical operator
3==2
a<-2
a

# make a TABLE
table(nodes_df$nodes,nodes_df$bodymass)
table(links_df$node1,links_df$node2)

# show the tables of our interaction network database
links_df
nodes_df

# join the links and nodes tables to an igraph network object
# nodes = vertices
# links = edges
# make an igraph data structure from the two dataframes 
g1<-igraph::graph_from_data_frame(d=links_df,vertices=nodes_df,directed=FALSE)
g1
V(g1) # what are nodes in the network
E(g1) # what are the edges in the network
E(g1)$interaction_s
# plot the network with plants and herbivores colored separately
# as a weighted network
plot(g1,
    vertex.color=ifelse(V(g1)$funcgroup=="P","green","orange"),
    edge.width=E(g1)$interaction_s*3,
    layout=layout_nicely)

# Load required libraries
library(ggraph)
library(igraph)
library(igraphdata)
data(package="igraphdata")
data(foodwebs,package="igraphdata")
Chesapeake <- foodwebs$Chesapeake

# Load the Les Miserables dataset

# Create the network plot using ggraph
ggraph(foodweb, layout="tree") +  
  geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +  # Links between species
  geom_node_point(color = "darkorange", size = 5) +  # Nodes representing species
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  # Species labels
  theme_void() +  # Remove background and axes
  scale_y_reverse() +  # Flip the y-axis to rotate the tree by 180 degrees
  ggtitle("Chesapeake Food Web")  # Add title

# Create an edge list where the first two columns are species interactions, and the third column is the weight
species_interactions <- data.frame(
  predator = c("Species_A", "Species_A", "Species_B", "Species_C", "Species_D"),
  prey = c("Species_B", "Species_C", "Species_C", "Species_D", "Species_E"),
  weight = c(5, 2, 3, 7, 4)  # Weight of interaction (e.g., biomass transfer)
)

# Convert to a graph object
food_web <- graph_from_data_frame(species_interactions, directed = TRUE)

# Add weights to the edges
E(food_web)$weight <- species_interactions$weight

ggraph(food_web, layout="tree") +  
  geom_edge_link(aes(linewidth = weight), show.legend = FALSE) +  # Links between species
  geom_node_point(color = "darkorange", size = 5) +  # Nodes representing species
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +  # Species labels
  theme_void() +  # Remove background and axes
  scale_y_reverse() +  # Flip the y-axis to rotate the tree by 180 degrees
  ggtitle("Chesapeake Food Web")  # Add title
