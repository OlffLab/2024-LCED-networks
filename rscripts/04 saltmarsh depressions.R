#### analysis of the saltmarsh data we collected on monday with the LCED course

# clear everything in the R environment
rm(list = ls())
renv::restore()  # restore the library

# load libraries
library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)

# we work with the following database that you made on monday
# browseURL("https://docs.google.com/spreadsheets/d/1gAhwMWjA6aD3SMHb0j9X_4xYaxsDBNre6B5XyWDgzzI/edit?usp=sharing")

# read the data tables from the database
MetTables<- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2zhCjdrR-4sMpcfvyXunOBdLXKI2VYBnTa8u2Xs-yCTQmLYhE54bl7g9a2-9zRxvgqmCe0RXDuW1X/pub?gid=894288297&single=true&output=csv')
MetVariables <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2zhCjdrR-4sMpcfvyXunOBdLXKI2VYBnTa8u2Xs-yCTQmLYhE54bl7g9a2-9zRxvgqmCe0RXDuW1X/pub?gid=1304764403&single=true&output=csv")
DimPlantSpecies <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2zhCjdrR-4sMpcfvyXunOBdLXKI2VYBnTa8u2Xs-yCTQmLYhE54bl7g9a2-9zRxvgqmCe0RXDuW1X/pub?gid=2069837170&single=true&output=csv")
DimGroup <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2zhCjdrR-4sMpcfvyXunOBdLXKI2VYBnTa8u2Xs-yCTQmLYhE54bl7g9a2-9zRxvgqmCe0RXDuW1X/pub?gid=556890394&single=true&output=csv")
FactVegObs <-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2zhCjdrR-4sMpcfvyXunOBdLXKI2VYBnTa8u2Xs-yCTQmLYhE54bl7g9a2-9zRxvgqmCe0RXDuW1X/pub?gid=0&single=true&output=csv")

### Add attributes of the dimension tables to the fact table
# show the relations in the database
MetVariables |> dplyr::filter(!is.na(ValidationTable))

# join the different tables into one based on their relations
AllData<-FactVegObs |>
  dplyr::left_join(DimGroup,by="Group_ID") |>
  dplyr::left_join(DimPlantSpecies,by="PlantSpecies_ID")

names(AllData)

p1 <-AllData |> 
  ggplot(aes(x=Type,y=BareCov, fill=Type)) +
  geom_boxplot() +
  labs(x="Inside or outside depression",
       y="% bare cover",
       title="Bare soil")
p1
p2 <-AllData |> 
  ggplot(aes(x=Type,y=ClayThick, fill=Type)) +
  geom_boxplot() +
  labs(x="Inside or outside depression",
       y="Clay thickness (cm)",
       title="Clay layer")
p2
p3<- AllData |>
  group_by(Type,DomPlantName) |>
  summarize(Count=n()) |>
  ggplot(aes(x=Type,y=Count, fill=DomPlantName)) +
  geom_bar(stat="identity") +
  labs(x="Inside or outside depression",
       y="number of plots",
       title="Dominant plant species",
       fill="Species")
p3
p4<- AllData |>
  group_by(Type,AnnualPerenn) |>
  summarize(Count=n()) |>
  ggplot(aes(x=Type,y=Count, fill=AnnualPerenn)) +
  geom_bar(stat="identity") +
  labs(x="Inside or outside depression",
       y="number of plots",
       title="Dominant plant species",
       fill="Plant functional type")
p4
# show all the plots as a panel plot (using the patchwork library)
combined_plot <- (p1 + p2 + p3 + p4) +
  patchwork::plot_annotation(tag_levels = 'a', tag_suffix = ')')  # Automatically label as a), b), c)
combined_plot

##### statistical tests
# Fit a linear mixed model testing the difference between inside outside depressions, 
# with a random effect of Point (paired samples were taken at each point, as a block)
model <- lmerTest::lmer(ClayThick ~ Type + (1 | Point), data = AllData)
# Display the summary of the model
summary(model)

# test if the bare soil cover is different

# test if the species community composition is different with a permanova

