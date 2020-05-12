#if(!require(devtools)) install.packages("devtools")
### GGplot2 is used for the graphing, see https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
#also install ggpubr for multiple graph organization
library(ggplot2)
library(ggpubr)
### Tidyverse is unused right now, but can be used to change data and for arranging/sorting data better
library(tidyverse)
library(gWidgets)

plant_data <- read.csv("BIENdata.csv")
head(plant_data)


### Clears the user-defined variables so as not to read old data
rm(list = lsf.str())
### The following two objects are the only things that need to be changed in the program
### IMPORTANT: This must be THE SAME TEXT as what is in the .csv
trait <- "leaf nitrogen content per leaf dry mass"
species <- "Phragmites australis"

### Sorting the data for the specified species
species_sort <- plant_data %>% filter(scrubbed_species_binomial == species)

traits <- unique(species_sort$trait_name)

### Filtering all the data for the specified trait
species_sort_trait <- species_sort %>% filter(trait_name == trait)

### Creating labels for x-axis and graph plot based off data choices
graph_label <- paste(trait, "of", species) 
trait_label <- paste(trait, "(", species_sort_trait$unit, ")")

### Assigning the trait value to its independant variable
trait_sort <- species_sort_trait$trait_value

### The data is read as a function from the .csv, so it must be converted to characters and then numeric to be graphed as continuous data
trait_sort <- as.numeric(as.character(trait_sort))
### binwidth determite function that takes a 10-frame histogram distribution
bin_distribution <- (0 + max(trait_sort)/10)

### Binwidth is the range of each histogram bar, color is the line color, and fill is the fill of the actual graph
z <- ggplot() + geom_histogram(data = species_sort_trait, aes(x = trait_sort), binwidth = bin_distribution, fill = "green", color = "black")+
  ggtitle(graph_label) +
  xlab(trait_label)+
  ylab ("Number of Individuals") 

average <- mean(trait_sort) 

###Density plot for different looking graph
y <- ggplot() + geom_density(data = species_sort_trait, aes(x = trait_sort), fill = "green")+
  ggtitle(graph_label) +
  xlab(trait_label)+
  ylab ("Distribution") 

ggarrange(z,y, nrow = 2)
