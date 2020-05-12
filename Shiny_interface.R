### GGplot2 is used for the graphing, see https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
library(ggplot2)
### Tidyverse is unused right now, but can be used to change data and for arranging/sorting data better
library(tidyverse)
library(gWidgets)

plant_data <- read.csv("BIENdata.csv")
head(plant_data)


### Clears the user-defined variables so as not to read old data
rm(list = lsf.str())
### Designating the trait at the start
### IMPORTANT: This must be THE SAME TEXT as what is in the .csv
trait <- "leaf area"
species <- "Bromus rubens"



### Sorting the data for the specified species
species_sort <- plant_data %>% filter(scrubbed_species_binomial == species)


### Filtering all the data for the specified trait
species_sort_trait <- species_sort %>% filter(trait_name == trait)

### Creating labels for x-axis and graph plot based off data choices
graph_label <- paste(trait, "of", species) 
trait_label <- paste(trait, species_sort_trait$unit)

### Assigning the trait value to its independant variable
trait_sort <- species_sort_trait$trait_value

### The data is read as a function from the .csv, so it must be converted to characters and then numeric to be graphed as continuous data
trait_sort <- as.numeric(as.character(trait_sort))
### binwidth determite function that takes a 10-frame histogram distribution
bin_distibution <- (0 + max(trait_sort)/10)

### Binwidth is the range of each histogram bar, color is the line color, and fill is the fill of the actual graph
ggplot() + geom_histogram(data = species_sort_trait, aes(x = trait_sort), binwidth = bin_distibution, fill = "green", color = "black")+
  ggtitle(graph_label) +
  xlab(trait_label)+
  ylab ("Number of Individuals") 

average <- mean(trait_sort) 

###Density plot for different looking graph
#ggplot() + geom_density(data = Bromus_rubens_trait, aes(x = trait_sort), binwidth = 20, fill = "green")+
#  ggtitle("Leaf Area of Bromus Rubens") +
#  xlab("Leaf Area (mm2)")+
#  ylab ("Number of Individuals") 


library(shiny)


if (interactive()) {
  ui <- fluidPage(
    sidebarPanel(
   selectInput(inputId = "species", label = strong("Species"),
                choices = unique(plant_data$scrubbed_species_binomial),
                selected = "Bromus rubens"),
   selectInput(inputId = "trait", label = strong("Trait"),
                choices = unique(plant_data$trait_name))),
    mainPanel(
   plotOutput("plot")))

   server = function(input, output, session) {
     observe({
     x <- input$species
     selected_species <-  plant_data %>% filter(scrubbed_species_binomial == x)
     y <- unique(selected_species$trait_name)
     updateSelectInput(session, "trait", choices = y, label = paste("Select species specific trait,", length(y), "options"),)
     v <- input$trait
     selected_trait <- selected_species %>% filter(trait_name == v)
     selected_trait_sort <- selected_trait$trait_value
     selected_trait_sort <- as.numeric(as.character(selected_trait_sort))
      output$plot <- renderPlot({ hist(rnorm(100, trait_sort), xlab = v, ylab = "Number of Individuals", main = paste("distribution of", v, "of", x))
      })
 
      })
      }
     
     

   shinyApp(ui = ui, server = server)
   }


