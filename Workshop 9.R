#Workshop 9 - Data Visualisation II
#1.Introduction
#This week we will do more on labelling data points and then look into manipulating the optics of our graph using theme()

install.packages("palmerpenguins")
install.packages("tidyverse")
library(palmerpenguins)
library(tidyverse)

#2.Annotating data points
#Q.Let’s say we would like to label the five biggest Gentoo penguins with names in a scatterplot.
#We can subset our dataframe to the five heaviest Gentoo penguins and add a column with names. 
#We can then use that dataframe to label the dots.

#Subset penguins dataframe to the five heaviest penguins
big_penguins <- penguins %>%
  filter(species == "Gentoo",!is.na(body_mass_g)) %>% #remove na
  arrange(body_mass_g) %>% tail(n = 5L) #arrange according to body mass

#Add a column with names to big_penguins
big_penguins$names <- c("Dwayne", "Hulk", "Giant", "Gwendoline", "Usain")

#Plot all Gentoo penguins adn use big_penguins dataframe for labels 
penguins %>% filter(species == "Gentoo") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point(aes(colour = flipper_length_mm)) +
  geom_text( 
    data = big_penguins,#We are switching to diff data 
    mapping = aes(label = names),
    nudge_x = -1.5,
    nudge_y = -0.5,
    colour = "red"
  ) +
  xlim(3900, 6400)
#From geom_text(), we are switching to different data 
#It inherites the position mappings from ggplot(), that is how geom_text() knows where to put the labels
#We use the nudge parameters to push the labels down and left a bit, so that they don’t sit right on top of the dots they are labeling
#We’ve made the x-axis a bit longer with xlim(), so that the names don’t get cut off.

#If your desired labels are already in your dataframe you can filter within the data argument to geom_text
#Q. For example, I want to highlight the home islands of Adelie penguins with flipper lengths over 200 mm:






