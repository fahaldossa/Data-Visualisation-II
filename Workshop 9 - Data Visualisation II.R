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

#Plot all Gentoo penguins and use big_penguins data frame for labels 
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
#It inherits the position mappings from ggplot(), that is how geom_text() knows where to put the labels
#We use the nudge parameters to push the labels down and left a bit, so that they don’t sit right on top of the dots they are labeling
#We’ve made the x-axis a bit longer with xlim(), so that the names don’t get cut off.

#If your desired labels are already in your dataframe you can filter within the data argument to geom_text
#Q. For example, I want to highlight the home islands of Adelie penguins with flipper lengths over 200 mm:
penguins %>% filter(species == "Adelie") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  geom_text(
    data = filter(penguins, species == "Adelie" &
                    flipper_length_mm > 200),
    aes(label = island),
    nudge_y = -0.7
  )
#q. Why do we have to filter for the species again in geom_text()? What happens if we don’t do that?
#a. this is because it will include all the species with flipper length over 200mm

#3. Facets
# Another nice way to separate subsets of data is faceting where you produce several small plots separated by categorical variables
#There are 2 types of faceting: facet-wrap() and facet_grid()
#facet_wrap: Takes a number of plots and 'wraps' them into a panel 

#facet_wrap - malaria data
# Reading in data
modeltab <- read.table("wmr_modelling.txt",sep="\t",header=T)

# Subsetting to the first half or so for readability
modeltab_short <- head(modeltab, n = 506L)

# Plotting deaths in years 2019-2021 faceted by country
modeltab_short %>% drop_na() %>% filter(year >2018) %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~country, ncol = 5, dir = "v")
#drop_na: gets rid of all the na's
#filter the years to 2019-2021
#geom_col(): using fill, you can color the columns
#facet_wrap: '~' determines the variable by which we want to split our data into separate plots 
#We can choose the number of rows and columns with ncol or nrow (One follows from the other, so you only need to set one)
#dir: controls the direction of the wrap (v or h)

#Copy the code above and play around with different options. 
#Q. What does the facet_wrap() argument as.table do?
#A. If TRUE, the default, the facets are laid out like a table, with the highest values at the bottom right. If FALSE, the facets are laid out like a plot with the highet value at the top-right

#Q.What happens if you set the argument 'scales' to 'free'?
modeltab_short %>% drop_na() %>% filter(year >2018) %>%
  ggplot(aes(x = year, y = deaths)) +
  geom_col(fill = "firebrick") +
  facet_wrap(~country, ncol = 5, dir = "v", scales = 'free')
#Here the y and x axis are 'freed' depending on what the scales axis is 

#facet_grid(): lays out the plots in a 2D grid. This is often used to separate plots by two categorical variables like so:
penguins %>% drop_na() %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() +
  facet_grid(sex ~ species) #splits both sex and species

#The formula in facet_grid() determines the rows first, then the columns. You can also use this to control how you want plots laid out that are separated by just one variable:
p_plot <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point()

p_plot + facet_grid(. ~ species) #since its rows then columns, species will be split into columns

p_plot + facet_grid(species ~ .) #Here species are split into rows 

#4.Patchwork
#When we publish papers or produce figures for a presentation, we often need to combine several panels into a larger figure. There is a package called patchwork that can do that for us
install.packages("patchwork")
library(patchwork)

p1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm, colour = species)) +
  geom_point() + facet_grid(. ~ species)

p2 <- penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity")

p3 <- penguins %>% drop_na() %>% 
  ggplot(aes(x = species, y = body_mass_g)) +
  geom_violin(aes(fill = sex))

#Patchwork stitches these plot together using an intuitive formula approach: 
p1/(p2+p3)

#We can also say that we want only one plot on the left, and the other 2 stacked on the right: 
p2 | (p1/p3)

#Patchwork allows you to add annotations using the plot_annotation() function:
p1/(p2+p3) + plot_annotation(tag_levels = "a", #labels a,b,c
                             title = "Plenty of penguin plots")

#Patchwork is very useful when we want to align plots with the same x or y axis
#%in% is extremely handy or sub-setting 
p_deaths <- modeltab %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = deaths, colour = country)) +
  geom_point() +
  geom_line() +
  xlim(1999,2022)

p_pop <- modeltab %>% filter(country %in% c("Angola", "Burkina Faso", "Chad")) %>% 
  ggplot(aes(x = year, y = population, fill = country)) +
  geom_col(position = "dodge") +
  xlim(1999,2022)

p_deaths/p_pop

#5. Colours
#These are the colour schemes inbuilt in ggplot2
#In principle, we use colours in three ways: We map either categorical or continuous variables to them, or we manually colour geometric elements independently from a variable.

#Discrete colour scales: 
#When changing colours manually you can use the built-in colour names in R. There are 657 of them! You can get their names with colours(). 

#Here is an example of how to change discrete colours manually: scale_fill_manual(values = c())
s_counts <- penguins %>% ggplot(aes(x = species, fill = species)) +
  geom_bar()

s_counts + scale_fill_manual(values = c("yellow2", "magenta", "darkblue"))

#It’s a good idea to use established colour palettes to make sure the colours are well-balanced and colour-blind friendly. ggplot2 has several built-in colour palettes that are based on colour packages. For discrete colours we can use scales with palettes from “ColorBrewer”
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
#ColorBrewer has three types of palettes: The first type is suitable for ranked discrete graphs, e.g. a series of years. The second is best for our example plot of unranked categorical data. The third type is used when you have discrete diverging data going from low to high through 0.

#We can apply ColorBrewer palettes like this:
brew_1 <- s_counts + scale_fill_brewer(palette = "Set1")
brew_2 <- s_counts + scale_fill_brewer(palette = "Dark2", direction = -1)

brew_1 + brew_2 #You will notice that the colours in the first graph aren’t great because the red and green will be hard to distinguish for people with colour blindness.

#The viridis scales are designed to be colour-blind friendly:
viri_1 <- s_counts + scale_fill_viridis_d() #Uses default option viridis
viri_2 <- s_counts + scale_fill_viridis_d(option = "plasma")

viri_1 + viri_2

#Continuous colour scales
#Similar to discrete scales you can manipulate continuous scales as well. The function for continuous viridis scales is scale_colour_viridis_c() and for ColorBrewer scale_colour_distiller().
con_plot_1 <- penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(size = body_mass_g, colour = body_mass_g))

con_plot_2 <- con_plot_1 + scale_colour_viridis_c(option = "magma")

con_plot_1 + con_plot_2

#NA values
#If you’re displaying NA values in your graphs it is recommended to give them a colour. Some palette functions have grey set as default for NA whereas others don’t. In the latter case the colour of NA gets sometimes set to the background of the plot. This can have unintended results, for example if you remove the default grey plot background (more on that in the next section).

#6.Themes
#ggplot2 has a default theme, theme_grey(), that determines the overall look of your plot. It sets the plot panel to grey, grid lines and axes to white, determines where the legend goes, etc. There is a number of other complete themes available, such as theme_minimal(), theme_classic(), etc. We can simply change from the default to another one like so:
con_plot_3 <- con_plot_1 + theme_classic()

con_plot_1 + con_plot_3 + plot_annotation(title = "Default theme on the left, theme_classic() on the right")
#The function theme() allows us to change each element of the plot.

#The elements of a plot are divided into three broad types: Lines, text and rectangles. Accordingly, there are three functions that are used to manipulate them: element_line(), element_text(), and element_rect(). If we want to remove an element entirely we use the function element_blank().
#Let’s see how this works in practice:
penguins %>% drop_na() %>%
  ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point(aes(colour = body_mass_g)) +
  labs(title = "My pretty plot") +
  scale_colour_viridis_c(option = "magma") +
  theme(legend.position = "bottom",
        axis.title.x = element_text(colour = "red", size = 14, hjust = 1),
        axis.title.y = element_blank(),
        axis.line.y = element_line(colour = "cornflowerblue", size = 4),
        axis.text.y = element_text(size = 20, angle = 45),
        panel.background = element_rect(colour = "green", fill = "yellow", size = 10),
        plot.title = element_text(family = "Times", face = "italic",  hjust = 0.5, size = 18))
#The changes I’ve made to the plot are obviously hideous, but they give you a glimpse of what is possible. Removing certain elements, such as axis titles is useful when we combine different plots using patchwork and we don’t want to repeat the same axis names and labels.
#One note on fonts: R knows a few basic fonts such as Helvetica, Courier, etc., but most of the time we would stick with the default.

#theme() is also very handy for adjusting the position of the legend. As you’ve seen above, we can move it below the plot area, but we can also put it in the plot to save space:
penguins %>%  drop_na() %>%
  ggplot(aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), position = "identity") +
  theme(legend.position = c(0.9,0.85),
        legend.background = element_blank())















