library(ggplot2)
library(tidyr)

"
ggplot/viz resources:
  - Soure for this code: https://uc-r.github.io/ggplot_intro
  - General good resource: https://r4ds.had.co.nz/data-visualisation.html
"

# preview data
mpg


##### Layers #####

# create canvas
ggplot(mpg)

# variables of interest mapped
ggplot(mpg, aes(x = displ, y = hwy))

# data plotted
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

mpg


###### Playing with aesthetics ######

# mapping color to a variable
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

# common mistake!!!
ggplot(mpg, aes(x = displ, y = hwy), color = class) +
  geom_point()

# what if i just want blue points?
ggplot(mpg, aes(x = displ, y = hwy), color = "blue") +
  geom_point()

# or maybe...
ggplot(mpg, aes(x = displ, y = hwy, color = "blue")) +
  geom_point()
"above tells the aesthetics object that every car has an attribute 'blue' which 
 should be used to determine color, NOT that the color itself should be blue!"

# make THE POINTS blue
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

# what other things can we do?


##### Other Geoms #####

# x and y mapping needed!
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()

# no y mapping needed!
ggplot(data = mpg, aes(x = class)) +
  geom_bar()  

ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram()

# now the fun part: combinations of geoms

# plot with both points and smoothed line
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# you can even manipulate color and other features separately
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue") +
  geom_smooth(color = "red")
# since we declared the aes in the ggplot() function, the information was passed
# to both geoms


##### Separating aes in geoms #####

# color aesthetic passed to each geom layer
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth(se = FALSE)

# color aesthetic specified for only the geom_point layer
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE)


##### Facets #####

# generating a row of plots based on a variable
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(~ class)

# generating a rows and columns of plots based on a variables
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(year ~ cyl)


##### Labels & Annotations #####

# standard axis labels and titles
ggplot(mpg, aes(x = displ, y = hwy, color = class, shape = factor(year))) +
  geom_point() +
  labs(title = "Fuel Efficiency by Engine Power",
       subtitle = "Fuel economy data from 1999 and 2008 for 38 popular models of cars",
       x = "Engine power (litres displacement)",
       y = "Fuel Efficiency (miles per gallon)",
       color = "Car Type", 
       shape = "Car Year")

# adding text labels to points in a plot
library(dplyr)

# a data table of each car that has best efficiency of its type
# this is a good example of how often data needs to be transformed to plot in more advanced ways
best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) +
  geom_label(data = best_in_class, aes(label = model), alpha = 0.5)

# preventing label overlap
# i didn't know this package existed! a google search of "how to prevent point 
# labels from overlapping R" yields this package in the first result.

library(ggrepel)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point(aes(color = class)) +
  geom_text_repel(data = best_in_class, aes(label = model))

##### Plotly and ggplotly #####

# these plots are pretty and all, but what about ~interactiveness~
# instead of adding static labels to our plot from earlier, we can use the 
# ggplotly wrapper to make the plot interactive

library(plotly)

plot1 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  labs(title = "Fuel Efficiency by Engine Power",
       subtitle = "Fuel economy data from 1999 and 2008 for 38 popular models of cars",
       x = "Engine power (litres displacement)",
       y = "Fuel Efficiency (miles per gallon)",
       color = "Car Type")

ggplotly(plot1)

ggplotly(ggplot(mpg, aes(
  x = displ, y = hwy, color = class
)) +
  geom_point())

# this gplotly wrapper works for /most/ ggplot geoms, but not all!
# more info: https://plotly.com/ggplot2/

# plotly plots are the basis for our DMME dashboard
# here's an example of how plotly's grammar differs from ggplot 
# it's also similar to how a lot of the dashboard plots are made
plot2 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

p <- ggplotly(plot2) %>%
  layout(                        # all of layout's properties: /r/reference/#layout
    title = "Fuel Efficiency by Engine Power", # layout's title: /r/reference/#layout-title
    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
      title = "Engine power (litres displacement)",      # xaxis's title: /r/reference/#layout-xaxis-title
      showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
      title = "Fuel Efficiency (miles per gallon)")     # yaxis's title: /r/reference/#layout-yaxis-title
  )

p


##### Statistical Transformations ######

ggplot(mpg, aes(x = class)) +
  geom_bar()
# where does bar get the coutns from? it uses stat_count
# what if we want to change that?

class_count <- dplyr::count(mpg, class)
class_count

ggplot(class_count, aes(x = class, y = n)) +
  geom_bar(stat = "identity")
# we can change the function used to identity to make use of the data we've already created

# there are other, arguably more useful ways to use this flexibility, like adding a mean line
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(color = "grey") + 
  stat_summary(fun = "mean", geom = "line", size = 1, linetype = "dashed")

# and maybe even
(ggplot(mpg, aes(displ, hwy)) + 
  geom_point(color = "grey") + 
  stat_summary(fun = "mean", geom = "line", size = 1, linetype = "dashed")) %>%
  ggplotly()

##### Position Adjustments #####

# bar chart of class, colored by drive (front, rear, 4-wheel)
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar()

# position = "dodge": values next to each other
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "dodge")

# position = "fill": percentage chart
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill")

# Check the documentation for each particular geom to learn more about its positioning adjustments

##### Managing Scales #####

# color the data by engine type
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

# behind the scenes
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete()

# scale_[aes property]_[scale] is commonly used to manage scales
# scale can be log, reverse, discrete, continuous, etc

# parameters of the scale function can be used to format axes
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, by = .2), labels = scales::percent)

# fun color scaling with brewer!
# default color brewer
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  scale_color_brewer()

# specifying color palette
ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  scale_color_brewer(palette = "Set3")

# quickly flip a graph with coord_flip()
ggplot(mpg, aes(x = class)) +
  geom_bar() +
  coord_flip()
