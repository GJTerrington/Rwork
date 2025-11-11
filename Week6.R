library(tidyverse)
library(MASS)
library(rgl)
library(waffle)
View(mpg)

# this creates a simple bar chart of manufacturer vs number of cars released
ggplot(mpg, aes(manufacturer)) + geom_bar() +
  labs(x = "Manufacturer", y = "Frequency",
       title = "Number of cars released",
       caption = "mpg dataset")

# this creates a simple bar chart of manufacturer vs number of cars released, but changes the
ggplot(mpg, aes(manufacturer)) + geom_bar(width = 0.5) +
  labs(x = "Manufacturer", y = "Frequency",
       title = "Number of cars released",
       caption = "mpg dataset")

# this uses dplyr to count the freq of each category. Groups dataset by manufacturer, and counts the number of rows for each.
df <- mpg %>% group_by(manufacturer) %>% summarise(count = n())

# this creates the same as the first bar chart but now using stat function identity
ggplot(df, aes(manufacturer, count)) + geom_bar(stat = "identity") +
  labs(x = "Manufacturer", y = "Frequency",
       title = "Number of cars released",
       caption = "mpg dataset")

# this creates the same chart but uses the theme function, changing the aesthetics as can be observed
ggplot(mpg, aes(manufacturer)) + geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Manufacturer", y = "Frequency",
       title = "Number of cars released",
       caption = "mpg dataset")

# this creates a pie chart but it has axis labels etc
ggplot(mpg, aes(factor(1), fill = factor(cyl))) + geom_bar() +
  coord_polar(theta = "y")

# this creates a pie chart without the axis labels using theme and element_blank on the axis lines, background, text and ticks on the axes, and the labels
ggplot(mpg, aes(factor(1), fill = factor(cyl))) + geom_bar() +
  coord_polar(theta = "y") +
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Number of \ncylinders",
       title = "Proportion of cars by cylinder number",
       caption = "mpg dataset")

# this downloads the waffle library and its dependencies
install.packages(c('DT', 'htmlwidgets', 'extrafont'))
install.packages("waffle")
library(waffle)

# this uses dplyr to count the freq of each category. Groups dataset by manufacturer, and counts the number of rows for each
df <- mpg %>% group_by(cyl = factor(cyl)) %>% summarise(count = n())

# this creates a waffle chart
ggplot(df, aes(fill = cyl, values = count)) + geom_waffle()

# this creates a pie chart for the fuel type
ggplot(mpg, aes(factor(1), fill = factor(fl))) + geom_bar() +
  coord_polar(theta = "y") +
  theme(axis.line = element_blank(), panel.background = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Type of \nfuel",
       title = "Proportion of cars by fuel type",
       caption = "mpg dataset")

# this this uses dplyr to count the freq of each category of fuel type. Groups dataset by fuel type, and counts the number of rows for each
df <- mpg %>% group_by(fl = factor(fl)) %>% summarise(count = n())

# creates waffle chart of fuel type
ggplot(df, aes(fill = fl, values = count)) + geom_waffle()

# this installs and loads the package for creating tree maps
install.packages("treemapify")
library(treemapify)

View(G20)

# this creates a tree map for the distribution of countries by GDP
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) +
  geom_treemap() + geom_treemap_text() +
  labs(title = "Distribution of countries by GDP",
       fill = "Human \ndevelopment \nindex",
       caption = "G20 data")

# this creates a tree map for the distribution of countries by GDP but with better aesthetics as can be shown
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Distribution of countries by GDP",
       fill = "Human \ndevelopment \nindex",
       caption = "G20 data")

# this creates the same tree map but with hierarchy using subgroup, grouping by continent
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country, subgroup = region)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  geom_treemap_subgroup_border() + geom_treemap_subgroup_text() +
  labs(title = "Distribution of countries by GDP",
       fill = "Human \ndevelopment \nindex",
       caption = "G20 data")

# this creates a data frame for manufacturer count
manufacturer_count <- mpg %>% count(manufacturer, name = "num_cars")

# this then creates a df including hwy
manufacturer_summary <- mpg %>%
  group_by(manufacturer) %>%
  summarise(num_cars = n(),
            avg_mpg = mean(hwy, na.rm = TRUE))

# this now plots this on a tree map
ggplot(manufacturer_summary, aes(area = num_cars, fill = avg_mpg, label = manufacturer)) +
  geom_treemap() + geom_treemap_text(fontface = "italic", colour = "white", place = "centre") +
  labs(title = "Distribution of cars by manufacturer",
       fill = "City Fuel \nConsumption",
       caption = "mpg data")

# this installs gridExtra which is required for grid.arrange
install.packages("gridExtra")
library(gridExtra)

# this creates three plots, one using size, one colour, and one shape, to distinguish the label variable
size.plot <- ggplot(mpg, aes(displ, cyl)) + geom_point(aes(size = cty)) +
  labs(x = 'Displacement', y = 'Cylinders', size = 'City mpg')
colour.plot <- ggplot(mpg, aes(displ,cyl)) + geom_point(aes(colour = hwy)) +
  labs(x = 'Displacement', y = 'Cylinders', size = 'Highway\nmpg')
shape.plot <- ggplot(mpg, aes(displ,cyl)) + geom_point(aes(shape = fl)) +
  labs(x = 'Displacement', y = 'Cylinders', shape = 'Fuel type')
grid.arrange(size.plot, colour.plot, shape.plot, ncol = 1)

# this creates a plot whereby points are separated by translucency, with a more opaque colour meaning more data points are at the same spot
ggplot(mpg, aes(displ, cyl)) + geom_point(size = 6, colour = rgb(1, 0, 0, 0.25)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Translucent colours can show data point density",
       caption = "mpg dataset")

# the same as above but using jitter
ggplot(mpg, aes(displ, cyl)) +
  geom_point(size = 2,
             position = position_jitter(w = 0.05, h = 0.05)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Random noise can also show data point density",
       caption = "mpg dataset")

# the same as above but using position_jitter to control how much the random values withh affect the position
ggplot(mpg, aes(displ, cyl)) +
  geom_point(size = 1,
             position = position_jitter(w = 0, h = 0.15)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Noise can be added on just one axis",
       caption = "mpg dataset")

# this adds a line of best fit to a scatterplot, using the geom_smooth function. This calculates the line based on the method parameter. The se parameter determines if the confidence intervals of the line are shown.
ggplot(mpg, aes(cty, hwy)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "City", y = "Highway", title = "Comparing fuel economy",
       caption = "mpg dataset")

# exercise 5.1
ggplot(mpg, aes(cty, hwy)) + geom_point(size = 6, colour = rgb(0, 1, 0, 0.25)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Translucent colours can show data point density",
       caption = "mpg dataset")

ggplot(mpg, aes(cty, hwy)) +
  geom_point(size = 2,
             position = position_jitter(w = 0.15, h = 0.15)) +
  labs(x = "Displacement", y = "Cylinders",
       title = "Random noise can also show data point density",
       caption = "mpg dataset")

ggplot(mpg, aes(cty, hwy)) + geom_point() +
  geom_smooth(method = "rlm", se = FALSE) +
  labs(x = "City", y = "Highway", title = "Comparing fuel economy",
       caption = "mpg dataset")

# this creates a line plot as shown after getting economics data
data("economics")

ggplot(economics, aes(x = date, y = unemploy)) + geom_line() +
  labs(x = "Date", y = "Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset")

# this makes the same plot but with the line thicker and red
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(colour = "red", size = 2) +
  labs(x = "Date", y = "Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset")

# this creates the same plot but with the line colour mapped to population
ggplot(economics, aes(x = date, y = unemploy)) +
  geom_line(aes(colour = pop), size = 1) +
  labs(x = "Date", y = "Unemployed (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset",
       colour = "Population\n(in thousands)")

# this plots different lines on the same plot by calling geom_line multiple times. Because their values are very different, I transform the Y axis to a logarithmic using scale_y_continuous and the trans = "log" parameter. Also define where axis ticks should appear with the breaks parameter.
ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy)) +
  geom_line(aes(y = pop)) +
  labs(x = "Date", y = "Numbers (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset") +
  scale_y_continuous(trans = "log",
                     breaks = c(10**3, 10**4, 10**5, 10**6))

# the above is a bad example. we can show what each line represents using colour by below.
ggplot(economics, aes(x = date)) +
  geom_line(aes(y = unemploy, col = "Unemployed")) +
  geom_line(aes(y = pop, col = "Total population")) +
  labs(x = "Date", y = "Numbers (in thousands)",
       title = "Unemployment progression",
       caption = "economics dataset") +
  scale_y_continuous(trans = "log",
                     breaks = c(10**3, 10**4, 10**5, 10**6)) +
  scale_colour_manual(name = "Legend",
                      values = c("Unemployed" = "red",
                                 "Total population" = "blue"))

# creates three histograms from code that includes parameters to define number of bins (bins), binwidth, and breaks (a vector of numbers representing the bin boundaries)
hist1 <- ggplot(mpg, aes(cty)) +
  geom_histogram(bins = 10, fill = "lightgreen") +
  labs(x = "Fuel economy in the city",
       y = "Frequency",
       caption = "mpg dataset",
       title = "Bins")
hist2 <- ggplot(mpg, aes(cty)) +
  geom_histogram(binwidth =5, fill = "lightblue") +
  labs(x = "Fuel economy in the city",
       y = "Frequency",
       caption = "mpg dataset",
       title = "Binwidth")
hist3 <- ggplot(mpg, aes(cty)) +
  geom_histogram(breaks = c(5, 10, 15, 20, 25, 30, 35, 40), fill = "lightcoral") +
  labs(x = "Fuel economy in the city",
       y = "Frequency",
       caption = "mpg dataset",
       title = "Breaks")
grid.arrange(hist1, hist2, hist3, ncol = 1)

# this creates a boxplot. varwidth = TRUE will make width of the box proportional to number of data points. notch = TRUE will create anotch around the median to show its confidence interval.
ggplot(mpg, aes(class, cty)) +
  geom_boxplot(varwidth = TRUE, fill = "plum") +
     labs(title = "Fuel economy in city grouped by Class of vehicle",
          caption = "mpg dataset",
          x = "Class of Vehicle",
          y = "City Mileage")

# this creates a violin plot of the above
ggplot(mpg, aes(class, cty)) +
  geom_violin(fill = "plum") +
  labs(title = "Fuel economy in city grouped by Class of vehicle",
       caption = "mpg dataset",
       x = "Class of Vehicle",
       y = "City Mileage")




