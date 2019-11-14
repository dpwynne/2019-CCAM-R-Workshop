# R Code for the 11/14/2019 CCAM/BDAC R Workshop

library(ggplot2)
library(readr)
cereal <- read_csv("cereal_calories.csv")
housing <- read_csv("housing.csv")
crime <- read_csv("lacrime2017.csv")


# Slide 13: bar graph
zip_bargraph <- ggplot(data = housing, 
                       mapping = aes(x = as.factor(ZipCode))
) + geom_bar()
print(zip_bargraph)

# Slide 14: bar graph
zip_bargraph2 <- ggplot(data = housing, 
                        mapping = aes(x = as.factor(ZipCode), y = Price)
) + geom_col()
print(zip_bargraph2)

# Slide 15: Cereal avearages
library(dplyr)
cereal_averages <- cereal %>% 
  group_by(Manufacturer) %>%
  summarize(Calories = mean(Calories))

# Slides 16-24: create the barplot
cereal1 <- ggplot(cereal_averages, 
                  aes(x = Manufacturer, y = Calories)) + geom_col()
# Slide 17
cereal1 <- ggplot(cereal_averages, 
                  aes(x = Manufacturer, y = Calories)) + 
  geom_col(aes(fill = Manufacturer))

# Slide 18
cereal1 <- ggplot(cereal_averages, 
                  aes(x = reorder(Manufacturer, Calories),
                      y = Calories)) + 
  geom_col(aes(fill = Manufacturer))

# Slide 19
cereal2 <- cereal1 + labs(x = "", y = "Calories per Serving")

# Slide 20
cereal3 <- cereal2 + ggtitle("Average Calories by Cereal Manufacturer")

# Slide 21
cereal4 <- cereal3 + scale_fill_manual(values = cbPalette[c(1,1,1,7)])

# Slide 23
cereal5 <- cereal4 + guides(fill = FALSE)

# Slide 24
cereal6 <- cereal5 + theme(plot.title = element_text(hjust = 0.5))

# End product of slides 16-24
print(cereal6)

# Slide 27: Scatterplot
sqft_scatter <- ggplot(data = housing, 
                       mapping = aes(x = SqFt, y = Price)
) + geom_jitter()
print(sqft_scatter)

# Slide 28: Line plot
housing.month <- housing %>% group_by(Month) %>% summarize(AvgPrice = mean(Price))
housing_linegraph <- ggplot(data = housing.month, 
                            mapping = aes(x = Month, y = AvgPrice)
) + geom_line()
print(housing_linegraph)

# Slide 29: Line plot setup
print(crime)

# Slides 30-36 : create the line plot
crime1 <- ggplot(crime, 
                 aes(x = month, y = ncrimes)) + geom_line()

# Slide 31
crime2 <- ggplot(crime, aes(x = month, y = ncrimes)) + 
  geom_line(aes(color = `Crime Name`))

# Slide 32
crime3 <- crime2 + theme_bw()

# Slide 33
month.labels <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

crime4 <- crime3 + scale_x_continuous(
  breaks = seq(1,12),
  labels = month.labels) 

# Slide 34
crime5 <- crime4 + theme(panel.grid.minor.x = element_blank())

# Slide 35
crime6 <- crime5 + labs(x = "", y = "Number of Crimes") +
  ggtitle("Crime in 2017 in Los Angeles")

# Slide 36
crime7 <- crime6 + scale_color_manual(name = "",
                            breaks = c("Battery","Vehicle Theft","Burglary", "Identity Theft"),
                            values = cbPalette[c(1,2,3,4)])

# Output of slides 30-36
print(crime7)

# Slide 38: Faceting
crime_facet <- crime1 + 
  facet_wrap(~`Crime Name`)
print(crime_facet)

# Slide 39: Annotations
crime_annotated <- crime7 + 
  annotate(geom = "text",
           x = 1, y = 300,
           label = "Source: LAPD, https://data.lacity.org",
           hjust = 0)
print(crime_annotated)

# Slides 40-41: Fixing Font Size
cereal_fontsize <- cereal5 + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 24)
  )
print(cereal_fontsize)

crime_fontsize <- crime7 + 
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.text = element_text(size = 14)
  )
print(crime_fontsize)
