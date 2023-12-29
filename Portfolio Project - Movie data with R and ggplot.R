#import ggplot
install.packages('ggplot')
library(ggplot2)

#Get data
setwd("C:\\Users\\MSI\\Documents\\R Scripts\\Udemy Course Downloads")
list.files()

movies <- read.csv("Section6-Homework-Data.csv", stringsAsFactors = T)
head(movies)

#Set Column Names
colnames(movies) <- c("Day.Of.Week","Director","Genre","Movie.Title","Release.Date","Studio","Adjusted.Gross.Millions","Budget.Millions","Gross.Millions","IMDB.Rating","MovieLens.Rating","Overseas.Millions","Overseas.Percentage","Profit.Millions","Profit.Percentage","Runtime.Minutes","US.Millions","US.Gross.Percentage")

#Exploring Data
head(movies)
tail(movies)
str(movies)
summary(movies)
nrow(movies)

#Change numbers from factor to numeric value
movies$Profit.Millions <- as.numeric(as.character(movies$Profit.Millions))
movies$Overseas.Millions <- as.numeric(as.character(movies$Overseas.Millions))
movies$Gross.Millions <- as.numeric(as.character(movies$Gross.Millions))
movies$Adjusted.Gross.Millions <- as.numeric(as.character(movies$Adjusted.Gross.Millions))

#Filter 6 largest studios

filt1 <- movies$Studio %in% c("Buena Vista Studios", "WB", "Fox", "Sony", "Paramount Pictures", "Universal")
studiofilter <- movies[filt1,]
nrow(studiofilter)

#Create object with basic ggplot functionality
z <- ggplot(data=movies, 
            aes(x=Genre,
                y=US.Gross.Percentage))

#--------Compare IMDB Ratings, Profitability and Budget per studio

#add geometries
x <- z + 
  geom_point(data=studiofilter,
             aes(x=IMDB.Rating,
                 y=Budget.Millions,
                 colour=Studio,
                 size=Profit.Millions))

#add non-data ink
x <- x +
  xlab("IMDB Rating") + 
  ylab("$ Millions in US") +
  ggtitle("IMDB rating vs $ revenue in Millions in US")

#add theme
x <- x +
  theme(text = element_text(family="Courier"),
        axis.title.x = element_text(colour="Black",size=18),
        axis.title.y = element_text(colour="Black",size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        
        plot.title = element_text(hjust = 0.5, size=18,))

#Change label in legend
x$labels$size <- "$ Profit in Millions"
x

#--------Find out overseas sales percentage of studios
#Geometries
a <- ggplot(data=studiofilter,
            aes(x=Overseas.Percentage,
                colour=Studio))

#add non-data ink
a <- a +
  xlab("Studio") + 
  ylab("Overseas %") +
  ggtitle("Overseas % per Studio")

#add theme
a <- a +
  theme(text = element_text(family="Courier"),
        axis.title.x = element_text(colour="Black",size=14),
        axis.title.y = element_text(colour="Black",size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        
        plot.title = element_text(hjust = 0.5, size=18,))

#In a Boxplot
a + geom_boxplot(aes(x=Studio,
                     y=Overseas.Percentage))             

#------Do the Same but in a Histogram

#Set (smaller) labels for facets
StudioNames <- c(
  'Buena Vista Studios'="BVS",
  'Fox'="Fox",
  'Paramount Pictures'="Paramount",
  'Sony'="Sony",
  'Universal'="Universal",
  'WB'="WB"
)

#add non-data ink
b <- a +
  xlab("Overseas %") + 
  ylab("Number of Movies") +
  ggtitle("Overseas % per Studio")

#add theme
b <- b +
  theme(text = element_text(family="Courier"),
        axis.title.x = element_text(colour="Black",size=18),
        axis.title.y = element_text(colour="Black",size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        
        plot.title = element_text(hjust = 0.5, size=18,))

b + geom_histogram(binwidth=5) +
  facet_grid(Studio~.,labeller = as_labeller(StudioNames))

#-----Plotting the best value vs best rating Directors
str(movies)

w <- ggplot(data=studiofilter, 
            aes(x=Profit.Millions))
w <- w + 
  
w
rm(w)

#-----Finding Best Directors with 4 or more movies made using dplyr

#install dplyr

install.packages('dplyr')
library(dplyr)

#Checking out what data we have

glimpse(movies)
View(movies)

#See if there is any correlation between IMDB and MovieLens ratings
#by Director with 7+ movies made
movies %>%
  select(Director, IMDB.Rating, MovieLens.Rating, Budget.Millions) %>%
  group_by(Director) %>%
  filter(n() > 6) %>%
  ggplot(aes(x=IMDB.Rating,
             y=MovieLens.Rating,
             colour=Director,
             size=Budget.Millions)) +
  geom_point()

movies %>%
  select(Director, IMDB.Rating, MovieLens.Rating) %>%
  group_by(Director) %>%
  summarise(IMDB.avg = mean(IMDB.Rating)) %>%
  arrange(desc(IMDB.avg)) %>%
  ggplot(aes(x=IMDB.avg)) +
  geom_histogram(binwidth=0.25, colour="Black", fill="Blue")

movies %>%
  select(Director, IMDB.Rating, MovieLens.Rating) %>%
  group_by(Director) %>%
  summarise(IMDB.avg = mean(IMDB.Rating)) %>%
  arrange(desc(IMDB.avg)) %>%
  ggplot(aes(x=IMDB.avg)) +
  geom_histogram(binwidth=0.25, colour="Black", fill="Blue")

movies$Release.Date <- as.Date(movies$Release.Date)

#----- Plotting data for top 3 studios
#Create a vector with top 3 studios by Profit

top_3_studios <- movies %>%
  select(Studio, Profit.Millions) %>%
  filter(!is.na(Studio)) %>%
  aggregate(Profit.Millions ~ Studio, sum) %>%
  arrange(desc(Profit.Millions)) %>%
  top_n(3) %>%
  pull(Studio)

#Margin change over the years for Top 3 Studios
movies %>%
  select(Release.Date, Budget.Millions, Studio, Profit.Millions) %>%
  mutate(date = dmy(Release.Date)) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  filter(Studio %in% top_3_studios) %>%
  filter(year > "1984-01-01") %>%
  group_by(Studio) %>%
  mutate(Margin.Millions = Profit.Millions - Budget.Millions) %>%
  arrange(desc(Margin.Millions)) %>%
  ggplot(aes(x=year,
             y=Margin.Millions,
             colour=Studio,
             size=Budget.Millions)) +
  geom_point() + 
  geom_smooth()

#Profit per movie for a single Studio (Fox)
movies %>%
  select(Release.Date, Budget.Millions, Studio, Profit.Millions) %>%
  mutate(date = dmy(Release.Date)) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  filter(Studio == "Fox") %>%
  filter(year > "1984-01-01") %>%
  ggplot(aes(x=year,
           y=Profit.Millions,
           size=Budget.Millions)) +
  geom_point()
  
#Budget Change over the years for top 3 studios.
budget.change <- movies %>%
  select(Release.Date, Budget.Millions, Studio) %>%
  mutate(date = dmy(Release.Date)) %>%
  mutate(year = floor_date(date, unit = "year")) %>%
  filter(Studio %in% top_3_studios) %>%
  filter(year > "1984-01-01") %>%
  ggplot(aes(x=year,
             y=Budget.Millions,
             colour=Studio)) +
  geom_point() +
  geom_smooth() +
  
  xlab("Year") + 
  ylab("Budget in Millions") +
  ggtitle("Budget per Movie from 1985 to 2020") +

  theme(text = element_text(family="Courier"),
        axis.title.x = element_text(colour="Black",size=18),
        axis.title.y = element_text(colour="Black",size=18),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        
        legend.title = element_text(size=10),
        legend.text = element_text(size=8),
        
        plot.title = element_text(hjust = 0.5, size=18,))
