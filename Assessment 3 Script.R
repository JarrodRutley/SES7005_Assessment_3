#load in packages and load in the data set
library(tidyverse)
library(plotly)
library(ggraph)
library(viridis)

RawData <- read.csv("Assessment3_SpatiotemporalDataset2.csv")

# View the structure of the data set
str(RawData)

# Remove NA's form data set
MatchData <- na.omit(RawData)

# Find the total count for variables
TableData <- MatchData %>%
  group_by(Action, Location) %>%
  mutate(Total = n())

SummaryStats <- MatchData %>% 
  select(Game, Action, Location, Quarter) %>% 
  group_by(Location)



HandballData <- MatchData %>% 
  filter(Action == "Handball Effective" | Action == "Handball Ineffective" | Action == "Handball Clanger")

HBD <- ggplot(HandballData, aes(x= Game, y = Quarter)) +
  geom_jitter(aes(colour = Action), size = 2) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_colour_viridis_d()


ggplotly(HBD)



KickingData <- MatchData %>% 
  filter(Action == "Kick Backwards" | Action == "Kick Clanger" | Action == "Kick Ineffective" | Action == "Kick Inside 50" | Action == "Kick Short" | Action == "Kick Long")

KickData <- ggplot(KickingData, aes(x= Game, y = Location)) +
  geom_jitter(aes(colour = Action), size = 2) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.y=element_blank()) +
  scale_colour_viridis_d() 


ggplotly(KickData)






PlotTest <-  ggplot(data = SummaryStats) +
  geom_bar(mapping = aes(x = Quarter, fill = Location)) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  labs(y = "Total")

ggplotly(PlotTest)


-------------------------------------------------------------------------------------

# View Total Performance Indicators on top of Bars - DOESN'T WORK
PerInData <- ggplot(data = MatchData) +
  geom_bar(mapping = aes(x = Action, fill = Action)) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_text(aes(x = Action, y = " ", label = "Total")) +
  theme(legend.position = "bottom") +
  position = ("dodge") +
  labs(y = "Total")

ggplotly(PerInData)


ggplot(data = MatchData) + geom_bar(mapping = aes(x = Quarter, fill = Action), position = "dodge")


----------------------------------------------------------------------------------------------------



# Visualise the Notional Data data - Histogram - MAYBE
MatchPlot <- ggplot(data = SummaryStats, mapping = aes(x = Quarter, fill = Action)) +
  geom_histogram(binwidth = 0.3) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "bottom") +
  facet_wrap(~ Location)

ggplotly(MatchPlot)


# Count of Instances Quarter x Location - KEEP
Quarter_Location <- MatchData %>%
  count(Game, Location, Quarter) %>%
  ggplot(aes(x = Quarter, y = Location)) +
  theme_classic() +
  theme(axis.title.y = element_blank()) +
  geom_tile(aes(fill = n)) + 
  scale_colour_viridis_d() +
  facet_wrap(~ Game)

ggplotly(Quarter_Location)



# Look at Acceleration over Time (Minuets) - NO
TimePlot <- ggplot(data = MatchData, mapping = aes(x = Time_Minutes, y = Acceleration)) +
  geom_line(aes(color = Quarter) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position = "bottom"))

ggplotly(TimePlot)


# Plot Total Actions by Field Location - MAYBE
ggplot(data = MatchData) +
  geom_histogram(mapping = aes(x = Quarter, fill = Location)) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  labs(y = "Total") +
  facet_wrap(~ Game) 


# Point Plot Total Actions by Game - NO
ggplot() +
  geom_point(data = TableData, aes(x = Longitude , y = Latitude, colour = Action))

# lINE Plot Total Actions by Game - NO
TableData %>% 
  ggplot(aes(Total, Game, group = Action, colour = Action)) +
  geom_line(alpha = 4 / 8) 


ggplot(data = MatchData) + 
  geom_density(mapping = aes(x = Game, color = "Action"), binwidth = 1000, boundry = 0)






































