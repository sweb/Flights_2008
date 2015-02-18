require(dplyr)
#require(lubridate)
#require(ggplot2)
require(Amelia)

setwd("C:/dev/repositories/R/Flights_2008")

raw.data <- read.csv("2008.csv", header = TRUE, sep = ",", nrow = 1000000, as.is = FALSE)

#missmap(raw.data, main="Flights 2008 data - Missings Map", 
#        col=c("yellow", "black"), legend=FALSE)

#raw.data <- raw.data %>% mutate(ArrDelay = ifelse(is.na(ArrDelay), 0, ArrDelay),
#                                DepDelay = ifelse(is.na(DepDelay), 0, DepDelay))

#raw.data <- raw.data %>% filter(!is.na(ArrDelay), !is.na(DepDelay))

u_dest <- mutate(unique(select(raw.data, Dest)), comp = as.character(Dest)) %>% select(comp)
u_origin <- mutate(unique(select(raw.data, Origin)), comp = as.character(Origin)) %>% select(comp)

dplyr::setdiff(u_dest, u_origin)


ex2.data <- raw.data %>% 
  filter(!is.na(ArrDelay), !is.na(DepDelay)) %>%
  mutate(origin_delay = ArrDelay > 0, dest_delay = DepDelay > 0)

ex2.originData <- ex2.data %>%
  select(airport = Origin, delay = origin_delay)

ex2.destData <- ex2.data %>%
  select(airport = Dest, delay = dest_delay)

ex2.airports <- rbind(ex2.originData, ex2.destData)

ex2.airports %>% 
  group_by(airport) %>%
  summarize(sumOfDelays = sum(delay), flights = n()) %>%
  filter(flights > 10000) %>%
  mutate(probOfDelay = sumOfDelays / flights) %>%
  arrange(probOfDelay)


ex3.data <- raw.data %>% 
  filter(!is.na(ArrDelay), !is.na(DepDelay)) %>%
  select(DayOfWeek, DepTime, UniqueCarrier, ArrDelay, DepDelay) %>%
  mutate(delay = ArrDelay > 0 | DepDelay > 0,
         isWeekday = DayOfWeek %in% 1:5,
         isWeekend = DayOfWeek %in% 6:7,
         isDayTime = DepTime %in% 501:1700, 
         isNightTime = DepTime %in% 1701:2400,
         isRedEye = DepTime %in% 0:500)

ex3.data %>%
  filter(isDayTime, isWeekday) %>%
  group_by(UniqueCarrier) %>%
  summarize(probOfDelay = mean(delay))

ex3a.data <- ex2.data %>%
  select(origin_delay, dest_delay, DayOfWeek, DepTime, UniqueCarrier) %>%
  mutate(isWeekday = DayOfWeek %in% 1:5,
         isWeekend = DayOfWeek %in% 6:7,
         isDayTime = DepTime %in% 501:1700, 
         isNightTime = DepTime %in% 1701:2400,
         isRedEye = DepTime %in% 0:500)

ex3a.originData <- ex3a.data %>%
  select(UniqueCarrier, delay = origin_delay, isWeekday, 
         isWeekend, isDayTime, isNightTime, isRedEye)

ex3a.destData <- ex3a.data %>%
  select(UniqueCarrier, delay = dest_delay, isWeekday, 
         isWeekend, isDayTime, isNightTime, isRedEye)

ex3a.airports <- rbind(ex3a.originData, ex3a.destData)

ex3a.airports %>%
  filter(isDayTime, isWeekday) %>%
  group_by(UniqueCarrier) %>%
  summarize(probOfDelay = mean(delay))