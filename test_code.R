library(tidyverse)
library(ggplot2)
library(stringr)
library(purrr)
library(lubridate)

## Data Processing:

df <- read_csv('data/repdata%2Fdata%2FStormData.csv') # Read in data
dim(df) # Print size of dataframe
dim(df)[1] * dim(df)[2] # Print number of data points
colnames(df) # Print column names - link here to codebook...
sum(is.na(df)) # Print number of na values
(sum(is.na(df)) / (dim(df)[1] * dim(df)[2])) * 100 # Print % NA values in data


# Check the events (EVTYPE) column:
sum(is.na(df$EVTYPE)) # No missing data
length(unique(df$EVTYPE)) # 977 event types !!! There are only ~40 in the codebook
head(df$EVTYPE, 20) # All the early entries are 'TORNADO'?

# Lets filter by year and have a look at how the event types are distributed
x <- df %>%
        group_by(year = year(BGN_DATE)) %>%
        summarise(events = length(unique(EVTYPE)))

ggplot(x, aes(x = year, y = events)) +
        geom_line()

# There seems to be a sudden peak in the 1990s, before that only a few were recorded.

# In 1990 how many event types were recorded?
length(unique(df$EVTYPE[year(df$BGN_DATE) == 1990]))

# Only 3, what were they?
(unique(df$EVTYPE[year(df$BGN_DATE) == 1990]))

# It seems as though the recording of event type only really picked up around 1993
length(unique(df$EVTYPE[year(df$BGN_DATE) == 1993]))

# As we are looking to compare different types of event it is not necessary to include years where only a few types of event where recorded. Exclude all dates before 1993.
df1 <- df %>%
        filter(year(BGN_DATE) > 1993)

# Cuts 20000 rows from the data set

x <- df1 %>%
        group_by(year = year(BGN_DATE)) %>%
        summarise(events = length(unique(EVTYPE)))

ggplot(x, aes(x = year, y = events)) +
        geom_line()



## Results::

# What type of event is most harmful with respect to population health?

# Across the United States, which types of events have the greatest economic consequences?
