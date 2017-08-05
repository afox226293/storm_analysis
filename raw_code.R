library(tidyverse)
library(lubridate)
library(magrittr)
library(stringr)
library(ggthemes)
library(ggplot2)


# Read in data
data <- read_csv('data/repdata%2Fdata%2FStormData.csv.bz2')

# Explore
dim(data) # 902297 rows, 37 variables
sum(is.na(data)) # 6.8 million missing values

# Complete initial tidy
# Remove entries from before 1996 as complete event types not recorded
# Select only the relevant columns

data$BGN_DATE <- mdy_hms(data$BGN_DATE) # convert begin time variable to datetime to allow for filtering

data %<>%
        filter(BGN_DATE >= 1996) %<>% # Filter dates
        select(EVTYPE, FATALITIES, INJURIES, PROPDMG:CROPDMGEXP) # Select variables

data$EVTYPE <- tolower(data$EVTYPE)

data$EVTYPE %<>% # Replace some common event type inconsistencies
        str_replace_all('^winter storm.*', 'winter storm') %<>%
        str_replace_all('^thunderstorm.*', 'thunderstorm') %<>%
        str_replace_all('^tstm.*', 'thunderstorm') %<>%
        str_replace_all('^tropical storm.*', 'tropical storm') %<>%
        str_replace_all('^hurricane.*', 'hurricane')

# Create specific data to answer each question
# Question 1: human impact

# Create dataframe of total impact (injuries + fatalities)
human <- data %>%
                group_by(EVTYPE) %>% # Group to event type
                summarise(human_impact = sum(INJURIES) + sum(FATALITIES),
                          `Killed/Injured\nper event` = human_impact / n()) %>% # Create summary total
                arrange(desc(human_impact)) # Arrange by most impactful

human <- head(human, 10) # Select only the top 10

# Plot 
human$EVTYPE <- factor(human$EVTYPE, levels = human$EVTYPE[order(human$human_impact)])
ggplot(human, aes(x = EVTYPE, y = human_impact)) +
        theme_tufte() +
        geom_segment(aes(x = EVTYPE, xend = EVTYPE,
                         y = 0, yend = max(human_impact)),
                     linetype = 3, colour = 'slategrey') +
        geom_point(aes(size = `Killed/Injured\nper event`), colour = 'steelblue') +
        coord_flip() +
        theme(axis.line.x = element_line(size = 0.2)) +
        labs(title = '',
             x = 'Type of Weather Event',
             y = 'Number of Killed or Injured') 
        

# Question 2: Economic impact

# Exclude NA values from data, 

data$PROPDMGEXP <- tolower(data$PROPDMGEXP) # Convert exponents to consistant case
data$CROPDMGEXP <- tolower(data$CROPDMGEXP)

economic <- data %>%
                filter(!is.na(PROPDMGEXP) & !is.na(CROPDMGEXP)) %>%  # Filter NA
                filter(CROPDMGEXP != '?') # Remove innaccurate damage estimates (few in set)

repl <- c('k' = '3', 'm' = '6', 'b' = '9') # Create string to replace exponents
economic$PROPDMGEXP %<>% str_replace_all(repl) # Replace exponent in property damage
economic$PROPDMGEXP <- as.integer(economic$PROPDMGEXP) # Force to integer value
economic$CROPDMGEXP %<>% str_replace_all(repl) # Replace exponent in crop damage
economic$CROPDMGEXP <- as.integer(economic$CROPDMGEXP) # Force to integer value

economic %<>% # Create summary dataframe
        mutate(property = as.numeric(PROPDMG * (10^PROPDMGEXP)), # Total property damage
               crops = as.numeric(CROPDMG * (10^CROPDMGEXP)), # Total crops damage
               damage = property + crops) %<>% # Total damage overall
        group_by(EVTYPE) %<>% 
        summarise(damage = sum(damage) / 1e9,
                  per_event = damage / n()) %<>% # Create summary of damage / event type
        arrange(desc(damage))

economic <- head(economic, 10) # Take top 10 events by damage caused

# Create Plot

economic$EVTYPE <- factor(economic$EVTYPE, levels = economic$EVTYPE[order(economic$damage)])
ggplot(economic, aes(x = EVTYPE, y = damage)) +
        theme_tufte() +
        geom_segment(aes(x = EVTYPE, xend = EVTYPE,
                         y = 0, yend = max(damage)),
                     linetype = 3, colour = 'slategrey') +
        geom_point(aes(size = per_event), colour = 'steelblue') +
        theme(axis.line.x = element_line(size = 0.2)) +
        coord_flip() +
        labs(title = 'For Recorded Data in the United States 1996 - 2016 Flooding has Caused\nSignificantly More Damage by Cost than any Other Weather Type',
             x = 'Type of Weather Event',
             y = 'Damage caused (billion USD)')

