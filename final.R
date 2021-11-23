#Loading libraries
library(dplyr)
library(tidyverse)
library(markdown)
library(lubridate)


#importing files 1. Netflix title data from kagle
#2. Netflix subscriber summary from businessofapps.com

netflixdf_raw <- data.frame(read.csv('netflix_titles.csv'))
netflixSubs <- data.frame(read.csv('netflix_subs.csv'))
multiplier <- data.frame(read.csv('month.csv'))

colnames(netflixSubs) <- c("Year", "Subscribers")
colnames(multiplier) <- c("Month", "multiplier")
print(colnames(netflixSubs))
head(netflixdf_raw)

#Removing some unwanted columns 
netflix_clean <- netflixdf_raw %>% select(-c(director, cast, description ))
which(is.empty(netflix_clean$duration))

#validating that none of the entries are duplicated
glands <- netflix_clean[duplicated(netflix_clean$title)]


#verifying the unique values in the type column
type_Unique <- unique(netflix_clean$type)
print(type_Unique)
print(length(unique(netflix_clean$type)== "TV Show"))

#the shows column only has two entries Movies and TV Show


############################################################
#Extending df by seperating the durations and category columns
netflix_clean_time_pivoted <- netflix_clean %>% separate(col = duration,
                                                         into = c('duration', 'metric'),
                                                         sep= ' ') %>% separate(col = listed_in,
                                                                                into = c('Category', 'Cat2', 'Cat3'),
                                                                                sep= ',') %>% mutate(upload_date = date_added)

netflix_clean_time_pivoted <- netflix_clean_time_pivoted %>% separate(col = upload_date,
                                                                      into = c('Month', 'Day', 'Year'),
                                                                      sep= ' ')
############################################################
#Coercing timeline column in integers for easy of mathematical operations
netflix_clean_time_pivoted$timeline <- as.integer(netflix_clean_time_pivoted$duration) 
netflix_clean_time_pivoted$Year <- as.integer(netflix_clean_time_pivoted$Year)
netflix_clean_time_pivoted$Month <-  factor(netflix_clean_time_pivoted$Month, levels = month.name)
multiplier$Month<-  factor(multiplier$Month, levels = month.name)
#Received an error that 3 cells where empty

head(netflix_clean_time_pivoted)

############################################################
#Manipulating the df to extract a movies sub set
netflix_movies <- netflix_clean_time_pivoted %>% filter(type == 'Movie') %>% mutate(runtime = timeline,
                                                                                    low_fi = timeline * (0.3/60),
                                                                                    med_fi = timeline * (0.7/60),
                                                                                    hi_fi = timeline * (3/60),
                                                                                    ultrahi_fi =timeline * (7/60),
                                                                                    tot_load = low_fi + med_fi + hi_fi + ultrahi_fi,
                                                                                    storage_cost = tot_load * 0.021)

#printing the colnames to determine which cols to subset                                                                                       
print(colnames(netflix_movies))

netflix_Movies <- subset(netflix_movies, select = c(date_added, 
                                                    Category,
                                                    type,
                                                    rating, 
                                                    runtime,
                                                    low_fi, 
                                                    med_fi,         
                                                    hi_fi,
                                                    ultrahi_fi,
                                                    tot_load,
                                                    Month,
                                                    Year,
                                                    storage_cost))

############################################################
#Manipulating the df to extract a tv show subset

#Using a 7 episode season and a 42min duration per episode,
#For streaming purposes content is saved in 3 formats
#low_fi/low fidelity holds a storage space of 0.3GB/hr of content,
#med_fi/medium fidelity holds a storage space of  0.7GB/hr of content,
#hi_fi/ High fidelity holds a storage space of 3GB/hr of content
#ultrahi_fi/Ultra High fidelity holds a storage space of 7GB/hr of content
#the data will be modified to display these assumptions
#Basic intelligent store with AWS costs $0.021/Gb per month
#reference:https://www.thrillist.com/entertainment/nation/netflix-episode-length-streaming-services-traditional-tv

netflix_TvShows <- netflix_clean_time_pivoted %>% filter(type == 'TV Show') %>% mutate(episode_length = c(rep(42, times = 2676)),
                                                                                       episode_nos = c(rep(7, times = 2676)),
                                                                                       runtime = episode_length * episode_nos * timeline,
                                                                                       low_fi = episode_length * episode_nos * timeline * (0.3/60),
                                                                                       med_fi = episode_length * episode_nos * timeline * (0.7/60),
                                                                                       hi_fi = episode_length * episode_nos * timeline * (3/60),
                                                                                       ultrahi_fi = episode_length * episode_nos * timeline * (7/60),
                                                                                       tot_load = low_fi + med_fi + hi_fi + ultrahi_fi,
                                                                                       storage_cost = tot_load * 0.021)

#printing the colnames to determine which cols to subset                                                                                       
print(colnames(netflix_TvShows))

#Final dataframe to be visualized
netflix_tv <- subset(netflix_TvShows, select = c(date_added, 
                                                 Category,
                                                 type,
                                                 rating, 
                                                 runtime,  
                                                 low_fi, 
                                                 med_fi,         
                                                 hi_fi,
                                                 ultrahi_fi,
                                                 tot_load,
                                                 Month,
                                                 Year,
                                                 storage_cost))
#Subseting the data to remove other unwanted columns


############################################################
#Combining the modified tables
netflixClean4Export <- rbind(netflix_Movies, netflix_tv)


############################################################
#Combining the modified netflix data with subscriber df

dfnetflix <- inner_join(netflixClean4Export, netflixSubs,by="Year")
dfnetflix <- inner_join(dfnetflix, multiplier,by="Month")
summary(dfnetflix)

dfnetflix$tot_cost <- dfnetflix$storage_cost * dfnetflix$multiplier

#dropping all rows with na
dfnetflix <- dfnetflix %>% drop_na()
summary(dfnetflix)

netflixExport <- subset(dfnetflix, select = c(date_added, 
                                              Category,
                                              type,
                                              rating,
                                              runtime,  
                                              low_fi, 
                                              med_fi, 
                                              hi_fi,
                                              ultrahi_fi,
                                              tot_load,  
                                              Month,  
                                              Year,  
                                              Subscribers,
                                              tot_cost,
                                              multiplier))
netflixExport$date_added <- mdy(netflixExport$date_added)

ordnetflix <- netflixExport[order(netflixExport$date_added), ]

ordnetflix$cumsum_storage_cost <- cumsum(ordnetflix$tot_cost)
ordnetflix$cumsum_storage <- cumsum(ordnetflix$tot_load)




write.csv(ordnetflix, "netflixMod.csv", row.names = FALSE)
cat("The data frame is exported", "\n")


library(reticulate)
repl_python()

