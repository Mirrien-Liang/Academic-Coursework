load("translink.RData")

library(jsonlite)
library(twitteR)
library(stringr)
library(tidyverse)
library(lubridate)


# First, we want to tidy data so that the function can run faster

# Extract post content and date/time
time=c()
tweet=c()

for (i in seq_along(data)) {
  time[i]=as.character(data[[i]]$created)
  tweet[i]=data[[i]]$text
}

f=data.frame(Time = ymd_hms(time), Post = tweet) # convert strings to date/time
# head(f)

# Remove post rows irrelevant to bus routes
# Note that: adding a new line to the following criterion within the quotation
# will cause error because the new line "\n" will be also considered as part
# of the criteria. Therefore, I will list the criterion I selected as follows:

# Skytrain|SkyTrain|StationAlert|Elevator|elevator|Expo|WCE|West Coast Express
# |time|Time|SeaBus|Compass|Handy|morning|Morning|evening|desk|Desk|Transit|
# transit|Good|Rehab| night |tonight|will be|changes|Congrats|Pattullo Bridge|
# Valentines|year|Year|multiple

# Plus that based on the above pattern, there should be precisely 701
# observations in total. Space and case of letter do matter! 
df <- f[!grepl("Skytrain|SkyTrain|StationAlert|Elevator|elevator|Expo|WCE|West Coast Express|time|Time|SeaBus|Compass|Handy|morning|Morning|evening|desk|Desk|Transit|transit|Good|Rehab| night |tonight|will be|changes|Congrats|Pattullo Bridge|Valentines|year|Year|multiple",f$Post),]




# Remove url substrings that can cause confusion

pattern = "(.*) (.*)" #Greedy match to the last space
for (i in seq_along(df$Post)) {
  if (str_detect(df$Post[i],"http")) {
    df$Post[i] <- str_replace(df$Post[i],pattern,"\\1")
  }
}


# Extract all types of numbers (route#, road/street#, time, date, duration):

# Consider the following cases (in order):
# 1. (Optional)    Start with the letter "Hwy", e.g., "Hwy 9" (only a few cases)
# 2. (Optional)    Routes start with an upper letter, e.g., "R5", "N9"
# 3. (Optional)    Road names that contain only numbers but are always included
#                   in sentences followed "Regular route", e.g., "Regular route
#                   to 96 and 116, then 116, 75A, Scott Rd"
# 4. 1 to 3 digits, e.g., "9", "99", "240"
# 5. (Optional)    to determine street names and time, find numbers that satisfy
#                   the above conditions and followed by a space or a colon with
#                   some digits and patterns as shown below. E.g., "5:10",
#                   "10:45am", "3pm", "3 PM", "49 Ave", "49th", "30 minutes",
#                   "on Sunday, February 9", "on Wed Feb 19" and truncated road
#                   names that end with dots "Regular route to Granville &amp;
#                   Davie, Pacific, Cambie, Nelson, Cambie Bridge, 6 Ave, 4¡­"



# Note that by including strings and symbols like "Regular route", "Feb", "am",
# ":", "Ave", we are able to later distinguish bus routes from other types of
# numbers.

# And similarly, the following pattern/regexp should not be split into multiple
# lines in an effort to avoid errors. I will list it in the comment for review:

# ([A-Z]|Hwy\\s|Regular.*|Jan\\s|Feb\\s|Feburary\\s|Mar\\s)?  # Possible prefix
# (\\d){1,3}  # 1 to 3 digits
# (\\s|:\\d{1,})? # Possible suffix
# (Ave|St |St,|St¡­|Rd|th|nd|rd|pm|PM|am|AM|minutes|¡­)?" # Possible suffix


df1 <- mutate(df,Numbers = str_extract_all(Post,"([A-Z]|Hwy\\s|Regular.*|Jan\\s|Feb\\s|Feburary\\s|Mar\\s)?(\\d){1,3}(\\s|:\\d{1,})?(Ave|St |St,|St¡­|Rd|th|nd|rd|pm|PM|am|AM|minutes|¡­)?"))






# Now that we have all numbers extracted, we need to further extract only the
# bus routes by pattern that "optionally starts with one upper case letter,
# followed by 1 to 3 digits, and optionally ends with a space"
# While doing so, we also omit rows that contains no bus route info, e.g.,
# missing bus routes
# The intermediate "Numbers" column is also removed for concision
# Store the result in a new data frame for error detection
for (i in seq_along(df1$Numbers)){
  df1$Routes[[i]] <- na.omit(str_extract(df1$Numbers[[i]],"^[A-Z]?\\d{1,3}\\s?$"))
}

df2 <- df1 %>% filter(!(Routes=="character(0)")) %>%
  select(-Numbers)
# At this point, we should have a total of 697 valid observations (posts).


# Now that we have a tidy data frame with Date/Time, Post, and each
# corresponding bus route number(s), we want to determine how a post indicates
# START and which STOP. To do so, we want to mutate a new column "Status" to
# mark the indication of each post.
# Note that the Status is binary: either START (TRUE) or STOP (FALSE).

df2 <- mutate(df2, Status = str_detect(Post,"^(?!.*( clear|Clear|CLEAR| end| back| over)).*(regular route|Regular route|onward|detour|experienc|suspend)"))

# Note that in some cases, the post was truncated and failed to show whether
# a disruption occurs or not. In these cases, the default status will be FALSE,
# which is an approximation of the status.

# For example, in row 32, "#RiderAlert 403 Bridgeport Station/Three Rd/ 407
# Bridgeport Station/ Gilbert/ 430 Brighouse Station/ Metrotown Stat¡­" will be 
# considered as FALSE.
