# a5 bonus question

#################################################

library(tidyverse)
library(rtweet)
library(twitteR)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
library(stringr)
source('access.r')



data = userTimeline(
  'Translink',
  n = 3200,
  includeRts = F,
  excludeReplies = T
)

data[[1]]$text
data[[1]]$created

save(file = 'translink.data', data)

load('translink.data')

# empty vector
time=c() 
tweet=c()

# extract date and post from data
for (i in seq_along(data)) {
  time[i]=as.character(data[[i]]$created)
  tweet[i]=data[[i]]$text
}

# create a new data frame for data manipulation
f=data.frame(Time = time, Post = tweet)

# Track results
# View(f)

# Pattern to find posts irrelevant to bus routes
# It's too long that I have to cut it and use str_c to combine strings
pattern_non_bus = str_c("Skytrain|SkyTrain|StationAlert|Elevator|elevator|",
                        "Expo|WCE|West Coast Express|board|time|Time|SeaBus|",
                        "Compass|Handy|morning|Morning|evening|desk|Desk|",
                        "Transit|transit|Good|Rehab| night |tonight| call|",
                        "will be|changes|Congrats|Pattullo Bridge|Valentines|",
                        "year|Year|multi")

# Exclude these posts
df <- f[!grepl(pattern_non_bus,f$Post),]

# Track results
# View(df)

# Delete all url from posts
pattern_url = "(.*) (.*)" #Greedy match to the last space
for (i in seq_along(df$Post)) {
  if (str_detect(df$Post[i],"http")) {
    df$Post[i] <- str_replace(df$Post[i],pattern_url,"\\1")
  }
}

# Track results
# View(df)

# Extract numbers of all types
# Store resulted data frame into a new one for error detection
pattern_number = str_c("([A-Z]|Bay\\s|Hwy\\s|stop\\s|Regular.*|Jan\\s|Feb\\s|",
                       "Feb.\\s|\\February\\s|Mar\\s)?",
                       "(\\d){1,3}(\\d+|\\s|:\\d+)?",
                       "(Ave|St |St,|St/|St¡­|Rd|th|st|nd|rd|pm|",
                       "PM|am|AM|min|minutes|due|Station|¡­)?")
df1 <- mutate(df,Numbers = str_extract_all(Post,pattern_number))

# Track results
# View(df1)

# Extract only numbers of bus route
# Omit NA rows which are generated when str_extract() fails
pattern_bus_route = "^[A-Z]?\\d{1,3}\\s?$"
for (i in seq_along(df1$Numbers)){
  df1$Routes[[i]] <- na.omit(str_extract(df1$Numbers[[i]],pattern_bus_route)) 
}

# Track results
# View(df1)

# Store resulted data frame into a new one for error detection
# Remove rows where bus routes are missing
# Remove intermediate column "Numbers" for concision
df2 <- df1 %>% filter(!(Routes=="character(0)")) %>%
  select(-Numbers)

# Track results
# View(df2)

# Determine whether a post indicates a start or a stop
pattern_status = str_c("^(?!.*( clear|Clear|CLEAR| ended| back| over|cancel|",
                       "return|ease )).*(regular route|Regular route|onward|",
                       "detour|experienc|suspend|off )")
df2 <- mutate(df2, Status = str_detect(Post,pattern_status))

# Track results
# View(df2)


# Formulate the function translink()

translink <- function(y,m,d,h){
  # take four arguments of time, e.g., 2020,1,26,3
  
  # Create an empty list to store results
  ret = list(start=c(),stop=c())
  
  # format input ymd_h
  # i.e., (2020,1,26,3) -> "2020-01-26 03"
  if(nchar(m)==1){m=str_c("0",m)}
  if(nchar(d)==1){d=str_c("0",d)}
  if(nchar(h)==1){h=str_c("0",h)}
  date = str_c(y,m,d,sep="-")
  datetime = str_c(date,h,sep = " ")
  
  # Match time, then match Status, then combine/append vectors
  for (i in seq_along(df2$Time)) {
    if (str_detect(df2$Time[[i]], datetime)){ # time matched
      if (df2$Status[[i]]) {  # Status == True
        ret$start <- c(ret$start, df2$Routes[[i]]) # Append route# to start
      } else {  # Status == False
        ret$stop <- c(ret$stop, df2$Routes[[i]])  # Append route# to stop
      }
    }
  }
  
  
  # Remove all the Space in the Routes column
  ret$start <- gsub('\\s+', '', ret$start)
  ret$stop <- gsub('\\s+', '', ret$stop)
  
  # Remove duplicates
  ret$start = unique(ret$start)
  ret$stop = unique(ret$stop)
  
  # If no match, append warning message
  if (length(ret$start)==0) {ret$start <- c(ret$start,"No detour had started")}
  if (length(ret$stop)==0) {ret$stop <- c(ret$stop,"No detour had ended")}
  
  return(ret)
}

# Several test cases from general ones to corner ones.


# Test 1: Pass
df2$Post[22:24]
translink(2022,3,2,16)

# Test 2: Pass
df2$Post[53:54]
translink(2022,3,1,13)

# Test 3: Pass with one corner case (will be included in the next section)
df2$Post[586:594]
translink(2022,1,31,16)

# rows such as 593 do not include its status; should have been filtered out
# but the default assignment of status goes "FALSE"


