library(stringr)
library(tidyverse)

load("translink.RData")

# View(data)

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
pattern_non_bus = "Skytrain|SkyTrain|StationAlert|Elevator|elevator|Expo|WCE|West Coast Express|time|Time|SeaBus|Compass|Handy|morning|Morning|evening|desk|Desk|Transit|transit|Good|Rehab| night |tonight|will be|changes|Congrats|Pattullo Bridge|Valentines|year|Year|multi"

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
pattern_number = "([A-Z]|Hwy\\s|Regular.*|Jan\\s|Feb\\s|\\February\\s|Mar\\s)?(\\d){1,3}(\\s|:\\d{1,})?(Ave|St |St,|St/|St…|Rd|th|st|nd|rd|pm|PM|am|AM|min|minutes|due|Station|…)?"
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
pattern_status = "^(?!.*( clear|Clear|CLEAR| ended| back| over|cancel|return)).*(regular route|Regular route|onward|detour|experienc|suspend)"
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
tail(df2$Post,n=3)
translink(2020, 1, 26, 3)$start
translink(2020, 1, 26, 3)$stop

# Test 2: Pass
df2$Post[213:218]
translink(2020,2,13,17)

# Test 3: Pass
df2$Post[264:272]
translink(2020,2,12,19)

# Test4 : Pass
df2$Post[207]
translink(2020,2,14,13)

# Test 5: Pass
df2$Post[285:290]
translink(2020,2,12,6)

