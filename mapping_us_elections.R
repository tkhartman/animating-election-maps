##' ---
##' title: "Mapping US Presidential Elections, 1960 to 2016"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## Load packages via pacman
pacman::p_load(animation, gsheet, maps)

##' 2016 US presidential election map
## Scrape the most recent data from a Google Sheet using the 'gsheet' package
url <- "https://docs.google.com/spreadsheets/d/133Eb4qQmOxNvtesw2hdVns073R68EZx4SfCnP4IGQf8"  # The Google Sheet URL
cook2016 <- gsheet2text(url)  # Function to download the Google Sheet
pres16 <- read.csv(text=cook2016, skip = 4, header = TRUE)  # Read the data into R, skipping the first 4 rows
head(pres16)  # Check the data

## Tidy the 2016 election data
bad.rows <- c(1:6, 20:21)  # Identify rows with extraneous info
pres16 <- pres16[-bad.rows, ]  # Remove bad rows
pres16$State <- gsub("[*]", "", pres16$State)  # Remove the asterisk by the state names
head(pres16, n = 20)

## Calculate the 2-party vote share for each state
pres16$dem.vote <- as.numeric(gsub(",", "", pres16$`Clinton..D.`)) / 
    as.numeric(gsub(",", "", pres16$`Total..16.Votes`))
pres16$rep.vote <- as.numeric(gsub(",", "", pres16$`Trump..R.`)) / 
    as.numeric(gsub(",", "", pres16$`Total..16.Votes`))

## Create the election map
map(database = "state")
    for (i in 1:nrow(pres16)) {
        if ((pres16$State[i] != "Hawaii") & (pres16$State[i] != "Alaska") &
            (pres16$State[i] != "District of Columbia")) {
                map(database = "state", regions = pres16$State[i],
                col = ifelse(pres16$rep.vote[i] > pres16$dem.vote[i], "red", "blue"),
                fill = TRUE, add = TRUE)
        }
}
title("2016 US Election Map")

##' US presidential election maps from 1960 to 2012, BY STATE (48 contiguous)
## Load the election data (from Imai 2017)
elections <- read.csv("elections.csv")

## Sum county-level votes by state & year
election.rep <- with(elections, aggregate(rep ~ year + state, FUN = "sum"))
election.dem <- with(elections, aggregate(dem ~ year + state, FUN = "sum"))
election.other <- with(elections, aggregate(other ~ year + state, FUN = "sum"))

## Merge the datasets
election.total <- merge(election.rep, election.dem, by = c("year","state"))
election.total <- merge(election.total, election.other, by = c("year", "state"))

## Calculate the 2-party vote share by state & year
election.total$total <- rowSums(election.total[, c("rep", "dem", "other")])
election.total$rep.vote <- election.total$rep / election.total$total
election.total$dem.vote <- election.total$dem / election.total$total

## Function to make the state-level US presidential election maps
## x = data.frame, y = year
election.map <- function(x, y){
  vote.share <- subset(x, year == y)
  map(database = "state")
  for (i in 1:48) {
    map(database = "state", regions = vote.share$state[i],
      col = ifelse(vote.share$rep.vote[i] > vote.share$dem.vote[i], "red", "blue"),
      fill = TRUE, add = TRUE)
  }
   title(main = paste(y, "Election"))
}

## Merge the 2016 election data with previous election data
election.2016 <- subset(pres16, 
                        State != "Alaska" & 
                        State != "District of Columbia" &
                        State!="Hawaii", 
                        select = c("State", "dem.vote", "rep.vote"))
colnames(election.2016)[1] <- "state"  # Change name for consistency across datasets
election.2016$state <- tolower(election.2016$state)  # Make state names lower case
election.2016$state <- as.factor(election.2016$state)
election.2016 <- election.2016[order(election.2016$state), ]  # Sort by state name
election.2016$year <- 2016  # Add the year
election.2016 <- election.2016[ , c(4, 1, 3, 2)] # Reorder the columns

election.total2 <- subset(election.total, 
                          select = c("year", "state", "rep.vote", "dem.vote"))

election.total2 <- rbind(election.total2, election.2016)  # Append the 2016 data

## Print all election maps by year
for (i in seq(1960, 2016, 4)) {
    election.map(election.total2, i)
}

##' Animate US presidential election maps from 1960 to 2016
par(cex = 1.5)
saveHTML({
    for (i in seq(1960, 2016, 4)) {
        election.map(election.total2, i)
    }
}, htmlfile = "mapping_us_elections_by_state.html", outdir = getwd(), autobrowse = FALSE)


##' Animate US presidential election maps from 1960 to 2012 (BY COUNTY)
## Create a character vector of `state,county' inputs
elections$poly <- paste(elections$state, elections$county, sep = ",")

## Calculate county-level two-party vote share
elections$dem.county <- elections$dem / (elections$dem + elections$rep)
elections$rep.county <- elections$rep / (elections$dem + elections$rep)

## Create colors based upon two-party vote share
elections$colors <- rgb(red = elections$rep.county, blue = elections$dem.county, green = 0)
    
par(cex = 1.5)
saveHTML({
  for (i in 1:14) {
    data1 <- subset(elections,
                         subset = (year == 1960 + 4 * (i - 1)))
    states <- unique(data1$state)
    map(database = "state")
    for (j in 1:length(states)) {
      data2 <- subset(data1,
                           subset = (state == states[j]))
      map(database = "county", region = data2$poly,
          col = data2$color, fill = TRUE, add = TRUE)
    }
    title(main = paste("County-level Election Results,",
                       1960 + 4 * (i - 1), "Presidential Election"))
  }
}, htmlfile = "mapping_us_elections_by_county.html", outdir = getwd(), autobrowse = FALSE)
