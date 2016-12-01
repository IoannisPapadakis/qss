##' ---
##' title: "Mapping US Presidential Elections"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
pacman::p_load(animation, gsheet, maps)

##' Draw outline maps
map(database = "world")  # World map
map(database = "usa")  # US map
map(database = "state")  # US map with state borders
map(database = "state", region = "California")  # State map of California

##' Load the 2008 US presidential election vote share data
pres08<- read.csv("pres08.csv")
head(pres08)

## Two-party vote share (Democrats and REpublicans)
pres08$dem.vote <- pres08$Obama / (pres08$Obama + pres08$McCain)
pres08$rep.vote <- pres08$McCain / (pres08$Obama + pres08$McCain)

mean(pres08$dem.vote)
mean(pres08$rep.vote)

##' 2008 US presidential election map
map(database = "state")
for (i in 1:nrow(pres08)) {
  if ((pres08$state[i] != "HI") & (pres08$state[i] != "AK") &
      (pres08$state[i] != "DC")) {
      map(database = "state", regions = pres08$state.name[i],
        col = ifelse(pres08$rep.vote[i] > pres08$dem.vote[i], "red", "blue"),
        fill = TRUE, add = TRUE)
  }
}
title("2008 US Election Map")

##' 2016 US presidential election map
## Load the Google Sheet using the 'gsheet' package
url <- "https://docs.google.com/spreadsheets/d/133Eb4qQmOxNvtesw2hdVns073R68EZx4SfCnP4IGQf8"  # The Google Sheet URL
cook2016 <- gsheet2text(url)  # Function to download the Google Sheet
pres16 <- read.csv(text=cook2016, skip = 4, na.strings="*", header = TRUE)  # Read the data into R, skipping the first 4 rows
head(pres16)  # Check the data

## Tidy the data
bad.rows <- c(1:6, 20:21)  # Identify rows with extraneous info.
pres16 <- pres16[-bad.rows, ]  # Remove bad rows
pres16$State <- gsub("[*]", "", pres16$State)  # Remove the asterisk by the state names
head(pres16, n = 20)

## Calculate 2-party vote share
pres16$dem.vote <- as.numeric(gsub(",", "", pres16$`Clinton..D.`)) / 
    as.numeric(gsub(",", "", pres16$`Total..16.Votes`))
pres16$rep.vote <- as.numeric(gsub(",", "", pres16$`Trump..R.`)) / 
    as.numeric(gsub(",", "", pres16$`Total..16.Votes`))

## Create the map
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


##' Animate US presidential election maps from 1960 to 2012 (BY STATE)
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

## Function to make the US presidential election maps by STATE
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

## Print all election maps by year
for (i in seq(1960, 2012, 4)) {
    election.map(election.total, i)
}

## Animate the map
par(cex = 1.5)
saveHTML({
    for (i in seq(1960, 2012, 4)) {
        election.map(election.total, i)
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
