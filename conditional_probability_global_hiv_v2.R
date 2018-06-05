##' ---
##' title: 'Lab Exercise: Conditional Probability and Global HIV'
##' author: "Todd K Hartman"
##' date: "Last updated `r format(Sys.Date())`"
##' output: github_document
##' ---

##' Housekeeping
## Load packages via 'pacman' package manager
pacman::p_load(googleVis, tmap, tmaptools)
  
## Set the working directory
# setwd("ENTER YOUR WORKING DIRECTORY HERE")

##' Download and import the data
## Enter the URL and extract the file name
url.df <- "https://goo.gl/eouHbt"
file.df <- "rawdata_2155.txt"

## Only download the file if it doesn't exist in the working directory
if (!file.exists(file.df))
    download.file(url = url.df, destfile = file.df)

## Import the data (.txt file using fixed widths)
## (Open this file in Excel to determine columun widths)
hiv <- read.fwf(file.df, width = c(7, 51, 4), strip.white = TRUE)  
dimnames(hiv)[[2]] <- c("id", "country", "percent")  # Add variables names
head(hiv)

##' Append the UK Prevalence Data for Comparison
## Convert factor name to string so we can add the UK
hiv$country <- as.character(hiv$country)

## Append UK data to end of dataset (update prevalence if needed)
hiv.rows <- nrow(hiv)
hiv[hiv.rows + 1, ] <- c(hiv.rows + 1, "United Kingdom", .16)

## Check that the data looks OK
hiv[hiv.rows + 1, ]

##' Using Bayes' Rule to Calculate Conditional Probabilities
## Enter the data from the OraQuick test (update if needed)
true.pos <- .9167  # From the test sensitivity
false.neg <- .0833  # Converse rule (1 - true.pos)

true.neg <- .999  # From the test specificity
false.pos <- .001  # Converse rule (1 - true.neg)

## Convert prevalence percentages into proportions 
hiv$prop <- as.numeric(hiv$percent)/100

## Numerator
## Pr(B|A)Pr(A)
numerator <- true.pos * hiv$prop

## Denominator
## Pr(B|A)Pr(A) + Pr(B|A')Pr(A')
denominator <- (true.pos * hiv$prop) + (false.pos * (1-hiv$prop))

## Apply Bayes' Rule
hiv$pr.hiv <- numerator / denominator

## Rank the Countries 
hiv <- hiv[order(-hiv$pr.hiv, hiv$country), ]

## Display probabilities by country
subset(hiv, select = c(country, pr.hiv)) 

##' Visualizing Global HIV Probabilities Given a Positive Test
## Create the interactive figure
bar.plot <- gvisBarChart(hiv, xvar = "country", yvar = "pr.hiv", 
                         options = list(legend = "none",
                                        vAxes = "[{textStyle:{fontSize: '16'}}]",
                                        chartArea = "{left:250,top:10,bottom:10}",
                                        width= 800, height = 3000) )
plot(bar.plot)

## Sort by country (alphabetically)
hiv <- hiv[order(hiv$country), ]

## Load the world shape file
data(World)

## List countries in hiv data where names don't match shape file
bad.names <- which(hiv$country %in% World@data$name == FALSE)
subset(hiv[bad.names, ], select = "country")

## Check country names in shape file
countries <- as.character(World@data$name)
countries

## Make a vector names from the shape file
good.names <- c("Bahamas", "Barbados", "Myanmar",
                "Cape Verde", "Central African Rep.",
                "Dem. Rep. Congo", "Congo", "Dominican Rep.", 
                "Eq. Guinea", "Gambia", "Malta", "S. Sudan")

## Replace 'bad' names with 'good' names
hiv[bad.names, ]$country <- good.names

## Check that the names all match
## (no data for 3 countries even though they appear in the file)
hiv$country %in% World@data$name

## Display countries that don't match
no.match <- which(hiv$country %in% World@data$name == FALSE)
hiv[no.match, ]

## Remove the 3 countries that don't match
match <- which(hiv$country %in% World@data$name == TRUE)
hiv2 <- hiv[match, ]

## Add a new variable called 'name' for merging
hiv2$name <- hiv2$country

## Create a new data.frame for merging
hiv3 <- data.frame(name = countries)

## Merge
hiv3 <- merge(hiv3, subset(hiv2, select = c("name", "pr.hiv")), by = "name", all = TRUE)

## Append hiv data to map shape file
hiv.map <- append_data(World, hiv3, key.shp = "name", key.data = "name")

## Make the map
map1 <- qtm(hiv.map, fill = "pr.hiv", format = "World", 
            style = "gray", text.root = 5, 
            fill.title = "Pr(HIV|Positive Test)", 
            fill.textNA = "No data")
map1
