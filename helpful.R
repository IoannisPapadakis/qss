##' ---
##' title: "Helpful Funcitons in R"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

## Render the file in R Markdown
# rmarkdown::render("Filename.R")

##' Housekeeping
## Load packages via the 'pacman' package manager
pacman::p_load(ggplot2)

## Install the 'crosstab' function
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

## Check and set the working directory
getwd()  # Get the current working directory
# setwd("C:/Some/location/here")  # For PC (Note the forward slashes!)
# setwd("~/Some/location/here")  # For Mac OSX 

##' Load the data
file.choose()  # Find the file in the working directory

## Use 'with' to avoid retyping the data.frame
with(dataset, summary(variable))

## Using factors
## Income
levels(df$Q20)
df$inc <- df$Q20
df$inc[df$inc == "Don't know / Prefer not to answer"] <- NA
df$income <- 
    factor(df$inc,
           levels=c("Less than 10,000 GBP", "10,000 to 19,999 GBP", "20,000 to 29,999 GBP",
                    "30,000 to 39,999 GBP", "40,000 to 49,999 GBP", "50,000 to 59,999 GBP",
                    "60,000 to 69,999 GBP", "70,000 to 99,999 GBP", "100,000 to 149,999 GBP", 
                    "150,000 GBP and over"),
           ordered = TRUE)
levels(df$income)

## UK NUTS 1 Regions (12 total; 9 in England, plus Wales, Scotland, and Northern Ireland)
df$region <- "999"
df$region[df$Q18 %in% c("Berkshire", "Buckinghamshire", "East Sussex", "Hampshire", 
                        "Isle of Wight", "Kent", "Oxfordshire", "Surrey", "West Sussex", "Middlesex")] <- "South East"

df$region[df$Q18 %in% c("London")] <- "London"

df$region[df$Q18 %in% c("Cheshire", "Cumbria", "Greater Manchester", "Lancashire", "Merseyside")] <- "North West"

df$region[df$Q18 %in% c("East Anglia", "Bedfordshire", "Cambridgeshire", "Essex",
                        "Hertfordshire", "Norfolk", "Suffolk")] <- "East of England"

df$region[df$Q18 %in% c("Herefordshire", "Shropshire", "West Midlands", 
                        "Staffordshire", "Warwickshire", "Worcestershire")] <- "West Midlands"

df$region[df$Q18 %in% c("Gloucestershire", "Bristol", "Wiltshire", "Somerset", 
                        "Dorset", "Devon", "Cornwall", "Isles of Scilly", "Avon")] <- "South West"

df$region[df$Q18 %in% c("South Yorkshire", "West Yorkshire", "North Yorkshire", "East Riding of Yorkshire",
                        "Northern Lincolnshire", "North East Lincolnshire", "Cleveland", "Humberside")] <- "Yorkshire and the Humber"

df$region[df$Q18 %in% c("Derbyshire", "Leicestershire", "Lincolnshire", "Northamptonshire", 
                        "Nottinghamshire", "Rutland")] <- "East Midlands"

df$region[df$Q18 %in% c("Northumberland", "Durham", "Tyne and Wear", "Tees Valley")] <- "North East"

df$region[df$Q18 %in% c("--- SCOTLAND ---", "Aberdeen City", "Aberdeenshire", "Angus", "Argyll and Bute", "Banffshire",
                        "Borders", "Clackmannan", "Dumfries and Galloway", "East Ayrshire", "East Dunbartonshire",
                        "East Lothian", "East Renfrewshire", "Edinburgh City", "Falkirk", "Fife",
                        "Glasgow (City of)", "Highland", "Inverclyde", "Midlothian", "Moray",
                        "North Ayrshire", "North Lanarkshire", "Orkney", "Perthshire and Kinross",
                        "Renfrewshire", "Roxburghshire", "Shetland", "South Ayrshire", "South Lanarkshire",
                        "Stirling", "West Dunbartonshire", "West Lothian", "Western Isles")] <- "Scotland"

df$region[df$Q18 %in% c("--- WALES ---", "Blaenau Gwent", "Bridgend", "Caerphilly", "Cardiff", "Carmarthenshire",
                        "Ceredigion", "Conwy", "Denbighshire", "Flintshire", "Gwynedd", "Isle of Anglesey",
                        "Merthyr Tydfil", "Monmouthshire", "Neath Port Talbot", "Newport", "Pembrokeshire",
                        "Powys", "Rhondda Cynon Taff", "Swansea", "Torfaen", "The Vale of Glamorgan",
                        "Wrexham")] <- "Wales"

df$region[df$Q18 %in% c("--- NORTHERN IRELAND ---", "Antrim", "Armagh", "Down", 
                        "Fermanagh", "Londonderry", "Tyrone")] <- "Northern Ireland"

df$region[df$region == "999"] <- "Other"
table(df$Q18[df$region=="Other"])
table(df$region)
