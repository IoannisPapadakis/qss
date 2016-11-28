##' ---
##' title: "What is the conditional probability of having HIV if you test positive?"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Background information
## OraQuick Advance Rapid HIV-1/2
## A Centers for Disease Control study with 12,000 participants found:
## Test Sensitivity: 99.1% of HIV cases detected (true positive)                       [Pr(Positive|HIV)]
## thus, test misses .9% of real HIV cases (false negative)                            [Pr(Negative|HIV)]
##
## Test Specificity: 99.6% of healthy people test negative (true negative)             [Pr(Negative|No HIV)]
## thus, the test incorrectly returns a positive result in .4% cases (false positive)  [Pr(Positive|No HIV)]
##
## Prevalence rates for adults (aged 18-49) in 2014; CIA World Factbook
## weblink: https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_2155.txt
## http://aidsinfo.unaids.org/
## Prevalence of HIV in the UK: 3.3 cases per 1,000

##' Housekeeping
## Load packages via Pacman
pacman::p_load(ggplot2, gridExtra)

## Set the working directory
# setwd("ENTER YOUR DIRECTORY PATH HERE")

##' UK Only: Conditional probability of HIV given a positive test
## Bayes' Rule: Pr(A|B) = Pr(B|A)Pr(A) / [Pr(B|A)Pr(A) + Pr(B|A')Pr(A')] 
## Pr(HIV|positive) = Pr(Positive|HIV)Pr(HIV) / [Pr(Positive|HIV)Pr(HIV) + Pr(Positive|No HIV)Pr(No HIV)]
pr.hiv.uk <- .991*.0028 / ((.991*.0028)+(.004*.9972))

##' World: Conditional probability of HIV given a positive test
## Pull the data from the CIA World Factbook website (.txt file with fixed widths)
hiv <- read.fwf("https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_2155.txt",
                      width = c(7,51,4))  
dimnames(hiv)[[2]] <- c("id", "country", "percent")  # Add variables names
head(hiv)

## Use Bayes' Rule to calculate the conditional probabilities
hiv$prop<- hiv$percent/100  # Transform percentages into proportions
hiv$pr.hiv <- (.991*hiv$prop)/((.991*hiv$prop)+(.004*(1-hiv$prop)))
hiv <- hiv[order(-hiv$pr.hiv, hiv$country), ]
subset(hiv, select = c(country, pr.hiv))  # Display by country

## Create the figure
hiv$country2 <- factor(hiv$country, levels = hiv[order(hiv$pr.hiv), "country"])
median(hiv$pr.hiv)
hiv2 <- subset(hiv, hiv$pr.hiv<median(hiv$pr.hiv))
hiv3 <- subset(hiv, hiv$pr.hiv>=median(hiv$pr.hiv))

x <- ggplot(hiv2, aes(y = country2, x = pr.hiv)) +
     geom_point(stat = "identity") +
     xlab("Probability") + 
     ylab("Country")
y <- ggplot(hiv3, aes(y = country2, x = pr.hiv)) +
     geom_point(stat = "identity") +
     xlab("Probability") +
     ylab("")
grid.arrange(x, y, ncol=2)
