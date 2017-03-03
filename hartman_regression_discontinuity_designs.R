##' ---
##' title: "Regression Discontinuity Designs (RDDs)"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' Housekeeping
## Get and set the working directory
getwd()
setwd("/R/")

##' Load the MPs data
mp <- read.csv("MPs.csv")

## Check the data
head(mp)

## Subset the data by major party
mp.labour <- subset(mp, subset = (party == "labour"))
mp.tory <- subset(mp, subset = (party == "tory"))

##' Data analysis - Tories
## Regressions for Tories - negative and positive margin
tory.fit1 <- lm(ln.net ~ margin, data = mp.tory[mp.tory$margin < 0, ])
tory.fit2 <- lm(ln.net ~ margin, data = mp.tory[mp.tory$margin > 0, ])

summary(tory.fit1)
exp(tory.fit1$coef[1])
summary(tory.fit2)
exp(tory.fit2$coef[1])


## Tories - range of predictions
y1t.range <- c(min(mp.tory$margin), 0)  # Min to 0
y2t.range <- c(0, max(mp.tory$margin))  # 0 to max

## Prediction
y1.tory <- predict(tory.fit1, 
                   newdata = data.frame(margin = y1t.range))
y2.tory <- predict(tory.fit2, 
                   newdata = data.frame(margin = y2t.range))

## Create scatterplot with regression lines for Tories
plot(mp.tory$margin, mp.tory$ln.net, main = "Conservatives", 
     xlim = c(-0.5, 0.5), ylim = c(6, 18), xlab = "Margin of election victory",
     ylab = "Net wealth of MP (at death, logged)",
     col = "blue")
abline(v = 0, lty = "dashed", lwd = 2)
lines(y1t.range, y1.tory, col = "black", lwd = 3)  # Add regression line 1
lines(y2t.range, y2.tory, col = "black", lwd = 3)  # Add regression line 2

## Calculate average net wealth for Tory MP
tory.mp <- exp(y2.tory[1])

## Average net wealth for Tory non-MP
tory.nonmp <- exp(y1.tory[2])

## Causal effect (difference in average net wealth, in pounds)
tory.mp - tory.nonmp

##' Data analysis - Labour
## Regressions for Labour - negative vs positive margin
labour.fit1 <- lm(ln.net ~ margin, data = mp.labour[mp.labour$margin < 0, ])
labour.fit2 <- lm(ln.net ~ margin, data = mp.labour[mp.labour$margin > 0, ])

## Labour - range of predictions
y1l.range <- c(min(mp.labour$margin), 0)  # Min to 0
y2l.range <- c(0, max(mp.labour$margin))  # 0 to max

## Prediction
y1.labour <- predict(labour.fit1, 
                     newdata = data.frame(margin = y1l.range))
y2.labour <- predict(labour.fit2, 
                     newdata = data.frame(margin = y2l.range))

## Create scatterplot with regression lines for labour
plot(mp.labour$margin, mp.labour$ln.net, main = "Labour",
     xlim = c(-0.5, 0.5), ylim = c(6, 18), xlab = "Margin of election victory",
     ylab = "Net wealth of MP (at death, logged)", 
     col = "red")
abline(v = 0, lty = "dashed", lwd = 2)
lines(y1l.range, y1.labour, col = "black", lwd = 3)  # Add regression line 1
lines(y2l.range, y2.labour, col = "black", lwd = 3)  # Add regression line 2

## Calculate average net wealth for Tory MP
labour.mp <- exp(y2.labour[1])

## Average net wealth for Tory non-MP
labour.nonmp <- exp(y1.labour[2])

## Causal effect (difference in average net wealth, in pounds)
labour.mp - labour.nonmp

##' Placebo test
## Regressions for Tories - negative and positive margin at
tory.fit3 <- lm(margin.pre ~ margin, data = mp.tory[mp.tory$margin < 0, ])
tory.fit4 <- lm(margin.pre ~ margin, data = mp.tory[mp.tory$margin > 0, ])

## Difference between two intercepts is the estimated effect 
coef(tory.fit4)[1] - coef(tory.fit3)[1]

##' Using the 'rdd' package
pacman::p_load(rdd)
tory.rdd.fit <- RDestimate(ln.net ~ margin, data = mp.tory)
summary(tory.rdd.fit)
plot(tory.rdd.fit)
abline(v = 0, lty = "dashed", lwd = 2)
