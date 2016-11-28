Probability and the Enigma Machine
================
November 2016

Housekeeping

``` r
## Load packages via pacman
pacman::p_load(rmarkdown)

## Set the working directory
# setwd("ENTER YOUR DIRECTORY PATH HERE")

## Turn off scientific notation
options(scipen = 999)
```

Background Information

``` r
## The Enigma Machine
## The machine has 5 different rotors, each of which comes 
## with 10 pins (0 to 9). The plugboard contains 26 holes, 
## corresponding to the 26 letters of the alphabet. There are 
## 13 cables to connect all possible pairs of letters.
## To either de/encode a message, provide the
## Enigma machine with a correct 5-digit passcode 
## to align the rotors AND a correct configuration of the plugboard.
## 
## k permutations of n is denoted as nPk, which equals n!/(n - k)!
## k combinations of n is denoted as nCk, which equals n!/k!(n - k)!
```

How many different 5 digit passcodes can be set on the 5 rotors (0-9)?

``` r
## There are 5 dials each with 10 digits, so we start with 0-0-0-0-0, 0-0-0-0-1, 
## 0-0-0-0-2...until we get to 9-9-9-9-9. So, we have 10*10*10*10*10. or 
total.rotors <- 10^5
total.rotors
```

    ## [1] 100000

How many possible combinations does the plugboard provide?

``` r
## The trick to solving this problem is to think about sequences of pairs
## There are 26C2 to choose the first pair; 24C2 for the second pair; and so on.
## This sequential selection process continues until all 13 pairs are selected.
n <- seq(from = 26, to = 2, by = -2)  # Countdown from 26 by 2s 
## Use the function for combinations, choose(n, k), 
## to calculate sequetial combinations for the plugboard
nCk <- choose(n, 2)
## Total number of combinations: 26C2 * 24C2 * 22C2 ... 2C2
total.combinations <- prod(nCk)
total.combinations
```

    ## [1] 49229914688306353800428

``` r
## However, sequential pairs will contain duplicates not allowed by the machine.
## e.g., for ABCD, there are 6 possible combinations (4C2): AB-CD, AC-BD, AD-BC, 
## BC-AD, BD-AC, CD-AB. But that allows the identical pairs AB-CD AND CD-AB.
## So, we need to divide the total number of combinations by the 
## total number of ways we can arrange 2 pairs, or 2P2 = 2! 
## Scaling this up for the full plugboard, the denominator is 13P13 = 13!
total.plugboard <- total.combinations/factorial(13)
total.plugboard
```

    ## [1] 7905853580625

What is the total number of possible settings for the Enigma machine?

``` r
## For every 5-digit passcode, we have the total calculated for the plugboard, so
total.enigma <- total.rotors * total.plugboard
total.enigma
```

    ## [1] 790585358062499968

If 1 message is selected at random, which machine is most likely responsible for a mistaken decoded message?

``` r
## There are 1,500 encrypted messages, and the failure rate:
## 1. Banburismus 300 messages, 10% failure rate
## 2. Bombe 400, 5%
## 3. Herivel tip 250, 15%
## 4. Crib 340, 17%
## 5. Hut 6 210, 20%
## 
## In practice, German procedural flaws (rather than crytographic weaknesses) 
## and capturing the hardware enabled Allied forces to break the code in WWII. 
## This is akin to the probability problem of drawing a red marble  
## from a sack of 3 red marbles and 7 white marbles
messages <- c(300, 400, 250, 340, 210)  # Total number of messages decoded by each team
failure.rate <- c(.1, .05, .15, .17, .2)  # Decoding failure rate (by team)
failures <- messages*failure.rate  # Total number of failures (per team)
probability <- failures / sum(failures)  # Probability of team failure by total failures
probability
```

    ## [1] 0.1601708 0.1067806 0.2002136 0.3085958 0.2242392

Write a function that randomly configures the plugboard.

``` r
## Output should be a 2 x 13 matrix where each column represents a pair of letters.
plugboard <- function() {
  draw <- sample(letters)
  pairs <- matrix(draw, nrow = 2, ncol = 13)
  return(pairs)
}

plugboard()
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
    ## [1,] "c"  "q"  "i"  "r"  "o"  "z"  "k"  "t"  "g"  "y"   "s"   "b"   "u"  
    ## [2,] "j"  "f"  "p"  "a"  "m"  "e"  "x"  "w"  "n"  "l"   "v"   "d"   "h"

``` r
config <- plugboard()
```

Write a function that encodes/decodes a message given by plugboard().

``` r
## Input should be result of plugboard() and message; output should by decoded message.
enigma <- function(x, plug.config) {
  ## Lower cases indicate letters that need to be switched
  ## Upper cases indicate letters that have been changed
  x.encoded <- tolower(x)
  plug.config.up <- toupper(plug.config)
  ## Switch letters
  for(i in 1:13) {
    x.encoded <- gsub(plug.config[1, i], plug.config.up[2, i],
                      x.encoded)
    x.encoded <- gsub(plug.config[2, i], plug.config.up[1, i],
                      x.encoded)
  }
  ## Change it back to lower cases
  x.encoded <- tolower(x.encoded)
  return(x.encoded)
}

## Encode a message using the new function
text <- "now we can win the war"
text.encoded <- enigma(text, p.config)
text.encoded
```

    ## [1] "dxu uy bmd uqd hty ump"

``` r
## Decode the message using the new function
enigma(text.encoded, p.config)
```

    ## [1] "now we can win the war"
