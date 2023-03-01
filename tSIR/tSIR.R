library(tidyverse); library(wpp2019); library(RcppRoll); library(lubridate)

## tsiR examples

## load the package and dependencies
## kernlab is for the gaussian process 
## the rest are for plotting 
require(tsiR); require(kernlab); require(ggplot2); require(reshape2); require(grid)

# theme_set(theme_gray(base_size = 22))
## twentymeas is the included data set and is a list
## pull out cities by name 
names(twentymeas)

## London example

LondonMeas <- twentymeas[["London"]]
head(LondonMeas)
plotdata(LondonMeas)

## run the tsir model
## i.e. estimate parameters and then resimulate the model based on the fitted data
## arguments and their descriptions can be found via ?runtsir
LondonRes <- runtsir(data=LondonMeas,IP=2,
                     xreg='cumcases',regtype='gaussian',
                     alpha=NULL, sbar=NULL,
                     method='negbin',nsim=100,
                     family='gaussian',link='identity')

## plot the diagnostics, estimates, and forward simulations
## first two plots are the cum.cases cum.births regression and under-reporting
## next is the residuals Z, inferred Sbar, and thus resconstructed S
## next is the seasonality beta (with CI) and alpha estimates 
## the last two plots are forward simulations 
## the top forward simulation is 10 randomly chosen simulations
## the bottom is the mean (with CI)
plotres(LondonRes)

## we can also call most of the individual subplots from plotres as below
## this is useful for examining trends and confidence intervals closer

## plot the cumbirths-cumcases regression
plotregression(LondonRes)

## plot the inferred reporting rate
plotrho(LondonRes)

## plot the inferred sbar
plotsbar(LondonRes)

## plot the inferred beta
plotbeta(LondonRes)

## plot the forward simulate with CI with prediction both inverted and not
plotforward(LondonRes,inverse = F)
plotforward(LondonRes,inverse = T)

## Northwich example
NorthwichMeas <- twentymeas[["Northwich"]]

NorthwichParms <- estpars(data=NorthwichMeas,IP=2,
                          alpha=0.97, sbar=NULL,
                          regtype='loess',
                          family='poisson',link='log')

plotbreaks(data=NorthwichMeas,threshold=3)

NorthwichRes <- simulatetsir(data=NorthwichMeas,IP=2,
                             parms=NorthwichParms,
                             epidemics='break',threshold=3,
                             method='pois',nsim=100)


## the last two plots are forward simulations 
## the top forward simulation is 10 randomly chosen simulations
## the bottom is the mean (with CI)
plotcomp(NorthwichRes)

