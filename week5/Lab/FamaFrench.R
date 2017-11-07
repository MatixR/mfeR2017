## MFE Programming Workshop Week 5 Lab Solution
## Robert Richmond
library(lubridate)
library(plyr)
library(reshape2)
library(ggplot2)

## read in the returns to the 25 fama-french portfolios
ffports <- read.csv("FFports.csv")

## clean up the dates
ffports$date <- ymd(paste(ffports$date,"01",sep=""))
## convert the date to an end of the month observation
ffports$EOM <- ffports$date + months(1) - days(1)
ffports$date <- NULL

## convert to long form
ffports <- melt(ffports,id.vars="EOM")
names(ffports) <- c("EOM","portfolio","ret")

## read in the risk factors
## and clean up the dates
ff <- read.csv("FFfactors.csv")
ff$Date <- ymd(paste(ff$Date,"01",sep=""))
ff$EOM <- ff$Date + months(1) - days(1)
ff$Date <- NULL

## merge everything together
all <- merge(ffports,ff,all.x=TRUE,by="EOM")

## calculate excess returns
all$excess <- all$ret - all$RF

## get a subset
start <- "1963-01-01"
end <- "2013-12-31"
subs <- all[all$EOM <= ymd(end) & all$EOM >= ymd(start),]

## function to get the data.frame of factor loadings
## and of mean excess returns
getFactorloadings <- function(factorlist) {

    ## first get beta estimates for each asset
    ## this constructs the forumala from a strings
    formulats <- as.formula(paste0("excess ~ ",paste(factorlist,collapse="+")))
    tsbeta <- ddply(subs,.(portfolio),
                    function(x) coef(lm(formulats,data=x)))

    ## now run time series regressions to estimate the factor loadings and pricing errors
    meanexcess <- ddply(subs,.(portfolio),
                        function(x) data.frame(meanexcess=mean(x$excess)))

    ## return the merged betas and mean excess returns
    merge(tsbeta,meanexcess)
}

## this is the list of factors that are in subs now that we want to find
## prices of risk for
factorlists <- list(c("Mkt.RF"),c("Mkt.RF","SMB","HML"))

## get the loadings for each list of factors
loadings <- lapply(factorlists,getFactorloadings)

## I'm going to use ggplot, but you could obviously just plot the
## two data series with plot. You may need to install ggplot2
## for this to work
ggplot(loadings[[1]]) + geom_text(aes(Mkt.RF,meanexcess,label=portfolio))

## now I'm going to look at the second part
factorsub <- ff[,factorlists[[2]]]
ffmeans <- data.frame(lapply(factorsub, mean))
names(ffmeans) <- paste0("mean",names(ffmeans))

## add the factor means onto the factor loadings data
preddf <- cbind(loadings[[2]],ffmeans)
## with is really nice so you don't have to type too many things
preddf <- transform(preddf, predval = Mkt.RF*meanMkt.RF+HML*meanHML+SMB*meanSMB)

## make the plot
ggplot(preddf) + geom_text(aes(predval,meanexcess,label=portfolio))

