##########################################################################################################
# CDC Proportional Estimator Drawing
# Zhihang Dong
# Version 0.5
# 3/6/2019
##########################################################################################################

# Import just one library
library(dplyr)

# ====================================================================================================================
# @Function 1: rankProp gives you a smaller dataset grouped by the coordinate of the data points 
# rounded to the closest 0.5 increment. For example, a datapoint P(36.229, 39.581) will be giving you
# (36.0, 39.5). These grouped data points naturally into several groups. We then calculate the proportion of
# time spent in each group as the percentage of the entire length of observation
#
# @Function 2: getFinalLvSet gives you the coordinate pair groups selected by the given levelSet
# Warning: when one group does not fulfill the levelSet requirement, the function will automatically append
# the second largest group, until the levelSet minimum has been fulfilled. 
# Warning 2: levelSet in our definition has been inflated 100 times. For example, a lambda of 0.1 should be written 
# as "10".
#
# @Function 3: propLvSet calculates our proportional estimator, which gives the first week, following when we have never 
# observed any differences of the coordinate group produced by the getFinalLvSet algorithms.
#
# @Function 4: drawLevelSets gives you the line plot of our PropEstimator given a step size,
# which, by essence, is an increment value (5 or 10 recommended)
# There are two types of visualization to choose from: 
# 1) Color Segments, which colors represent the amount of anchorLocations we have so far in our resulted dataset
# red = 1; orange = 2; green = 3-4; blue = 5-+inf
# 2) Annotaton of ANCH LOC numbers: a black text will appear above each step size \eta.
# =====================================================================================================================

rankProp <- function(dat){
  dat$long = round(dat$long*2, digit = 1)/2
  dat$lat = round(dat$lat*2, digit = 1)/2
  proptab<-dat %>%
    group_by(long, lat) %>%
    summarise(a_sum=sum(t.delta))
  proptab$prop_per = round((100*proptab$a_sum/sum(proptab$a_sum)),digits = 1)
  proptab = proptab[-c(3)]
  proptab = proptab[order(-proptab$prop_per),]
}

getFinalLvSet <- function(tab, levelSet){
  proptab = rankProp(tab)
  for (k in 1:nrow(proptab)){
    topdat = head(proptab, k)
    if (sum(topdat$prop_per)>levelSet){
      break
    }
  }
  topdat = topdat[-c(3)]
  topdat
}

# topdat = getFinalLvSet(tab, 20)

propLvSet <- function(dat, lvSet){
  topdat = getFinalLvSet(dat, levelSet = lvSet)
  for (i in max(dat$week):1){
    newdat = dat[which(dat$week <= i), ]
    newtopdat = getFinalLvSet(newdat,levelSet = lvSet)
    if (i == 1){
      return(i)
      break
    }
    if (identical(topdat,newtopdat) == FALSE){
      return(i)
      break
    }
  }
}

# resulted = propLvSet(dat,20)
# resulted

drawLevelSets <- function(dat, incre, anchorCount = TRUE, coloredLines = TRUE,
                          annotateLocation = FALSE){
  finalResults = c()
  bins = c()
  countAnchorLocations = c()
  for (j in seq(10, 100, incre)){
    results = propLvSet(dat, j)
    finalResults = append(finalResults, results)
    bins = append(bins, j)
    if(anchorCount == TRUE){
      locCounts = nrow(getFinalLvSet(dat, j))
      countAnchorLocations = append(countAnchorLocations, locCounts)
    }
  }
  if(dat$gen[1] == 1){
    cala = 16
  } else if(dat$gen[1] == 2){
    cala = 17
  } else{
    cala = 18
  }
  if(annotateLocation == TRUE){
    plot(finalResults~bins, bty = "l", type ="b", pch=cala, cex = 0.9, col="black", 
         ylim=c(0,max(dat$week)*1.3), xlim=c(0,100), ylab="Estimator Results", xlab="LevelSets")
    text(bins, finalResults, countAnchorLocations,
         cex=0.9, pos=3,col="black") 
  } else if(coloredLines == TRUE){
    lincol = c()
    for(m in 1:length(countAnchorLocations)){
      if(countAnchorLocations[m] == 1){
        lincol = append(lincol, "red")
      } else if(countAnchorLocations[m] == 2){
        lincol = append(lincol, "orange")
      } else if(countAnchorLocations[m] <= 4){
        lincol = append(lincol, "green")
      } else if(countAnchorLocations[m] > 4){
        lincol = append(lincol, "blue")
      }
    }
    plot(finalResults~bins, type ="b", pch=cala, cex = 0.9, col=lincol, 
         ylim=c(0,max(dat$week)*1.3), xlim=c(0,100), ylab="Estimator Results", xlab="LevelSets")
    segments(bins[-length(bins)],finalResults[-length(finalResults)],bins[-1L],finalResults[-1L],
             col=lincol, lwd = 3)
  }
  
  abline(h=max(dat$week), col="red")
  text(x=85, y=max(dat$week + 3), labels="Max Length", col="blue")
}

# Experimental Section
# dat = person_5976
# drawLevelSets(dat,10, anchorCount = TRUE, coloredLines = TRUE, annotateLocation = FALSE)
# Voila!

