---
output: html_document
---
# **US Storm Data Analysis for Casualties and Economic Cost**

### _by Raphael Kurlansik_
### _6/18/2015_

**Synopsis:**

This analysis will utilize the National Weather Service storm data documentation to answer the following questions: 

1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health? 
2. Across the United States, which types of events have the greatest economic consequences?

The approach taken was to find the values for casualties and damages that constituted the top 10% of each category.  This subset will give a snapshot of which storm events have had the greatest impact on human health and economic costs.  Results of this analysis indicate that floods and high speed winds had the greatest impact on both human and economic costs.

Data processing and results were achieved using RStudio (Version 0.98.1103).  The original data can be found [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2), and the associated documentation [here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) and [here](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).

### Data Processing
_________________________________________________________________________________________________

Load the required libraries and read data into R.


```r
library(dplyr)
library(ggplot2)
library(gridExtra)
raw.storm <- read.csv("repdata-data-StormData.csv", 
                      header = T, 
                      na.strings = "")
```

To start with, select only those columns relevant to the inquiries.  We won't need start/end times, or remarks about a particular storm.  Clean the data by changing the variable classes to make them easier to work with, and remove any missing values.


```r
tidy.storm <- raw.storm[, c(2,8,23:28)]  ## Select relevant columns
tidy.storm$BGN_DATE <- as.Date(tidy.storm$BGN_DATE, format = "%m/%d/%Y")  ## Change classes
tidy.storm$PROPDMGEXP <- as.character(tidy.storm$PROPDMGEXP)
tidy.storm$CROPDMGEXP <- as.character(tidy.storm$CROPDMGEXP)
tidy.storm <- na.omit(tidy.storm)
head(tidy.storm)
```

```
##          BGN_DATE                    EVTYPE FATALITIES INJURIES PROPDMG
## 187566 1995-10-04 HURRICANE OPAL/HIGH WINDS          2        0     0.1
## 187571 1994-06-26        THUNDERSTORM WINDS          0        0     5.0
## 187581 1995-08-03            HURRICANE ERIN          0        0    25.0
## 187583 1995-10-03            HURRICANE OPAL          0        0    48.0
## 187584 1995-10-04            HURRICANE OPAL          0        0    20.0
## 187653 1994-03-24        THUNDERSTORM WINDS          0        0    50.0
##        PROPDMGEXP CROPDMG CROPDMGEXP
## 187566          B      10          M
## 187571          M     500          K
## 187581          M       1          M
## 187583          M       4          M
## 187584          m      10          m
## 187653          K      50          K
```

### Impact on Human Health
_______________________________________________________________________________
To judge which storm events have the greatest impact on human health, we will find the group of events responsible for the greatest casualties (injuries & fatalities).  

Beginning with the `tidy.storm` data set:

1. First, subset the rows whose `FATALITIES` and `INJURIES` values are greater than zero.  Store this subset in a new dataframe, `casualties.df`.
2. Drop variables related to property and crop damage.
3. Create a new variable, `Casualties` which is the sum of `FATALITIES` and `INJURIES`.
4. Group the data by `EVTYPE`, summarize for the sum of `Casualties`, and arrange in descending order.
5. Look at events at constitute the top 10% of casualties.


```r
## Subset for non-zero values
casualties.df <- tidy.storm[which(tidy.storm$FATALITIES > 0 | tidy.storm$INJURIES > 0),]
casualties.df <- casualties.df[,c(1,2,3,4)]  ## Drop columns
casualties.df <- mutate(casualties.df, Casualties = FATALITIES + INJURIES)  ## New variable
casualties.df <- group_by(casualties.df, EVTYPE)  ## Group by event type
casualties.df <- summarise(casualties.df, sum(Casualties)) ## Sum casualties by type
colnames(casualties.df)[2] <- "Casualties"
casualties.df <- arrange(casualties.df, desc(Casualties))  ## Arrange, descending
quantile(casualties.df$Casualties, probs = 0.90)  ## Find top 10th percentile
```

```
##    90% 
## 1066.8
```

```r
casualties.df <- casualties.df[casualties.df$Casualties >= 1066.8,]
print(casualties.df)
```

```
## Source: local data frame [7 x 2]
## 
##              EVTYPE Casualties
## 1           TORNADO      13024
## 2             FLOOD       6756
## 3         ICE STORM       1629
## 4 THUNDERSTORM WIND       1542
## 5              HEAT       1376
## 6         LIGHTNING       1183
## 7    EXCESSIVE HEAT       1070
```

### Impact on Economic Conditions
_______________________________________________________________________________

To investigate the economic impact of storms, work toward subsetting the group of events responsible for greatest damages.

Starting from `tidy.storm`:

1. First, subset the rows whose `PROPDMG` and `CROPDMG` values are greater than zero.  Store this subset in a new dataframe, `damages.df`.
2. Use exponent values (i.e., `PROPDMGEXP`) to convert all damage to total dollars.  
3. Drop the exponent and casualty variables. 
4. Create a new variable, `total.damages`, which is the sum of crop and property damage for each event.
5. Group the data by `EVTYPE`, summarize for the sum of damages, and arrange in descending order.
6. Look at the events that constitute the top 10% of damages.


```r
## Subset for non-zero values
damages.df <- tidy.storm[which(tidy.storm$PROPDMG > 0 | tidy.storm$CROPDMG > 0),] 

for (i in 1:nrow(damages.df)){      ## Convert exponents to absolute dollar value.
    if(damages.df[i, 6] == "K"){
        damages.df$PROPDMG[i] <- damages.df$PROPDMG[i] * 10**3
    }else if(damages.df[i, 6] == "M"){
        damages.df$PROPDMG[i] <- damages.df$PROPDMG[i] * 10**6
    }else if(damages.df[i, 6] == "B"){
        damages.df$PROPDMG[i] <- damages.df$PROPDMG[i] * 10**9
    }
}

for (i in 1:nrow(damages.df)){    
    if(damages.df[i, 8] == "K"){
        damages.df$CROPDMG[i] <- damages.df$CROPDMG[i] * 10**3
    }else if(damages.df[i, 8] == "M"){
        damages.df$CROPDMG[i] <- damages.df$CROPDMG[i] * 10**6
    }else if(damages.df[i, 8] == "B"){
        damages.df$CROPDMG[i] <- damages.df$CROPDMG[i] * 10**9
    }
    
}

damages.df <- damages.df[,c(1,2,5,7)]  ## Drop columns
damages.df <- mutate(damages.df, total.damages = PROPDMG + CROPDMG)  ## Total damages variable
damages.df <- group_by(damages.df, EVTYPE)  ## Group by event type
damages.df <- summarise(damages.df, sum(total.damages)) ## Sum total damages by type
colnames(damages.df)[2] <- "total.damages" 
damages.df <- arrange(damages.df, desc(total.damages))  ## Arrange, descending
quantile(damages.df$total.damages, probs = 0.90) ## Find the 10th percentile
```

```
##        90% 
## 2129941727
```

```r
damages.df <- damages.df[damages.df$total.damages >= 2129941727,]
print(damages.df)
```

```
## Source: local data frame [13 x 2]
## 
##               EVTYPE total.damages
## 1              FLOOD  138007444500
## 2  HURRICANE/TYPHOON   29348167800
## 3            TORNADO   16570326363
## 4          HURRICANE   12405268000
## 5        RIVER FLOOD   10108369000
## 6               HAIL   10045180037
## 7        FLASH FLOOD    8715885183
## 8          ICE STORM    5925150850
## 9   STORM SURGE/TIDE    4641493000
## 10 THUNDERSTORM WIND    3813647990
## 11          WILDFIRE    3684468370
## 12         HIGH WIND    3057666640
## 13    HURRICANE OPAL    2157000030
```

### Results
_____________________________________________________________________________________________
**Question 1:**

A visualization of the data shows that tornados have the greatest impact on human health, followed by floods.


```r
cas.plot <- ggplot(data = casualties.df, aes(y=Casualties, x=EVTYPE, fill = EVTYPE)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_text(angle = 70, size = 10, vjust = 0.5), 
            plot.title = element_text(size = 18, face="bold", vjust = 2)) +
      labs(title = "Top 10% of Casualty-inducing\n Storm Events in USA, 1950-2011") +
      labs(x = "") +
      labs(y = "Total Casualties (Injuries & Fatalities)")
print(cas.plot)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

When calculating the 'impact on human health', a fatality is far worse than an injury.  This was not factored into the analysis, and is not represented in the results.  However, this data can still be useful because it gives a picture of the overall impact.  

The data shows that tornados - events that are difficult to predict - are a major concern. Conspicuously absent are major events like hurricanes.  The reason for this is unclear, but perhaps governments and scientists can sufficiently track those events to warn the public in advance, yet lack the technology to do so with tornados.  The same may be said for flooding.  

**Question 2:**

A visualization of this data confirms that flood damage is the worst, by far.  In general, high winds and flooding conditions caused the greatest damage.  Seeing hail in the top 10% was a surprise.


```r
dmgs.plot <- ggplot(data = damages.df, aes(y=total.damages, x=EVTYPE, fill = EVTYPE)) +
      geom_bar(stat="identity") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.title = element_text(size = 18, face="bold", vjust = 2)) +
      labs(title = "Top 10% of Damaging Storm Events\n in USA, 1950-2011") +
      labs(x = "") +
      labs(y = "Total Damages ($USD)")

print(dmgs.plot)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

When analyzing the data, it occured to me to group the flood event types together, or the hurricane events together.  One major issue with that method is that it may not always be reasonable to group two events that happen to have the same word in them. Are flash floods the same as river floods?  Is disaster planning or are the risk factors for each the same?  To answer these questions for every unique event is beyond the scope of this investigation.  Rather, these results tell a story that is consisent with the findings of question 1:  Generally speaking, flood and heavy wind events cause the most destruction!  Further analysis is required to generate more specific results.

_It is recommended based on these analyses that businesses and municipalities investigate where they may be structurally vulnerable to flooding or high winds._
<br> <br> <br> <br>

